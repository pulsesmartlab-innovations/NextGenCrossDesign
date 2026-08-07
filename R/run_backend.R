# ===========================================================================
# run_backend.R  -  assemble a run config and invoke the backend runner
# ===========================================================================

# Keep only the newest cfg$keep_runs run directories under cfg$runs_dir; delete
# older ones (by mtime). keep_runs = 0 (or NA/negative) disables pruning. Env
# overrides arrive as strings, so coerce. `protect` is one or more dirs (e.g. a
# persistent staged-pipeline run dir) that must never be removed, even if they
# would otherwise fall outside the newest-keep_runs window. Returns the number
# removed.
ngcd_prune_runs <- function(cfg, protect = NULL) {
  keep <- suppressWarnings(as.integer(cfg$keep_runs %||% 20L))
  if (is.na(keep) || keep <= 0L) return(invisible(0L))
  dirs <- list.dirs(cfg$runs_dir, recursive = FALSE)
  if (length(dirs) <= keep) return(invisible(0L))
  mt <- file.info(dirs)$mtime
  ordered <- dirs[order(mt, decreasing = TRUE)]   # newest first
  to_remove <- ordered[-seq_len(keep)]
  if (!is.null(protect) && length(protect)) {
    protect_norm <- normalizePath(protect, mustWork = FALSE)
    to_remove <- to_remove[!normalizePath(to_remove, mustWork = FALSE) %in% protect_norm]
  }
  unlink(to_remove, recursive = TRUE, force = TRUE)
  invisible(length(to_remove))
}

ngcd_new_run_dir <- function(cfg, label = NULL, protect = NULL) {
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  slug  <- if (!is.null(label) && nzchar(label)) gsub("[^A-Za-z0-9_-]+", "_", label) else "run"
  # tempfile() guarantees a unique name within the session (counter + PID), so
  # two runs in the same second do not collide. Keep the stamp+slug as a
  # human-readable prefix.
  dir <- tempfile(pattern = paste0(stamp, "_", slug, "_"), tmpdir = cfg$runs_dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # Bound disk; the just-created dir is newest so never pruned. `protect` shields
  # additional dirs from this prune -- notably an active staged-pipeline run dir,
  # so a poly/subgenome do_run() (which mints its own dir here) cannot evict the
  # pipeline dir the user is mid-way through.
  ngcd_prune_runs(cfg, protect = protect)
  dir
}

# Persistent run dir for the staged pipeline (Phase 2): one subprocess call per
# stage (qc -> predict -> index -> allocate -> rank), each writing artifacts
# into the SAME dir so later stages can resume from earlier ones (compute-once).
# Unlike ngcd_new_run_dir(), this dir must survive the ngcd_prune_runs() call
# that runs right after its creation, so it is passed as `protect` here. A later
# prune triggered by a regular ngcd_new_run_dir() call elsewhere in the session
# only spares it if that call ALSO forwards this dir as `protect` -- the do_run()
# call site does exactly that (protect = rv$pipeline$run_dir).
ngcd_new_pipeline_dir <- function(cfg) {
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  dir <- tempfile(pattern = paste0(stamp, "_pipe_"), tmpdir = cfg$runs_dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ngcd_prune_runs(cfg, protect = dir)
  dir
}

ngcd_write_config <- function(params, path) {
  keep <- params[!vapply(params, function(v) {
    is.null(v) || (length(v) == 0L) ||
      (is.character(v) && length(v) == 1L && !nzchar(v))
  }, logical(1))]
  jsonlite::write_json(keep, path, auto_unbox = TRUE, null = "null", pretty = TRUE, digits = 12)
  invisible(path)
}

# Write the (possibly edited) input tables to CSV in the run dir, and point the
# params at them. This is what makes on-the-fly table edits take effect.
ngcd_materialize_inputs <- function(data, run_dir) {
  in_dir <- file.path(run_dir, "inputs")
  dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- list()
  map <- c(genotype = "genotype.csv", phenotype = "phenotype.csv",
           map = "marker_map.csv", direction = "trait_direction.csv")
  for (key in names(map)) {
    df <- data[[key]]
    if (is.null(df)) next
    p <- file.path(in_dir, map[[key]])
    utils::write.csv(df, p, row.names = FALSE)
    paths[[key]] <- p
  }
  paths
}

# Run the backend. Returns ok, result, log, command, and the run paths.
ngcd_run_backend <- function(cfg, params, data = NULL, label = NULL, progress = NULL,
                             protect = NULL) {
  rscript <- ngcd_resolve_rscript(cfg)
  if (is.na(rscript)) return(list(ok = FALSE, result = NULL,
    log = paste0("Rscript not found at '", cfg$rscript_path, "'. Fix rscript_path in config.yml."),
    command = "", run_dir = NA))

  run_dir <- ngcd_new_run_dir(cfg, label, protect = protect)
  config_path       <- file.path(run_dir, "config.json")
  result_path       <- file.path(run_dir, "result.json")
  capabilities_path <- file.path(run_dir, "backend_capabilities.json")

  # Materialize (edited) input tables and repoint the file args.
  if (!is.null(data)) {
    paths <- ngcd_materialize_inputs(data, run_dir)
    if (!is.null(paths$genotype))  params$genotype_file  <- paths$genotype
    if (!is.null(paths$phenotype)) params$phenotype_file <- paths$phenotype
    if (!is.null(paths$map))       params$map_file       <- paths$map
    if (!is.null(paths$direction)) params$direction_file <- paths$direction
  }
  if (isTRUE(params$write_outputs) || isTRUE(params$write_figures)) params$output_dir <- run_dir
  params$run_label <- label %||% "run"

  ngcd_write_config(params, config_path)
  if (!is.null(progress)) progress$set(message = "Running backend...", value = 0.4)

  env <- character(0)
  if (nzchar(cfg$package_library)) env <- c(env, paste0("R_LIBS_USER=", cfg$package_library))

  args <- c(shQuote(cfg$runner_script), shQuote(config_path),
            shQuote(result_path), shQuote(capabilities_path))
  command <- paste(shQuote(rscript), paste(args, collapse = " "))

  t0 <- Sys.time()
  log <- tryCatch(
    system2(rscript, args = args, stdout = TRUE, stderr = TRUE, env = env,
            timeout = as.numeric(cfg$run_timeout_seconds %||% 1800)),
    error = function(e) paste("Runner invocation error:", conditionMessage(e)))
  status <- attr(log, "status")
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  if (!is.null(progress)) progress$set(message = "Reading results...", value = 0.85)

  result <- NULL
  if (file.exists(result_path))
    result <- tryCatch(jsonlite::fromJSON(result_path, simplifyVector = TRUE,
      simplifyDataFrame = TRUE, simplifyMatrix = FALSE), error = function(e) NULL)

  ok <- !is.null(result) && isTRUE(result$ok)
  error_message <- NULL
  if (!ok) {
    error_message <- if (!is.null(result) && !is.null(result$error_message))
      result$error_message
    else if (length(log)) tail(log[nzchar(trimws(log))], 1)
    else "Unknown backend error."
  }

  list(ok = ok, result = result, log = paste(log, collapse = "\n"),
       exit_status = status %||% 0L, error_message = error_message,
       command = command, config_path = config_path, result_path = result_path,
       capabilities_path = if (file.exists(capabilities_path)) capabilities_path else NA,
       run_dir = run_dir, elapsed = elapsed)
}

# Drive ONE stage of the staged pipeline (qc/predict/index/allocate/rank) in a
# subprocess call against a PERSISTENT run_dir (see ngcd_new_pipeline_dir()),
# so artifacts accumulate across calls and each stage is computed once. Mirrors
# ngcd_run_backend()'s invocation mechanism (same rscript resolution, config
# write, system2 call, env/R_LIBS handling) but:
#   - reuses the passed-in run_dir instead of minting a new one;
#   - only materializes/repoints input tables when `data` is supplied (the
#     caller does this for the "qc" stage, where the backend actually reads
#     the input files; downstream stages resume from the persisted ctx and
#     ignore the file-path params entirely);
#   - writes a per-stage config (config_<stage>.json) and reads a per-stage
#     result (stage_<stage>.json) so the files from different stage calls
#     never collide inside the same run_dir.
# The wrapper (workflow="stage" branch of run_cross_prediction_json.R) writes
# ng_run_stage()'s own small return value {status, stage, files} to the result
# path for every non-terminal stage, and the FULL assembled ng_run_result.v1
# for stage="rank" -- so reading the result path back gives us `status`
# directly for qc/predict/index/allocate, and the full result for rank. A
# top-level tryCatch in the wrapper turns any exception (e.g. resuming past a
# QC blocker) into an {ok=FALSE, error=TRUE, error_message} envelope instead,
# which is how failures are detected here regardless of stage.
ngcd_run_stage <- function(cfg, stage, run_dir, params, data = NULL, progress = NULL) {
  rscript <- ngcd_resolve_rscript(cfg)
  if (is.na(rscript)) return(list(ok = FALSE, status = NA_character_, stage = stage,
    stage_json = NULL, result = NULL,
    log = paste0("Rscript not found at '", cfg$rscript_path, "'. Fix rscript_path in config.yml."),
    exit_status = NA_integer_, error_message = "Rscript not found.", run_dir = run_dir))

  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  config_path       <- file.path(run_dir, paste0("config_", stage, ".json"))
  result_path       <- file.path(run_dir, paste0("stage_", stage, ".json"))
  capabilities_path <- file.path(run_dir, "backend_capabilities.json")

  # Materialize (edited) input tables and repoint the file args -- only when
  # data is supplied (the qc stage; downstream stages never re-read inputs).
  if (!is.null(data)) {
    paths <- ngcd_materialize_inputs(data, run_dir)
    if (!is.null(paths$genotype))  params$genotype_file  <- paths$genotype
    if (!is.null(paths$phenotype)) params$phenotype_file <- paths$phenotype
    if (!is.null(paths$map))       params$map_file       <- paths$map
    if (!is.null(paths$direction)) params$direction_file <- paths$direction
  }
  params$workflow <- "stage"
  params$stage    <- stage
  params$run_dir  <- run_dir

  ngcd_write_config(params, config_path)
  if (!is.null(progress)) progress$set(message = paste0("Running stage: ", stage, "..."), value = 0.4)

  env <- character(0)
  if (nzchar(cfg$package_library %||% "")) env <- c(env, paste0("R_LIBS_USER=", cfg$package_library))

  args <- c(shQuote(cfg$runner_script), shQuote(config_path),
            shQuote(result_path), shQuote(capabilities_path))

  log <- tryCatch(
    system2(rscript, args = args, stdout = TRUE, stderr = TRUE, env = env,
            timeout = as.numeric(cfg$run_timeout_seconds %||% 1800)),
    error = function(e) paste("Runner invocation error:", conditionMessage(e)))
  exit_status <- attr(log, "status")

  if (!is.null(progress)) progress$set(message = "Reading stage results...", value = 0.85)

  raw_out <- NULL
  if (file.exists(result_path))
    raw_out <- tryCatch(jsonlite::fromJSON(result_path, simplifyVector = TRUE,
      simplifyDataFrame = TRUE, simplifyMatrix = FALSE), error = function(e) NULL)

  # Backend-written figure/gate JSON for this stage (absent for "rank", whose
  # payload is the full result written to result_path instead).
  stage_json_path <- file.path(run_dir, "artifacts", paste0(stage, ".json"))
  stage_json <- NULL
  if (file.exists(stage_json_path))
    stage_json <- tryCatch(jsonlite::fromJSON(stage_json_path, simplifyVector = TRUE,
      simplifyDataFrame = TRUE, simplifyMatrix = FALSE), error = function(e) NULL)

  ok <- !is.null(raw_out) && !isTRUE(raw_out$error)

  status <- if (!ok) NA_character_
            else if (identical(stage, "rank")) "done"
            else raw_out$status

  result <- if (identical(stage, "rank") && ok) raw_out else NULL

  error_message <- NULL
  if (!ok) {
    error_message <- if (!is.null(raw_out) && !is.null(raw_out$error_message))
      raw_out$error_message
    else if (length(log)) tail(log[nzchar(trimws(log))], 1)
    else "Unknown backend error."
  }

  list(ok = ok, status = status, stage = stage, stage_json = stage_json,
       result = result, log = paste(log, collapse = "\n"),
       exit_status = exit_status %||% 0L, error_message = error_message,
       warnings = if (!is.null(raw_out)) raw_out$warnings else NULL,
       run_dir = run_dir)
}

# Heuristic hints for common backend errors, shown in the debug panel.
ngcd_error_hint <- function(msg) {
  msg <- tolower(msg %||% "")
  if (grepl("dosage.*ploidy|ploidy.*dosage|dosage must have row names|out of range", msg))
    return("Polyploid dosages must be 0..ploidy. Set Ploidy on the Data screen to match your coding (e.g. tetraploid data uses 0..4, so Ploidy = 4).")
  if (grepl("residual-heterozygosity|assume_inbred|inbred", msg))
    return("Parents look non-inbred for a DH/RIL model. Use homozygous 0/2 dosages, or uncheck 'Assume inbred parents' on Prediction & scoring.")
  if (grepl("not found|no such file|cannot open", msg))
    return("A file path is wrong or missing. Check the four input files on the Data screen.")
  if (grepl("id|match|align", msg))
    return("Parent IDs may not align across genotype and phenotype. Check the ID columns on the Data screen.")
  if (grepl("lpsolve|mip", msg))
    return("A MIP optimizer needs the 'lpSolve' package, or pick optimizer = evolution/greedy on Mate allocation.")
  if (grepl("marker|map", msg))
    return("Marker names in the map may not match the genotype columns. Check the marker-map mapping.")
  if (grepl("could not find function|there is no package|namespace", msg))
    return("The backend package may be missing or in a different library. Check package_library in config.yml and that the backend package is installed.")
  NULL
}
