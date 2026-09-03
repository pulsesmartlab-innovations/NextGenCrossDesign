# ===========================================================================
# runs.R  -  reopen a previous run from disk.
#
# Every run writes its full result and its exact input CSVs to the run folder
# before the browser ever sees them, but the app kept results only in the Shiny
# session: a closed tab, a browser crash or a dropped websocket meant re-running
# the pipeline to get back to numbers that were already on disk. These helpers
# read a finished run back so the Results screens can be restored as they were.
#
# TWO FILENAMES, ONE MEANING
#   stage_rank.json  the staged pipeline (the normal path: QC, fit, score, rank)
#   result.json      the single-shot path
# Both hold the same payload. A run folder has one or the other, so look for
# either - checking only result.json silently misses every staged run.
#
# FIDELITY
# The parse options and the post-processing here MUST match what run_backend.R
# and the run observer do (simplifyMatrix = FALSE, then ngcd_enrich_result), or
# a restored run would differ subtly from the same run when freshly computed -
# the kind of difference that shows up much later as a puzzling discrepancy.
# ===========================================================================

# The result file for one run folder, or NA if it has none (a failed or
# still-running run).
ngcd_run_result_file <- function(run_dir) {
  for (f in c("stage_rank.json", "result.json")) {
    p <- file.path(run_dir, f)
    if (file.exists(p)) return(p)
  }
  NA_character_
}

# Parse exactly as run_backend.R does.
ngcd_run_read <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE,
                              simplifyDataFrame = TRUE, simplifyMatrix = FALSE),
           error = function(e) NULL)
}

# One row per run folder, newest first. `ok` marks the runs that can actually be
# reopened; failed and part-finished runs are listed too, so a folder full of
# attempts still explains itself rather than appearing empty.
#
# PERFORMANCE. The obvious implementation - parse every run's result to count
# its crosses - costs ~100 ms per run, so a breeder with 100 runs waits 10
# seconds every time they open this tab, and it gets worse forever. Two guards:
#
#   * a small on-disk cache keyed by the result file's mtime, so each run is
#     parsed once ever rather than once per visit;
#   * `limit`, so the listing stays bounded no matter how many runs accumulate.
#
# The cache holds only counts, never results, so a stale or deleted cache costs
# one re-parse and can never serve wrong numbers: the mtime check invalidates
# any entry whose run has changed.
ngcd_run_cache_read <- function(path) {
  if (is.null(path) || !file.exists(path)) return(list())
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) list())
}
ngcd_run_cache_write <- function(path, cache) {
  if (is.null(path)) return(invisible(FALSE))
  tryCatch({
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(jsonlite::toJSON(cache, auto_unbox = TRUE, null = "null"), path)
    TRUE
  }, error = function(e) FALSE)   # a read-only data dir must not break the tab
}

ngcd_run_index <- function(runs_dir, cache_file = NULL, limit = 100L) {
  empty <- data.frame(run = character(0), when = as.POSIXct(character(0)),
                      ok = logical(0), crosses = integer(0), candidates = integer(0),
                      traits = character(0), path = character(0),
                      stringsAsFactors = FALSE)
  if (!length(runs_dir) || is.na(runs_dir) || !dir.exists(runs_dir)) return(empty)
  dirs <- list.dirs(runs_dir, recursive = FALSE, full.names = TRUE)
  if (!length(dirs)) return(empty)
  # newest first, then cap: never touch more folders than we will show
  dirs <- dirs[order(file.mtime(dirs), decreasing = TRUE)]
  if (is.finite(limit) && length(dirs) > limit) dirs <- dirs[seq_len(limit)]

  cache <- ngcd_run_cache_read(cache_file); dirty <- FALSE
  rows <- lapply(dirs, function(d) {
    run <- basename(d)
    p <- ngcd_run_result_file(d)
    mt <- if (!is.na(p)) file.mtime(p) else file.mtime(d)
    hit <- cache[[run]]
    if (!is.null(hit) && identical(as.character(hit$mtime), as.character(mt))) {
      return(data.frame(run = run, when = mt, ok = isTRUE(hit$ok),
                        crosses = hit$crosses %||% NA_integer_,
                        candidates = hit$candidates %||% NA_integer_,
                        traits = hit$traits %||% NA_character_,
                        path = if (is.na(p)) NA_character_ else p,
                        stringsAsFactors = FALSE))
    }
    r <- ngcd_run_read(p)
    ok <- !is.null(r) && !isTRUE(r$error) &&
      is.data.frame(r$selected_crosses) && nrow(r$selected_crosses) > 0
    rec <- list(mtime = as.character(mt), ok = ok,
                crosses    = if (ok) nrow(r$selected_crosses) else NA_integer_,
                candidates = if (ok && is.data.frame(r$candidate_crosses)) nrow(r$candidate_crosses) else NA_integer_,
                traits     = if (ok && is.data.frame(r$effect_summary)) paste(r$effect_summary$trait, collapse = ", ") else NA_character_)
    cache[[run]] <<- rec; dirty <<- TRUE
    data.frame(run = run, when = mt, ok = ok,
               crosses = rec$crosses %||% NA_integer_,
               candidates = rec$candidates %||% NA_integer_,
               traits = rec$traits %||% NA_character_,
               path = if (is.na(p)) NA_character_ else p,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (dirty) {
    cache <- cache[names(cache) %in% basename(dirs)]   # forget deleted runs
    ngcd_run_cache_write(cache_file, cache)
  }
  out[order(out$when, decreasing = TRUE), , drop = FALSE]
}

# Restore one run to the same shape a fresh run produces. NULL if unusable.
ngcd_run_restore <- function(path) {
  r <- ngcd_run_read(path)
  if (is.null(r) || isTRUE(r$error)) return(NULL)
  if (!is.data.frame(r$selected_crosses) || !nrow(r$selected_crosses)) return(NULL)
  ngcd_enrich_result(r)
}

# What the run was computed from. Shown so a reopened run can be traced back to
# its inputs rather than being an anonymous set of numbers.
ngcd_run_inputs <- function(run_dir) {
  d <- file.path(run_dir, "inputs")
  if (!dir.exists(d)) return(character(0))
  list.files(d, full.names = FALSE)
}

# Human label for the runs table.
ngcd_run_label <- function(idx) {
  if (!nrow(idx)) return(character(0))
  ifelse(idx$ok,
         sprintf("%s  -  %d crosses from %d candidates", idx$run, idx$crosses, idx$candidates),
         sprintf("%s  -  did not finish", idx$run))
}
