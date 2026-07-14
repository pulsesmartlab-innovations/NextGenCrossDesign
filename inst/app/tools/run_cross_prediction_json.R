#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# run_cross_prediction_json.R
#
# Headless bridge between the NDSU Cross Design Shiny front-end and the
# nextgenCrossDesign R backend.
#
#   Usage:
#     Rscript run_cross_prediction_json.R <config.json> <result.json> [capabilities.json]
#
# Reads a JSON run config (schema "ng_run_config.v1", every key mirrors an
# ng_run_cross_prediction() formal), calls the backend, and writes the result
# as JSON (schema "ng_run_result.v1") plus, optionally, the backend capability
# registry. The Shiny app never touches quantitative-genetics logic itself; it
# only assembles this config and renders the JSON that comes back.
#
# The script is deliberately defensive: unknown config keys are dropped with a
# warning (rather than erroring), so a slightly newer UI never hard-fails an
# older backend, and vice versa.
# ---------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  ok_jsonlite <- requireNamespace("jsonlite", quietly = TRUE)
}))
if (!ok_jsonlite) {
  stop("The 'jsonlite' package is required. Install it with install.packages('jsonlite').",
       call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("Usage: Rscript run_cross_prediction_json.R <config.json> <result.json> [capabilities.json]",
       call. = FALSE)
}
config_path       <- args[[1L]]
result_path       <- args[[2L]]
capabilities_path <- if (length(args) >= 3L) args[[3L]] else NULL

`%||%` <- function(a, b) if (is.null(a)) b else a

emit_error <- function(message, path) {
  payload <- list(
    schema       = "ng_run_result.v1",
    ok           = FALSE,
    error        = TRUE,
    error_message = as.character(message),
    generated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  jsonlite::write_json(payload, path, auto_unbox = TRUE, null = "null", pretty = TRUE)
  cat("ERROR:", as.character(message), "\n", file = stderr())
}

# ===========================================================================
# Polyploid mate design (ng_design_crosses_poly): dosage matrix (0..ploidy) +
# a single-trait phenotype -> QC -> additive[/dominance] effects -> scoring ->
# native allocation. Writes the same ng_run_result.v1 shape with poly_design=TRUE.
# ===========================================================================
run_polyploid_design <- function(raw, result_path) {
  if (!requireNamespace("nextgenCrossDesign", quietly = TRUE))
    stop("The 'nextgenCrossDesign' package is not installed / not on the library path.", call. = FALSE)
  suppressWarnings(suppressMessages(library(nextgenCrossDesign)))
  if (!exists("ng_design_crosses_poly", where = asNamespace("nextgenCrossDesign")))
    stop("This backend build does not provide ng_design_crosses_poly(). Reinstall a newer nextgenCrossDesign.", call. = FALSE)

  read_tab <- function(p) utils::read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
  # dosage matrix from the genotype file (first column = parent ID)
  if (is.null(raw$genotype_file) || !file.exists(raw$genotype_file))
    stop("Polyploid design needs a dosage (genotype) file.", call. = FALSE)
  gdf <- read_tab(raw$genotype_file)
  gid <- raw$genotype_id_col %||% names(gdf)[1L]
  if (!gid %in% names(gdf)) gid <- names(gdf)[1L]
  ids <- trimws(as.character(gdf[[gid]]))
  mcols <- setdiff(names(gdf), gid)
  dosage <- suppressWarnings(as.matrix(vapply(gdf[mcols], as.numeric, numeric(nrow(gdf)))))
  if (is.null(dim(dosage))) dosage <- matrix(dosage, nrow = nrow(gdf))
  rownames(dosage) <- ids; colnames(dosage) <- mcols
  storage.mode(dosage) <- "double"

  ploidy <- as.integer(raw$ploidy %||% 2L)

  # Guard the most common mistake with a clear message: dosages above the chosen
  # ploidy (the backend otherwise fails deep in QC with a misleading error).
  dmax <- suppressWarnings(max(dosage, na.rm = TRUE))
  if (is.finite(dmax) && dmax > ploidy)
    stop(sprintf("Dosage values go up to %g but the selected ploidy is %d. Set Ploidy = %g (or higher) on the Data screen so it matches your dosage coding (0..ploidy).",
                 dmax, ploidy, dmax), call. = FALSE)

  # single-trait phenotype (named vector aligned to the dosage rows)
  phenotype <- NULL
  if (!is.null(raw$phenotype_file) && file.exists(raw$phenotype_file)) {
    pdf <- read_tab(raw$phenotype_file)
    pid <- raw$phenotype_id_col %||% names(pdf)[1L]; if (!pid %in% names(pdf)) pid <- names(pdf)[1L]
    tcol <- raw$poly_trait_col
    if (is.null(tcol) || !tcol %in% names(pdf)) {
      num <- setdiff(names(pdf)[vapply(pdf, function(x) is.numeric(x) ||
        !any(is.na(suppressWarnings(as.numeric(as.character(x))))), logical(1))], pid)
      tcol <- if (length(num)) num[1L] else setdiff(names(pdf), pid)[1L]
    }
    y <- suppressWarnings(as.numeric(pdf[[tcol]]))
    names(y) <- trimws(as.character(pdf[[pid]]))
    phenotype <- y[rownames(dosage)]
  }

  # QC knobs (optional)
  qc <- list()
  if (!is.null(raw$poly_min_maf))            qc$min_maf <- as.numeric(raw$poly_min_maf)
  if (!is.null(raw$poly_max_missing_marker)) qc$max_missing_marker <- as.numeric(raw$poly_max_missing_marker)
  if (!is.null(raw$poly_max_missing_sample)) qc$max_missing_sample <- as.numeric(raw$poly_max_missing_sample)

  # native allocator controls, forwarded through `...`
  opt <- raw$method %||% raw$optimizer %||% "greedy_local"
  method <- if (opt %in% c("auto","greedy_local","repair_local","mip_linear","mip_contribution","evolution")) opt else "greedy_local"
  extra <- list(method = method)
  if (!is.null(raw$max_pair_kinship)) extra$max_pair_kinship <- if (identical(raw$max_pair_kinship,"Inf")) Inf else as.numeric(raw$max_pair_kinship)
  if (!is.null(raw$strategy))            extra$strategy <- raw$strategy
  if (!is.null(raw$diversity_emphasis))  extra$diversity_emphasis <- as.numeric(raw$diversity_emphasis)
  if (!is.null(raw$target_coancestry))   extra$target_coancestry <- as.numeric(raw$target_coancestry)
  if (!is.null(raw$committed_crosses)) {
    cc <- raw$committed_crosses
    if (!is.data.frame(cc)) cc <- data.frame(parent1 = as.character(cc$parent1),
                                             parent2 = as.character(cc$parent2), stringsAsFactors = FALSE)
    extra$committed_crosses <- cc
  }

  design_args <- c(list(
    dosage = dosage, n_crosses = as.integer(raw$n_crosses %||% 10L), ploidy = ploidy,
    phenotype = phenotype,
    max_crosses_per_parent = as.integer(raw$max_uses_per_parent %||% 4L),
    run_qc = isTRUE(as.logical(raw$run_qc %||% TRUE)), qc = qc,
    dominance = isTRUE(as.logical(raw$dominance %||% FALSE)),
    gain = raw$gain %||% "mean",
    selection_prop = as.numeric(raw$selection_prop %||% 0.1),
    double_reduction = as.numeric(raw$double_reduction %||% 0),
    grm_method = raw$grm_method %||% "vanraden"), extra)

  plan <- do.call(nextgenCrossDesign::ng_design_crosses_poly, design_args)

  sm <- attr(plan, "summary"); qcr <- attr(plan, "qc")
  plan_df <- as.data.frame(plan, stringsAsFactors = FALSE)
  gain_col <- sm$poly_gain_col %||% (if (isTRUE(design_args$dominance)) "cross_usefulness" else "poly_mean")
  scalar_summary <- sm[vapply(sm, function(x) is.atomic(x) && length(x) == 1L, logical(1))]

  payload <- list(
    schema          = "ng_run_result.v1",
    ok              = TRUE, error = FALSE,
    generated_at    = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    package_version = as.character(utils::packageVersion("nextgenCrossDesign")),
    run_label       = raw$run_label %||% NULL,
    poly_design     = TRUE,
    ploidy          = as.integer(attr(plan, "ploidy") %||% ploidy),
    prediction_mode = "polyploid_design",
    plan_summary    = list(
      n_crosses = nrow(plan_df), mean_gain = sm$mean_gain, group_coancestry = sm$group_coancestry,
      total_gain = sm$total_gain, unique_parents = sm$unique_parents, max_parent_use = sm$max_parent_use,
      strategy = sm$strategy, diversity_emphasis = sm$diversity_emphasis,
      achieved_emphasis = sm$achieved_emphasis, dominance = isTRUE(sm$dominance),
      frontier = sm$frontier),
    selected_crosses = plan_df,
    poly_plan       = list(crosses = plan_df, summary = scalar_summary, qc = qcr,
                           gain_col = gain_col, dominance = isTRUE(sm$dominance)),
    settings        = list(
      ploidy = ploidy, dominance = isTRUE(design_args$dominance), gain = design_args$gain,
      grm_method = design_args$grm_method, double_reduction = design_args$double_reduction,
      selection_prop = design_args$selection_prop, optimizer = method,
      allocation_method = "polyploid_ocs", trait_value_metric = paste0("poly_", design_args$gain),
      progeny = paste0("ploidy_", ploidy)))

  jsonlite::write_json(payload, result_path, auto_unbox = TRUE, null = "null", na = "null",
                       dataframe = "rows", pretty = TRUE, digits = 10)
  cat("OK: wrote", result_path, "(polyploid design)\n")
}

run <- function() {
  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path, call. = FALSE)
  }

  # ---- load backend --------------------------------------------------------
  if (!requireNamespace("nextgenCrossDesign", quietly = TRUE)) {
    stop("The 'nextgenCrossDesign' package is not installed / not on the library path. ",
         "Install it into the library this Rscript uses, or set 'package_library' in config.yml.",
         call. = FALSE)
  }
  suppressWarnings(suppressMessages(library(nextgenCrossDesign)))

  # ---- read + coerce config -----------------------------------------------
  raw <- jsonlite::fromJSON(config_path, simplifyVector = TRUE,
                            simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  if (!is.list(raw)) stop("Config JSON must be a single object of parameters.", call. = FALSE)

  # Polyploid design is a separate entry point (ng_design_crosses_poly): any
  # ploidy, dosage 0..ploidy, optional additive+dominance scoring, no map.
  if (identical(raw$workflow %||% "cross_prediction", "polyploid_design")) {
    run_polyploid_design(raw, result_path)
    return(invisible())
  }

  formals_list <- names(formals(nextgenCrossDesign::ng_run_cross_prediction))

  # Meta keys the UI may include that are NOT backend formals.
  meta_keys <- c("schema", "entrypoint", "runner", "note", "run_label", "run_id", "crop",
                 "cross_number_mode", "cross_sweep_k_min", "cross_sweep_k_max",
                 "cross_sweep_k_step", "cross_sweep_criterion",
                 "cross_sweep_relative_threshold", "cross_sweep_ne_min",
                 "cross_sweep_coancestry_max",
                 "robust_allocation", "robustness_quantile", "robust_objective",
                 "robust_top_n_target")
  supplied  <- setdiff(names(raw), meta_keys)
  unknown   <- setdiff(supplied, formals_list)
  if (length(unknown)) {
    warning("Dropping config keys not recognised by this backend: ",
            paste(unknown, collapse = ", "), call. = FALSE)
  }
  args_in <- raw[intersect(supplied, formals_list)]

  # Drop explicit nulls / empty strings so backend defaults win.
  args_in <- args_in[!vapply(args_in, function(v) {
    is.null(v) || (is.character(v) && length(v) == 1L && !nzchar(v))
  }, logical(1))]

  # Numeric infinities may arrive as the string "Inf".
  for (nm in names(args_in)) {
    v <- args_in[[nm]]
    if (is.character(v) && length(v) == 1L && v %in% c("Inf", "-Inf")) {
      args_in[[nm]] <- as.numeric(v)
    }
  }

  # ---- normalize advanced object-params to backend-native shapes -----------
  # JSON turns tables into lists; the backend wants data.frames / named vectors.
  as_pairs_df <- function(x) {
    if (is.data.frame(x)) return(x)
    if (!is.null(x$parent1) || !is.null(x$parent2))
      return(data.frame(parent1 = as.character(x$parent1),
                        parent2 = as.character(x$parent2), stringsAsFactors = FALSE))
    do.call(rbind, lapply(x, function(r) data.frame(
      parent1 = as.character(r$parent1), parent2 = as.character(r$parent2),
      stringsAsFactors = FALSE)))
  }
  as_rows_df <- function(x, cols, defaults = list()) {
    if (is.data.frame(x)) return(x)
    do.call(rbind, lapply(x, function(r) {
      row <- lapply(cols, function(cn) {
        v <- r[[cn]]; if (is.null(v)) v <- defaults[[cn]]; v
      })
      names(row) <- cols
      as.data.frame(row, stringsAsFactors = FALSE)
    }))
  }
  if (!is.null(args_in$committed_crosses))
    args_in$committed_crosses <- as_pairs_df(args_in$committed_crosses)
  if (!is.null(args_in$marker_target_spec))
    args_in$marker_target_spec <- as_rows_df(args_in$marker_target_spec,
      c("marker", "direction", "target_freq", "weight"),
      list(direction = "increase", target_freq = NA_real_, weight = 1))
  if (!is.null(args_in$lethal_spec))
    args_in$lethal_spec <- as_rows_df(args_in$lethal_spec,
      c("marker", "risk_allele"), list(risk_allele = "alt"))
  if (!is.null(args_in$parent_group) && is.list(args_in$parent_group))
    args_in$parent_group <- stats::setNames(as.character(unlist(args_in$parent_group)),
                                            names(args_in$parent_group))
  if (!is.null(args_in$group_quota))
    args_in$group_quota <- as.list(stats::setNames(
      as.numeric(unlist(args_in$group_quota)), names(args_in$group_quota)))
  if (!is.null(args_in$trait_weights) && is.list(args_in$trait_weights))
    args_in$trait_weights <- stats::setNames(as.numeric(unlist(args_in$trait_weights)),
                                             names(args_in$trait_weights))
  if (!is.null(args_in$cross_cost) && !is.data.frame(args_in$cross_cost))
    args_in$cross_cost <- as.data.frame(args_in$cross_cost, stringsAsFactors = FALSE)
  # group_permission: nested named list of logicals -> logical matrix
  if (!is.null(args_in$group_permission) && !is.matrix(args_in$group_permission) &&
      is.list(args_in$group_permission)) {
    gp <- args_in$group_permission
    rn <- names(gp); cn <- names(gp[[1L]])
    m <- matrix(TRUE, length(rn), length(cn), dimnames = list(rn, cn))
    for (i in seq_along(rn)) for (j in seq_along(cn)) m[i, j] <- isTRUE(gp[[rn[i]]][[cn[j]]])
    args_in$group_permission <- m
  }

  # ---- ensure an output directory exists when writing artifacts ------------
  wants_outputs <- isTRUE(args_in$write_outputs) || isTRUE(args_in$write_figures)
  if (wants_outputs && is.null(args_in$output_dir)) {
    args_in$output_dir <- dirname(normalizePath(result_path, mustWork = FALSE))
  }
  if (!is.null(args_in$output_dir) && !dir.exists(args_in$output_dir)) {
    dir.create(args_in$output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # ---- run backend ---------------------------------------------------------
  # Cross-number mode: "fixed" (default) uses n_crosses as given. "auto" sweeps a
  # range of K with ng_optimize_mating_plan_curve(), picks the diminishing-returns
  # elbow, and returns the final plan at that recommended K plus the whole curve.
  cross_mode <- raw$cross_number_mode %||% "fixed"
  have_sweep <- exists("ng_optimize_mating_plan_curve", where = asNamespace("nextgenCrossDesign")) &&
                exists("ng_parent_kinship", where = asNamespace("nextgenCrossDesign"))
  sweep_out <- NULL

  if (identical(cross_mode, "auto") && have_sweep) {
    k_min  <- as.integer(raw$cross_sweep_k_min %||% 3L)
    k_max  <- as.integer(raw$cross_sweep_k_max %||% max(k_min + 2L, as.integer(args_in$n_crosses %||% 20L)))
    k_step <- max(1L, as.integer(raw$cross_sweep_k_step %||% 1L))
    if (k_max < k_min) { tmp <- k_min; k_min <- k_max; k_max <- tmp }
    K_range <- as.integer(seq(k_min, k_max, by = k_step))
    if (length(K_range) < 2L) K_range <- as.integer(unique(c(k_min, k_max)))

    # 1. Full run at K_max scores every candidate cross.
    args_full <- args_in; args_full$n_crosses <- max(K_range)
    run_full  <- do.call(nextgenCrossDesign::ng_run_cross_prediction, args_full)

    # 2. Sweep the mating-plan portfolio over K and locate the elbow.
    parent_K <- tryCatch(nextgenCrossDesign::ng_parent_kinship(run_full$cleaned_data$genotype),
                         error = function(e) NULL)
    curve <- tryCatch(nextgenCrossDesign::ng_optimize_mating_plan_curve(
      scores                 = run_full$candidate_crosses,
      K_range                = K_range,
      gain_col               = "multi_trait_score",
      parent_K               = parent_K,
      max_crosses_per_parent = args_in$max_uses_per_parent %||% NULL,
      max_pair_kinship       = args_in$max_pair_kinship %||% Inf,
      lambda_group           = args_in$lambda_group %||% 0.05,
      lambda_mating          = args_in$lambda_mating %||% 0.02,
      lambda_parent_use      = args_in$lambda_parent_use %||% 0,
      lambda_parent_use_mode = args_in$lambda_parent_use_mode %||% "absolute",
      method                 = "greedy_local",
      local_iter             = args_in$local_iter %||% 2000,
      ocs_iter               = args_in$ocs_iter %||% 5,
      criterion              = raw$cross_sweep_criterion %||% "elbow_relative",
      relative_threshold     = as.numeric(raw$cross_sweep_relative_threshold %||% 0.05),
      ne_min                 = as.numeric(raw$cross_sweep_ne_min %||% 30),
      coancestry_max         = as.numeric(raw$cross_sweep_coancestry_max %||% 0.05)),
      error = function(e) { attr(e, "ng_msg") <<- conditionMessage(e); NULL })

    if (!is.null(curve)) {
      elbow_K <- attr(curve, "elbow_K")
      if (is.null(elbow_K) || !is.finite(elbow_K)) elbow_K <- max(K_range)
      elbow_K <- as.integer(elbow_K)
      sweep_out <- list(
        curve         = as.data.frame(curve),
        recommended_k = elbow_K,
        criterion     = attr(curve, "criterion") %||% (raw$cross_sweep_criterion %||% "elbow_relative"),
        k_range       = K_range)
      # 3. Final plan at the recommended K (reuse the K_max run if they match).
      if (identical(elbow_K, as.integer(max(K_range)))) {
        result <- run_full
      } else {
        args_final <- args_in; args_final$n_crosses <- elbow_K
        result <- do.call(nextgenCrossDesign::ng_run_cross_prediction, args_final)
      }
    } else {
      result <- run_full   # sweep failed: fall back to the K_max plan
    }
  } else {
    result <- do.call(nextgenCrossDesign::ng_run_cross_prediction, args_in)
  }

  # ---- optional robust posterior re-optimization ---------------------------
  # When posterior prediction ran, re-optimize the plan on a pessimistic
  # posterior quantile (or a top-N probability) so the chosen crosses are those
  # that hold up under marker-effect uncertainty, not just the point estimate.
  robust_out <- NULL
  want_robust <- isTRUE(as.logical(raw$robust_allocation %||% FALSE)) &&
    exists("ng_optimize_robust_mating_plan", where = asNamespace("nextgenCrossDesign")) &&
    exists("ng_parent_kinship", where = asNamespace("nextgenCrossDesign"))
  if (want_robust) {
    ps <- tryCatch(result$posterior_predictions[[1L]], error = function(e) NULL)
    if (is.data.frame(ps) && nrow(ps) > 0L) {
      objective <- raw$robust_objective %||% "posterior_quantile"
      # gain column must carry posterior draws (has *_post_mean); prefer the
      # DH-GEBV usefulness, then fall back to any posterior column.
      cand    <- c("uc_dh_gebv", "uc_dh", "dh_pmv_var", "cross_mean")
      hit     <- cand[paste0(cand, "_post_mean") %in% names(ps)]
      gain_col <- if (length(hit)) hit[1L] else {
        pm <- grep("_post_mean$", names(ps), value = TRUE)
        if (length(pm)) sub("_post_mean$", "", pm[1L]) else "uc_dh_gebv"
      }
      nK  <- if (is.data.frame(result$selected_crosses)) nrow(result$selected_crosses)
             else as.integer(args_in$n_crosses %||% 10L)
      opt <- args_in$optimizer %||% "greedy_local"
      rmethod <- if (opt %in% c("auto","greedy_local","repair_local","mip_linear","mip_contribution")) opt else "greedy_local"
      tnt <- if (identical(objective, "posterior_topn_prob")) as.integer(raw$robust_top_n_target %||% nK) else NULL
      parent_K <- tryCatch(nextgenCrossDesign::ng_parent_kinship(result$cleaned_data$genotype),
                           error = function(e) NULL)
      rob <- tryCatch(nextgenCrossDesign::ng_optimize_robust_mating_plan(
        posterior_scores       = ps, n_crosses = nK, parent_K = parent_K,
        gain_col               = gain_col,
        robustness_quantile    = as.numeric(raw$robustness_quantile %||% 0.25),
        objective              = objective, top_n_target = tnt,
        max_crosses_per_parent = args_in$max_uses_per_parent %||% NULL,
        min_unique_parents     = args_in$min_unique_parents %||% NULL,
        max_pair_kinship       = args_in$max_pair_kinship %||% Inf,
        lambda_group           = args_in$lambda_group %||% 0.05,
        lambda_mating          = args_in$lambda_mating %||% 0.02,
        lambda_parent_use      = args_in$lambda_parent_use %||% 0,
        lambda_parent_use_mode = args_in$lambda_parent_use_mode %||% "absolute",
        method = rmethod, local_iter = args_in$local_iter %||% 2000, ocs_iter = args_in$ocs_iter %||% 5),
        error = function(e) { attr(e, "m") <- conditionMessage(e); e })
      if (inherits(rob, "data.frame")) {
        keep <- intersect(c("parent1", "parent2", gain_col,
                            paste0(gain_col, c("_post_mean", "_post_lower", "_post_upper")),
                            "pair_kinship", "expected_progeny_inbreeding", ".robust_gain"), names(rob))
        # which robust crosses are NOT in the point-estimate plan
        pkey <- function(df) paste(pmin(df$parent1, df$parent2), pmax(df$parent1, df$parent2))
        std_key <- if (is.data.frame(result$selected_crosses)) pkey(result$selected_crosses) else character(0)
        rk <- pkey(rob)
        robust_out <- list(
          crosses      = as.data.frame(rob)[, keep, drop = FALSE],
          summary      = attr(rob, "summary"),
          gain_col     = gain_col, n_crosses = nK,
          n_shared_with_standard = sum(rk %in% std_key),
          n_changed    = sum(!(rk %in% std_key)))
      } else {
        robust_out <- list(error = attr(rob, "m") %||% "robust re-optimization failed")
      }
    } else {
      robust_out <- list(error = "posterior prediction produced no scores to re-optimize")
    }
  }

  # ---- assemble ng_run_result.v1 payload -----------------------------------
  pick <- function(name) if (!is.null(result[[name]])) result[[name]] else NULL

  payload <- list(
    schema          = "ng_run_result.v1",
    ok              = TRUE,
    error           = FALSE,
    generated_at    = format(as.POSIXct(Sys.time(), tz = "UTC"),
                             "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    package_version = as.character(utils::packageVersion("nextgenCrossDesign")),
    run_label       = raw$run_label %||% NULL,
    prediction_mode = pick("prediction_mode"),
    settings        = pick("settings"),
    input_match_audit = pick("input_match_audit"),
    qc              = pick("qc"),
    effect_summary  = pick("effect_summary"),
    trait_direction = pick("trait_direction"),
    objective       = pick("objective"),
    plan_summary    = pick("plan_summary"),
    candidate_crosses = pick("candidate_crosses"),
    selected_crosses  = pick("selected_crosses"),
    ld_pruning_report = pick("ld_pruning_report"),
    posterior_predictions = pick("posterior_predictions"),
    output_files    = pick("output_files"),
    cross_number_sweep = sweep_out,
    robust_plan     = robust_out
  )
  payload <- payload[!vapply(payload, is.null, logical(1))]

  jsonlite::write_json(payload, result_path,
                       auto_unbox = TRUE, null = "null", na = "null",
                       dataframe = "rows", pretty = TRUE, digits = 10)

  # ---- optional capability registry ---------------------------------------
  if (!is.null(capabilities_path) &&
      exists("ng_write_backend_capability_registry_json",
             where = asNamespace("nextgenCrossDesign"))) {
    try(nextgenCrossDesign::ng_write_backend_capability_registry_json(capabilities_path),
        silent = TRUE)
  }

  cat("OK: wrote", result_path, "\n")
}

tryCatch(run(), error = function(e) {
  emit_error(conditionMessage(e), result_path)
  quit(status = 1L, save = "no")
})
