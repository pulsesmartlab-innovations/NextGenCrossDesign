# ===========================================================================
# combinations.R  -  parameter-combination sweep against the backend
# ===========================================================================
# Runs many parameter combinations through the real backend on the demo data
# and reports which combinations succeed or fail. Used by run_combination_tests()
# and by tests/testthat/test-combinations.R.

# Base runner config for the demo data. For index_as_trait it writes an
# augmented phenotype carrying a trusted precomputed index column.
ngcd_combo_base <- function(cfg, prediction_mode = "trait_by_trait", work_dir = tempdir()) {
  demo <- ngcd_demo_files(cfg)
  base <- list(
    schema = "ng_run_config.v1",
    genotype_file = demo$genotype, phenotype_file = demo$phenotype,
    map_file = demo$map, direction_file = demo$direction,
    genotype_id_col = "NAME", phenotype_id_col = "NAME",
    map_marker_col = "SNP_code", map_chr_col = "Chromosome", map_pos_bp_col = "Position_BP",
    map_position_unit = "bp", bp_per_cm = 1e6,
    direction_trait_col = "Trait", direction_column_col = "Trait",
    direction_direction_col = "Selection_direction",
    prediction_mode = prediction_mode,
    trait_value_metric = "var_complex", uc_variance_source = "pmv", method_varPMV = "fast",
    multi_trait_method = "auto", progeny = "DH", recombination_model = "haldane",
    grm_method = "vanraden", selection_prop = 0.2, assume_inbred = TRUE,
    duplicate_action = "none", n_crosses = 10, max_uses_per_parent = 4,
    optimizer = "greedy_local", allocation_method = "ocs", use_ocs = TRUE,
    write_outputs = FALSE, write_figures = FALSE, seed = 20260706)
  if (identical(prediction_mode, "index_as_trait")) {
    ph <- utils::read.csv(demo$phenotype, check.names = FALSE)
    ph$sel_index <- ph$yield
    if (!dir.exists(work_dir)) dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
    pth <- file.path(work_dir, "phenotype_index.csv")
    utils::write.csv(ph, pth, row.names = FALSE)
    base$phenotype_file <- pth
    base$index_col <- "sel_index"; base$index_direction <- "increase"
  }
  base
}

# Run a single combination: merge overrides into the base config, invoke the
# runner, return ok/message/n_selected/seconds.
ngcd_run_combo <- function(cfg, overrides = list(), work_dir = tempdir()) {
  pm <- overrides$prediction_mode %||% "trait_by_trait"
  base <- ngcd_combo_base(cfg, pm, work_dir)
  params <- utils::modifyList(base, overrides)
  # Diversity-dial exclusivity: exactly one of strategy / emphasis / target.
  if (!is.null(params$diversity_emphasis) || !is.null(params$target_coancestry)) params$strategy <- NULL
  if (is.null(params$strategy) && is.null(params$diversity_emphasis) && is.null(params$target_coancestry))
    params$strategy <- "balanced"

  run_dir <- tempfile("combo", tmpdir = cfg$runs_dir); dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  config_path <- file.path(run_dir, "config.json"); result_path <- file.path(run_dir, "result.json")
  ngcd_write_config(params, config_path)

  rscript <- ngcd_resolve_rscript(cfg)
  env <- if (nzchar(cfg$package_library)) paste0("R_LIBS_USER=", cfg$package_library) else character(0)
  t0 <- Sys.time()
  log <- tryCatch(system2(rscript, c(shQuote(cfg$runner_script), shQuote(config_path), shQuote(result_path)),
                          stdout = TRUE, stderr = TRUE, env = env),
                  error = function(e) paste("invoke error:", conditionMessage(e)))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  res <- if (file.exists(result_path))
    tryCatch(jsonlite::fromJSON(result_path, simplifyDataFrame = TRUE), error = function(e) NULL) else NULL
  ok <- !is.null(res) && isTRUE(res$ok)
  msg <- if (ok) "" else if (!is.null(res) && !is.null(res$error_message)) res$error_message
         else { ll <- log[nzchar(trimws(log))]; if (length(ll)) utils::tail(ll, 1) else "no result written" }
  list(ok = ok, message = as.character(msg),
       n_selected = if (ok && !is.null(res$selected_crosses)) nrow(res$selected_crosses) else NA_integer_,
       seconds = round(secs, 2), run_dir = run_dir)
}

# Build the combination list. `level` = "smoke" (one-at-a-time only) or
# "full" (one-at-a-time + a factorial core).
ngcd_combo_list <- function(level = c("full", "smoke")) {
  level <- match.arg(level)
  combos <- list()
  add <- function(varied, value, ov) combos[[length(combos) + 1L]] <<-
    list(varied = varied, value = value, overrides = ov)

  metrics  <- c("var_complex", "uc", "pmv", "vpm", "mean", "var_simple")
  optims   <- c("auto", "evolution", "greedy_local", "repair_local", "mip_linear", "mip_contribution")
  methods  <- c("auto", "weighted", "economic_index", "desired_gain")
  ucsrc    <- c("pmv", "vpm", "var_simple")
  divspecs <- list(c("strategy", "high_gain"), c("strategy", "balanced"), c("strategy", "diversity"),
                   c("emphasis", "15"), c("emphasis", "50"), c("emphasis", "85"), c("target", "0.05"))

  wt <- list(yield = 0.5, disease = 0.5)
  method_ov <- function(m) if (m == "weighted") list(multi_trait_method = m, trait_weights = wt)
                           else list(multi_trait_method = m)
  div_ov <- function(kind, val) switch(kind,
    strategy = list(strategy = val, diversity_emphasis = NULL, target_coancestry = NULL),
    emphasis = list(strategy = NULL, diversity_emphasis = as.numeric(val), target_coancestry = NULL),
    target   = list(strategy = NULL, diversity_emphasis = NULL, target_coancestry = as.numeric(val)))

  # ---- one-at-a-time sweeps ----
  for (v in metrics) add("trait_value_metric", v, list(trait_value_metric = v))
  for (v in ucsrc)   add("uc_variance_source", v, list(trait_value_metric = "uc", uc_variance_source = v))
  for (v in methods) add("multi_trait_method", v, method_ov(v))
  for (v in c("DH", "RIL"))            add("progeny", v, list(progeny = v))
  for (v in c("haldane", "kosambi"))   add("recombination_model", v, list(recombination_model = v))
  for (v in c("vanraden", "yang"))     add("grm_method", v, list(grm_method = v))
  for (v in c("fast", "full_posterior")) add("method_varPMV", v, list(method_varPMV = v))
  for (v in optims)  add("optimizer", v, list(optimizer = v))
  for (v in c("ocs", "alphamate_style")) add("allocation_method", v, list(allocation_method = v))
  for (v in c("remove", "report", "none")) add("duplicate_action", v, list(duplicate_action = v))
  for (s in divspecs) add("diversity", paste(s, collapse = ":"), div_ov(s[1], s[2]))
  for (v in c("trait_by_trait", "index_as_trait")) add("prediction_mode", v, list(prediction_mode = v))
  add("ld_pruning", "TRUE", list(ld_pruning = TRUE))
  add("assume_inbred", "FALSE", list(assume_inbred = FALSE))

  # ---- advanced mate-selection controls (all optional; default off) ----
  add("advanced", "progeny_inbreeding", list(lambda_progeny_inbreeding = 0.05))
  add("advanced", "min_crosses_per_parent", list(min_crosses_per_parent = 2))
  add("advanced", "committed_crosses",
      list(committed_crosses = list(parent1 = "P09", parent2 = "P10")))
  add("advanced", "parent_group_quota",
      list(parent_group = stats::setNames(as.list(rep(c("A", "B"), each = 5)), sprintf("P%02d", 1:10)),
           group_quota = list("A||B" = 4)))
  add("advanced", "marker_steering",
      list(marker_target_spec = list(list(marker = "SNP_001", direction = "increase",
                                          target_freq = 0.9, weight = 1)), lambda_marker = 1))
  add("advanced", "lethal_guarding",
      list(lethal_spec = list(list(marker = "SNP_002", risk_allele = "alt")),
           drop_lethal_carrier_crosses = TRUE))
  add("advanced", "cost_budget_logistic",
      list(budget = 1e6, lambda_cost = 0.1, lambda_logistic = 0.1))

  # ---- factorial core (full only) ----
  if (level == "full") {
    grid <- expand.grid(
      metric = metrics,
      optimizer = c("auto", "evolution", "greedy_local", "repair_local"),
      method = c("auto", "weighted"),
      progeny = c("DH", "RIL"),
      stringsAsFactors = FALSE)
    for (i in seq_len(nrow(grid))) {
      r <- grid[i, ]
      ov <- utils::modifyList(list(trait_value_metric = r$metric, optimizer = r$optimizer,
                                   progeny = r$progeny), method_ov(r$method))
      add("factorial", sprintf("%s|%s|%s|%s", r$metric, r$optimizer, r$method, r$progeny), ov)
    }
  }
  combos
}

# Benign, expected failures: the backend correctly refusing an infeasible or
# degenerate plan (e.g. n_crosses beyond the parent pool's capacity). These are
# not bugs.
ngcd_expected_fail <- function(message) {
  m <- tolower(message %||% "")
  grepl("feasible plan|feasible alphamate|could not build|could not repair|could not fill|infeasible|no feasible", m)
}

# Draw `n` random combinations spanning ALL parameters. Slow paths (evolution,
# posterior) are given small budgets so a large sweep stays fast; the goal is to
# exercise code paths and detect errors, not to tune optimization quality.
ngcd_combo_random <- function(n = 1000, seed = 1) {
  set.seed(seed)
  metrics <- c("var_complex", "uc", "pmv", "vpm", "mean", "var_simple")
  optims  <- c("auto", "evolution", "greedy_local", "repair_local", "mip_linear", "mip_contribution")
  methods <- c("auto", "weighted", "economic_index", "desired_gain")
  ucsrc   <- c("pmv", "vpm", "var_simple")
  pick <- function(x) x[sample.int(length(x), 1L)]
  combos <- vector("list", n)
  for (i in seq_len(n)) {
    opt <- pick(optims); method <- pick(methods)
    pmode <- pick(c("trait_by_trait", "index_as_trait"))
    # In index_as_trait mode the index IS the objective; a multi-trait method
    # does not apply, so keep it at the default.
    if (pmode == "index_as_trait") method <- "auto"
    ov <- list(
      prediction_mode = pmode,
      trait_value_metric = pick(metrics),
      uc_variance_source = pick(ucsrc),
      method_varPMV = if (stats::runif(1) < 0.15) "full_posterior" else "fast",
      multi_trait_method = method,
      progeny = pick(c("DH", "RIL")),
      recombination_model = pick(c("haldane", "kosambi")),
      grm_method = pick(c("vanraden", "yang")),
      optimizer = opt,
      allocation_method = pick(c("ocs", "alphamate_style")),
      duplicate_action = pick(c("none", "remove", "report")),
      threshold_policy = pick(c("soft", "strict")),
      selection_prop = round(stats::runif(1, 0.02, 0.5), 3),
      n_crosses = sample(3:30, 1L),
      max_uses_per_parent = sample(1:8, 1L),
      lambda_group = round(stats::runif(1, 0, 0.2), 3),
      lambda_mating = round(stats::runif(1, 0, 0.1), 3),
      lambda_parent_use = round(stats::runif(1, 0, 0.1), 3),
      lambda_progeny_inbreeding = round(stats::runif(1, 0, 0.1), 3),
      ld_pruning = stats::runif(1) < 0.2,
      grm = NULL)
    if (method == "weighted") {
      w <- stats::runif(2, 0.1, 1); ov$trait_weights <- list(yield = round(w[1], 2), disease = round(w[2], 2))
    }
    dm <- pick(c("strategy", "emphasis", "target"))
    if (dm == "strategy") ov$strategy <- pick(c("high_gain", "balanced", "diversity"))
    else if (dm == "emphasis") ov$diversity_emphasis <- sample(0:100, 1L)
    else ov$target_coancestry <- round(stats::runif(1, 0, 0.3), 3)
    if (stats::runif(1) < 0.3) ov$min_unique_parents <- sample(2:10, 1L)
    if (stats::runif(1) < 0.3) ov$max_pair_kinship <- round(stats::runif(1, 0, 1), 2)
    if (opt == "evolution") { ov$evol_solutions <- 15L; ov$evol_iterations <- 20L; ov$evol_stop <- 8L; ov$evol_seed <- 42L }
    if (stats::runif(1) < 0.1) { ov$run_posterior_prediction <- TRUE; ov$posterior_method <- "closed_form"; ov$nIter <- 300L; ov$burnIn <- 50L }
    ov$grm <- NULL
    combos[[i]] <- list(varied = "random", value = as.character(i), overrides = ov)
  }
  combos
}

#' Run a parameter-combination sweep against the backend
#'
#' Executes many parameter combinations on the bundled demo data and reports
#' which succeed or fail. A quick way to detect regressions across the whole
#' option surface.
#'
#' @param dir Working directory holding \code{config.yml} (default: current).
#' @param level \code{"full"} (one-at-a-time + factorial core) or \code{"smoke"}
#'   (one-at-a-time only, faster).
#' @param random Number of additional random combinations spanning ALL
#'   parameters to draw and run (0 = none).
#' @param workers Number of parallel worker processes (forking; Unix/macOS
#'   only, ignored on Windows). Default 1 (serial).
#' @param seed Seed for the random combination sampler.
#' @param verbose Print progress and a summary.
#' @param results_csv Optional path to write the full results table.
#' @return A data.frame of results (varied, value, ok, message, n_selected,
#'   seconds), invisibly.
#' @export
run_combination_tests <- function(dir = getwd(), level = c("full", "smoke"),
                                  random = 0, workers = 1, seed = 1,
                                  verbose = TRUE, results_csv = file.path(dir, "combination_results.csv")) {
  level <- match.arg(level)
  cfg <- ngcd_load_config(dir)
  b <- ngcd_check_backend(cfg)
  if (!isTRUE(b$rscript_ok) || !isTRUE(b$backend_installed))
    stop("Backend not available: ", paste(b$messages, collapse = " "), call. = FALSE)

  combos <- ngcd_combo_list(level)
  if (random > 0) combos <- c(combos, ngcd_combo_random(random, seed))
  work_dir <- file.path(cfg$runs_dir, "_combo_work"); dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

  can_par <- workers > 1 && .Platform$OS.type == "unix" && requireNamespace("parallel", quietly = TRUE)
  if (verbose) message(sprintf("Running %d combinations (level=%s, random=%d)%s...",
                               length(combos), level, random,
                               if (can_par) sprintf(", %d workers", workers) else ""))

  run_one <- function(k) {
    cc <- combos[[k]]
    r <- ngcd_run_combo(cfg, cc$overrides, work_dir)
    if (verbose && !can_par) message(sprintf("  [%d/%d] %-18s %-10s %s%s", k, length(combos), cc$varied, cc$value,
      if (r$ok) "OK" else "FAIL",
      if (r$ok) sprintf(" (%d crosses, %.1fs)", r$n_selected, r$seconds) else paste0(" - ", substr(r$message, 1, 70))))
    data.frame(i = k, varied = cc$varied, value = cc$value, ok = r$ok,
      n_selected = r$n_selected, seconds = r$seconds,
      message = substr(r$message, 1, 200), stringsAsFactors = FALSE)
  }
  rows <- if (can_par) parallel::mclapply(seq_along(combos), run_one, mc.cores = workers, mc.preschedule = FALSE)
          else lapply(seq_along(combos), run_one)
  rows <- rows[vapply(rows, is.data.frame, logical(1))]  # drop any worker that died
  res <- do.call(rbind, rows)
  res$expected_fail <- !res$ok & vapply(res$message, ngcd_expected_fail, logical(1))
  res$unexpected_fail <- !res$ok & !res$expected_fail

  if (!is.null(results_csv)) tryCatch(utils::write.csv(res, results_csv, row.names = FALSE), error = function(e) NULL)
  if (verbose) {
    message(sprintf("\n== Summary ==\n  total: %d | ok: %d | expected-fail (infeasible): %d | UNEXPECTED-fail: %d",
                    nrow(res), sum(res$ok), sum(res$expected_fail), sum(res$unexpected_fail)))
    if (any(res$unexpected_fail)) {
      message("  UNEXPECTED failures (investigate):")
      f <- res[res$unexpected_fail, ]
      for (k in seq_len(nrow(f)))
        message(sprintf("    - %s=%s : %s", f$varied[k], f$value[k], substr(f$message[k], 1, 90)))
    } else message("  no unexpected failures.")
    if (!is.null(results_csv)) message("  results written to: ", results_csv)
  }
  invisible(res)
}
