# Shared helpers for the test suite.

demo_dir <- function() system.file("app", "data", "demo", package = "nextgenCrossWorkbench")

demo_paths <- function() {
  d <- demo_dir()
  list(genotype = file.path(d, "genotype.csv"),
       phenotype = file.path(d, "phenotype.csv"),
       map = file.path(d, "marker_map.csv"),
       direction = file.path(d, "trait_direction.csv"))
}

# Load the wrapper's internal helper functions (ngcd_coerce_backend_args(),
# ngcd_full_backend_config(), poly_design_args(), ...) from
# inst/app/tools/run_cross_prediction_json.R WITHOUT invoking its main run()
# entry point. That file is a standalone Rscript, not part of the package
# namespace, so ng()/getFromNamespace() cannot reach its functions -- every
# other test that needs its behaviour drives it as a real subprocess
# (skip_on_cran + backend-gated). This gives fast, in-process unit tests of
# its pure coercion helpers instead.
#
# Mechanics: the script's very first top-level statements read
# commandArgs(trailingOnly = TRUE) and stop() if fewer than 2 were supplied --
# sourcing it normally would abort right there, before ngcd_coerce_backend_args
# is even defined. So this parses the file into individual top-level
# expressions, gives the evaluation environment its own commandArgs() override
# (returning two dummy strings) to clear that check, and evaluates every
# expression EXCEPT the very last one, which is the file's own
# `tryCatch(run(), error = ...)` invocation -- run() is a real end-to-end
# driver (reads config JSON, calls the backend, writes results, quit()s the
# whole R session on error), so it must never actually execute here.
ngcd_runner_env <- function() {
  runner <- testthat::test_path("..", "..", "inst", "app", "tools", "run_cross_prediction_json.R")
  if (!file.exists(runner)) {
    runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                          package = "nextgenCrossWorkbench")
  }
  exprs <- parse(runner)
  e <- new.env()
  e$commandArgs <- function(...) c("_unused_config_", "_unused_result_")
  n <- length(exprs)
  for (i in seq_len(n - 1L)) eval(exprs[[i]], envir = e)
  e
}

# Can we actually drive a backend run? (Rscript + nextgenCrossDesign present.)
backend_available <- function() {
  cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
  b <- tryCatch(nextgenCrossWorkbench:::ngcd_check_backend(cfg), error = function(e) NULL)
  !is.null(b) && isTRUE(b$rscript_ok) && isTRUE(b$backend_installed)
}

# Why the backend was judged unavailable, in the skip message itself.
#
# backend_available() does not test whether THIS session can see the backend --
# ngcd_check_backend() spawns a nested Rscript and probes there, so it can be
# false for several unrelated reasons: Rscript unresolvable, the child not
# searching the library the backend was installed into, or the check erroring
# outright. A bare "Backend not available." cannot distinguish them, which cost
# a full CI cycle: the backend installed correctly, a hand-run probe reported
# BACKEND=TRUE, and these tests still skipped with no way to see why.
#
# Report the actual state instead. A skip that explains itself is the whole
# difference between a diagnosable CI run and a guess.
backend_skip_reason <- function() {
  cfg <- tryCatch(nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb")),
                  error = function(e) NULL)
  if (is.null(cfg)) return("Backend not available: ngcd_load_config() errored.")
  b <- tryCatch(nextgenCrossWorkbench:::ngcd_check_backend(cfg), error = function(e) e)
  if (inherits(b, "condition"))
    return(paste0("Backend not available: ngcd_check_backend() errored: ",
                  conditionMessage(b)))
  paste0("Backend not available: rscript_ok=", isTRUE(b$rscript_ok),
         " resolved='", b$rscript_resolved, "'",
         " installed=", isTRUE(b$backend_installed),
         " version=", b$backend_version,
         " package_library='", b$package_library, "'",
         " runner_exists=", isTRUE(b$runner_exists),
         " R_LIBS_USER='", Sys.getenv("R_LIBS_USER"), "'",
         if (length(b$messages)) paste0(" | ", paste(b$messages, collapse = "; ")) else "")
}

# A complete set of default inputs for the server, overridable via `...`.
demo_inputs <- function(...) {
  base <- list(
    data_source = "demo",
    genotype_id_col = "NAME", phenotype_id_col = "NAME",
    map_marker_col = "SNP_code", map_chr_col = "Chromosome", map_pos_bp_col = "Position_BP",
    pos_unit = "bp", bp_per_cm = 1e6,
    map_pos_cm_col = "Position_cM", map_pos_cm_divisor = 1, nselfing = 8,
    crop = "Wheat (spring)", ploidy = "2", ril_mode = "infinite",
    local_iter = 2000, ocs_iter = 5, n_threads = NA, alphamate_n_threads = 1,
    direction_trait_col = "Trait", direction_column_col = "Trait", direction_direction_col = "Selection_direction",
    objective_mode = "multi", single_trait = "yield", traits_to_use = c("yield", "disease"),
    index_col = "yield", index_direction = "increase",
    multi_trait_method = "auto", threshold_policy = "soft",
    threshold_penalty_weight = 1, threshold_penalty_autoscale = TRUE, trait_weights = "",
    trait_value_metric = "var_complex", uc_variance_source = "pmv", method_varPMV = "fast", selection_prop = 0.2,
    progeny = "DH", recomb_model = "haldane", grm_method = "vanraden",
    parent_type = "inbred", min_effect_reliability = 0.35,
    duplicate_action = "none", duplicate_threshold = 0.995, duplicate_maf_min = 0.01,
    duplicate_max_missing_prop = 0.4, duplicate_min_compared_markers = 100,
    ld_pruning = FALSE, ld_window = 100, ld_r2_threshold = 0.9, ld_maf_threshold = 0.01, ld_backend = "auto",
    n_crosses = 10, max_crosses_per_parent = 4, min_unique_parents = NA, max_pair_kinship = NA,
    optimizer = "greedy_local", allocation_method = "ocs", use_ocs = TRUE,
    lambda_group = 0.05, lambda_mating = 0.02, lambda_parent_use = 0, lambda_parent_use_mode = "absolute",
    lambda_progeny_inbreeding = 0, min_crosses_per_parent = 0,
    committed_crosses = "", parent_group = "", group_quota = "", group_disallow = "",
    training_genotype_id_col = "", training_phenotype_id_col = "",
    cost_col = "", logistic_col = "",
    alphamate_runtime_path = "", alphamate_workdir = "", alphamate_number_of_parents = NA,
    alphamate_lambda_group = NA, alphamate_keep_files = FALSE, alphamate_lambda_grid = "",
    alphamate_evol_solutions = 100, alphamate_evol_iterations = 1000, alphamate_evol_stop = 200,
    marker_target_spec = "", lambda_marker = 0, lethal_spec = "", drop_lethal_carrier_crosses = TRUE,
    budget = NA, lambda_cost = 0, lambda_logistic = 0,
    run_posterior_prediction = FALSE, posterior_method = "mcmc", n_iter = 5000, burn_in = 500, use_parallel = FALSE,
    priority_breaks = "0.10, 0.35, 0.70, 1.00",
    priority_labels = "highly_priority, priority, medium_priority, low_priority",
    priority_score_weight = 1, priority_kinship_weight = 0.15, priority_threshold_weight = 1,
    write_outputs = FALSE, write_figures = FALSE, output_file = "crossing_plan.xlsx", seed = 20260706,
    diversity_mode = "strategy", strategy = "balanced", diversity_emphasis = 45, target_coancestry = 0.05,
    evol_solutions = 100, evol_iterations = 200, evol_stop = 40, evol_seed = NA,
    alphamate_mode = "ModeOptTarget1", alphamate_target_degree = 45, alphamate_max_contributions = NA,
    alphamate_executable = "",
    restrict_shared_markers = FALSE, restrict_shared_ids = FALSE, drop_noninbred_parents = FALSE
  )
  ov <- list(...)
  base[names(ov)] <- ov
  base
}
