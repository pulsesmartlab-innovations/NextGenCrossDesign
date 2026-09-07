# Robust posterior allocation: when enabled, the runner re-optimizes the plan on
# a pessimistic posterior quantile via ng_optimize_robust_mating_plan() and
# returns the robust crosses + a comparison to the standard plan.
# Backend-gated (runs the real prediction with posterior draws).

test_that("robust allocation returns a robust plan alongside the standard plan", {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the backend robust-plan check.")
  skip_if(!backend_available(), "Backend not available.")
  skip_if_not(exists("ng_optimize_robust_mating_plan",
                     where = asNamespace("nextgenCrossDesign")),
              "Backend lacks ng_optimize_robust_mating_plan.")

  cfg  <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wbrobust"))
  demo <- nextgenCrossWorkbench:::ngcd_demo_files(cfg)
  rd   <- tempfile("run"); dir.create(rd, recursive = TRUE)

  cfgj <- list(schema = "ng_run_config.v1",
    phenotype_file = demo$phenotype, genotype_file = demo$genotype,
    map_file = demo$map, direction_file = demo$direction,
    phenotype_id_col = "NAME", genotype_id_col = "NAME", direction_trait_col = "Trait",
    direction_column_col = "Trait", direction_direction_col = "Selection_direction",
    map_marker_col = "SNP_code", map_chr_col = "Chromosome",
    # bp positions are physical, not genetic: the backend refuses to treat them as
    # centimorgans without an explicit bp:cM ratio. 1e6 (1 Mb per cM) is the app's own
    # default (NGCD_BP_PER_CM_DEFAULT) and what R/combinations.R uses for this same demo
    # map, so every demo-data fixture stays on one genetic scale (the demo's four
    # chromosomes span ~30-42 Mb, i.e. ~30-42 cM at this ratio).
    map_pos_bp_col = "Position_BP", map_position_unit = "bp", bp_per_cm = 1e6,
    prediction_mode = "trait_by_trait", multi_trait_method = "auto",
    trait_value_metric = "pmv", progeny = "DH", parent_type = "inbred",
    duplicate_action = "none",
    run_posterior_prediction = TRUE, posterior_method = "closed_form", n_iter = 12, burn_in = 4,
    n_crosses = 7, max_crosses_per_parent = 4, optimizer = "greedy_local",
    allocation_method = "ocs", use_ocs = TRUE, seed = 1,
    # 0.25 is the app's OWN default and the midpoint of its 0.05-0.50 slider. It used to
    # fail here: ng_optimize_robust_mating_plan() will only serve a quantile the posterior
    # draws actually cached, and before backend 0.25.0 that was only the CI tails
    # (0.025 / 0.975 at ci_level = 0.95), so no value this slider can produce yielded a
    # plan. The fix is NOT to write 0.025 here (a config the UI cannot build) and NOT
    # allow_normal_approximation: the run now sends robustness_quantile into
    # ng_run_cross_prediction(), which caches that exact empirical tail from the same
    # draws. The "for the right reason" assertions live in the two tests below.
    robust_allocation = TRUE, robustness_quantile = 0.25,
    robust_objective = "posterior_quantile")

  cfgp <- file.path(rd, "config.json"); resp <- file.path(rd, "result.json")
  jsonlite::write_json(cfgj, cfgp, auto_unbox = TRUE, null = "null", pretty = TRUE)
  runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                        package = "nextgenCrossWorkbench")
  system2("Rscript", c(runner, cfgp, resp), stdout = FALSE, stderr = FALSE)

  res <- jsonlite::fromJSON(resp, simplifyVector = TRUE)
  expect_true(isTRUE(res$ok), info = res$error_message)
  rp <- res$robust_plan
  expect_false(is.null(rp), info = "runner returned no robust_plan block at all")
  # The runner degrades a refused robust re-optimization to robust_plan$error rather than
  # failing the run, so EVERY assertion below must carry that message -- otherwise the
  # whole block fails mutely (which is exactly how the bp_per_cm defect hid for two
  # releases). See the note above robustness_quantile in the config.
  why <- rp$error %||% ""
  expect_null(rp$error, info = why)
  expect_equal(nrow(rp$crosses), 7L, info = why)
  expect_true(all(c("parent1", "parent2") %in% names(rp$crosses)), info = why)
  expect_true(!is.null(rp$summary$robust_objective), info = why)
  expect_equal(rp$n_shared_with_standard + rp$n_changed, 7L, info = why)
})

# ---------------------------------------------------------------------------
# Regression cover for the two defects fixed in workbench 0.28.0 / backend
# 0.25.0. Both drive the real runner subprocess, because the wiring under test
# (config -> ng_run_cross_prediction(robustness_quantile=) -> cached tail ->
# ng_optimize_robust_mating_plan(direction=)) only exists end to end.
# ---------------------------------------------------------------------------

# Build a robust-allocation config over the demo data. `direction_file` lets a
# caller make the FIRST (and only) trait a genuinely minimize-oriented one --
# emit_run_result() robust-allocates on posterior_predictions[[1]].
robust_cfg <- function(rd, metric, q, direction_file = NULL, ...) {
  demo <- nextgenCrossWorkbench:::ngcd_demo_files(
    nextgenCrossWorkbench:::ngcd_load_config(tempfile("wbrobust")))
  c(list(schema = "ng_run_config.v1",
    phenotype_file = demo$phenotype, genotype_file = demo$genotype,
    map_file = demo$map, direction_file = direction_file %||% demo$direction,
    phenotype_id_col = "NAME", genotype_id_col = "NAME", direction_trait_col = "Trait",
    direction_column_col = "Trait", direction_direction_col = "Selection_direction",
    map_marker_col = "SNP_code", map_chr_col = "Chromosome",
    map_pos_bp_col = "Position_BP", map_position_unit = "bp", bp_per_cm = 1e6,
    prediction_mode = "trait_by_trait", multi_trait_method = "auto",
    trait_value_metric = metric, progeny = "DH", parent_type = "inbred",
    duplicate_action = "none",
    run_posterior_prediction = TRUE, posterior_method = "closed_form",
    n_iter = 12, burn_in = 4,
    n_crosses = 7, max_crosses_per_parent = 4, optimizer = "greedy_local",
    allocation_method = "ocs", use_ocs = TRUE, seed = 1,
    robust_allocation = TRUE, robustness_quantile = q,
    robust_objective = "posterior_quantile"), list(...))
}

run_robust <- function(rd, cfgj) {
  cfgp <- tempfile("config", rd, ".json"); resp <- tempfile("result", rd, ".json")
  jsonlite::write_json(cfgj, cfgp, auto_unbox = TRUE, null = "null", pretty = TRUE)
  runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                        package = "nextgenCrossWorkbench")
  system2("Rscript", c(runner, cfgp, resp), stdout = FALSE, stderr = FALSE)
  jsonlite::fromJSON(resp, simplifyVector = TRUE)
}

test_that("a NON-default robustness quantile yields a real plan from the exact cached tail, not a normal approximation", {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the backend robust-plan check.")
  skip_if(!backend_available(), "Backend not available.")
  skip_if_not(exists("ng_optimize_robust_mating_plan",
                     where = asNamespace("nextgenCrossDesign")),
              "Backend lacks ng_optimize_robust_mating_plan.")

  rd  <- tempfile("run"); dir.create(rd, recursive = TRUE)
  # 0.10 is a slider position that is neither the app default nor any CI tail, so
  # nothing but the run-time caching of that exact quantile can serve it.
  res <- run_robust(rd, robust_cfg(rd, "usefulness", 0.10))
  expect_true(isTRUE(res$ok), info = res$error_message)
  rp  <- res$robust_plan
  why <- rp$error %||% ""
  expect_null(rp$error, info = why)
  expect_equal(nrow(rp$crosses), 7L, info = why)

  s <- rp$summary
  # The plan is genuinely produced from a cached empirical quantile of the draws:
  # the tail is the requested one, its source is the cached <gain>_post_q0_1 column,
  # and the normal-approximation escape hatch was NOT used.
  expect_equal(s$robust_tail_probability, 0.10, info = why)
  expect_false(isTRUE(s$robustness_quantile_is_normal_approximation), info = why)
  expect_match(s$robust_quantile_source, "_post_q0_1$", info = why)
  # ...and it is a distinct tail, not a relabelled CI bound.
  expect_false(grepl("_post_lower$|_post_upper$", s$robust_quantile_source), info = why)
})

test_that("the robust tail follows the RANKED value's orientation: minimize for a decrease trait on mean/usefulness, but still maximize for a pure-variance metric", {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the backend robust-plan check.")
  skip_if(!backend_available(), "Backend not available.")
  skip_if_not(exists("ng_optimize_robust_mating_plan",
                     where = asNamespace("nextgenCrossDesign")),
              "Backend lacks ng_optimize_robust_mating_plan.")

  rd <- tempfile("run"); dir.create(rd, recursive = TRUE)
  # A direction file carrying ONLY the demo's decrease trait, so the posterior the
  # runner robust-allocates on (posterior_predictions[[1]]) is minimize-oriented.
  dirf <- file.path(rd, "direction_decrease_only.csv")
  utils::write.csv(data.frame(Trait = "disease", Selection_direction = "decrease",
                              stringsAsFactors = FALSE),
                   dirf, row.names = FALSE, quote = FALSE)

  # usefulness carries the trait's own units -> lower is better -> the CONSERVATIVE
  # tail is the UPPER one (1 - q). Sending no direction (the pre-0.28.0 behaviour)
  # would have taken the 0.25 tail: the cross's BEST case, labelled robust.
  useful <- run_robust(rd, robust_cfg(rd, "usefulness", 0.25, direction_file = dirf))
  expect_true(isTRUE(useful$ok), info = useful$error_message)
  ru  <- useful$robust_plan
  whu <- ru$error %||% ""
  expect_null(ru$error, info = whu)
  expect_identical(ru$direction, "minimize", info = whu)
  expect_identical(ru$direction_source, "posterior_metadata", info = whu)
  expect_equal(ru$summary$robust_tail_probability, 0.75, info = whu)
  expect_match(ru$summary$robust_quantile_source, "_post_q0_75$", info = whu)
  expect_false(isTRUE(ru$summary$robustness_quantile_is_normal_approximation), info = whu)

  # SAME decrease trait, pure-variance metric: more within-family variance is more
  # opportunity whichever way the trait points, so the orientation stays "maximize"
  # and the conservative tail stays the LOWER one. Mapping trait_direction straight
  # through would have got this backwards.
  pmv <- run_robust(rd, robust_cfg(rd, "pmv", 0.25, direction_file = dirf))
  expect_true(isTRUE(pmv$ok), info = pmv$error_message)
  rv  <- pmv$robust_plan
  whv <- rv$error %||% ""
  expect_null(rv$error, info = whv)
  expect_identical(rv$direction, "maximize", info = whv)
  expect_equal(rv$summary$robust_tail_probability, 0.25, info = whv)
  expect_match(rv$summary$robust_quantile_source, "_post_q0_25$", info = whv)
  expect_false(isTRUE(rv$summary$robustness_quantile_is_normal_approximation), info = whv)
})
