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
    # NOTE (unresolved, reported not papered over): 0.25 is the app's OWN default and the
    # midpoint of its 0.05-0.50 slider, but ng_optimize_robust_mating_plan() can only serve
    # a quantile that is an EXACT cached posterior tail -- (1 - ci_level)/2 = 0.025 or
    # 0.975 at ng_posterior_cross_predict()'s ci_level = 0.95, which
    # ng_run_cross_prediction() does not expose. Anything else needs
    # allow_normal_approximation = TRUE, which the frontend never sends. So no value this
    # slider can produce yields a robust plan today, and the assertions below fail on
    # rp$error. The backend guard is correct (it refuses to fabricate a quantile the draws
    # do not support); the frontend is what needs a product decision. Left as a live,
    # loudly-reported failure on purpose -- do NOT "fix" it by writing 0.025 here, which
    # would green the test on a config the UI cannot build.
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
