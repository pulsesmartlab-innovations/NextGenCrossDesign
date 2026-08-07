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
    map_pos_bp_col = "Position_BP", map_position_unit = "bp",
    prediction_mode = "trait_by_trait", multi_trait_method = "auto",
    trait_value_metric = "pmv", progeny = "DH", parent_type = "inbred",
    duplicate_action = "none",
    run_posterior_prediction = TRUE, posterior_method = "closed_form", n_iter = 12, burn_in = 4,
    n_crosses = 7, max_crosses_per_parent = 4, optimizer = "greedy_local",
    allocation_method = "ocs", use_ocs = TRUE, seed = 1,
    robust_allocation = TRUE, robustness_quantile = 0.25,
    robust_objective = "posterior_quantile")

  cfgp <- file.path(rd, "config.json"); resp <- file.path(rd, "result.json")
  jsonlite::write_json(cfgj, cfgp, auto_unbox = TRUE, null = "null", pretty = TRUE)
  runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                        package = "nextgenCrossWorkbench")
  system2("Rscript", c(runner, cfgp, resp), stdout = FALSE, stderr = FALSE)

  res <- jsonlite::fromJSON(resp, simplifyVector = TRUE)
  expect_true(isTRUE(res$ok))
  rp <- res$robust_plan
  expect_false(is.null(rp))
  expect_null(rp$error)
  expect_equal(nrow(rp$crosses), 7L)
  expect_true(all(c("parent1", "parent2") %in% names(rp$crosses)))
  expect_true(!is.null(rp$summary$robust_objective))
  expect_equal(rp$n_shared_with_standard + rp$n_changed, 7L)
})
