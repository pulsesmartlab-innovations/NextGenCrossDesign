# Auto cross-number optimization: the headless runner sweeps K with
# ng_optimize_mating_plan_curve() and returns a recommended K + curve.
# Backend-gated (runs the real prediction twice).

test_that("auto cross-number mode returns a recommended K and a curve", {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the backend cross-number sweep.")
  skip_if(!backend_available(), "Backend not available.")
  skip_if_not(exists("ng_optimize_mating_plan_curve",
                     where = asNamespace("nextgenCrossDesign")),
              "Backend lacks ng_optimize_mating_plan_curve.")

  cfg  <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wbsweep"))
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
    trait_value_metric = "var_complex", progeny = "DH", assume_inbred = TRUE,
    duplicate_action = "none", max_uses_per_parent = 4, optimizer = "greedy_local",
    allocation_method = "ocs", use_ocs = TRUE, seed = 1,
    cross_number_mode = "auto", cross_sweep_k_min = 3, cross_sweep_k_max = 12,
    cross_sweep_k_step = 1, cross_sweep_criterion = "elbow_relative")

  cfgp <- file.path(rd, "config.json"); resp <- file.path(rd, "result.json")
  jsonlite::write_json(cfgj, cfgp, auto_unbox = TRUE, null = "null", pretty = TRUE)
  runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                        package = "nextgenCrossWorkbench")
  system2("Rscript", c(runner, cfgp, resp), stdout = FALSE, stderr = FALSE)

  res <- jsonlite::fromJSON(resp, simplifyVector = TRUE)
  expect_true(isTRUE(res$ok))
  sw <- res$cross_number_sweep
  expect_false(is.null(sw))
  expect_true(is.numeric(sw$recommended_k) && sw$recommended_k >= 3 && sw$recommended_k <= 12)
  expect_gt(nrow(sw$curve), 1L)
  expect_true(all(c("K", "total_gain", "group_coancestry") %in% names(sw$curve)))
  # the final plan is built at the recommended K
  expect_equal(nrow(res$selected_crosses), as.integer(sw$recommended_k))
})
