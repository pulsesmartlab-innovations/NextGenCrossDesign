# Polyploid design workflow: the runner's polyploid_design path drives
# ng_polyploid_design_crosses() on a dosage matrix + single-trait phenotype and
# returns a poly plan + summary + QC. Backend-gated.

test_that("polyploid design runner returns a plan, summary and QC", {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the backend polyploid check.")
  skip_if(!backend_available(), "Backend not available.")
  skip_if_not(exists("ng_polyploid_design_crosses", where = asNamespace("nextgenCrossDesign")),
              "Backend lacks ng_polyploid_design_crosses.")

  cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wbpoly"))
  pd  <- nextgenCrossWorkbench:::ngcd_poly_demo_files(cfg)
  expect_true(file.exists(pd$genotype) && file.exists(pd$phenotype))

  rd   <- tempfile("run"); dir.create(rd, recursive = TRUE)
  cfgj <- list(schema = "ng_run_config.v1", workflow = "polyploid_design",
    genotype_file = pd$genotype, phenotype_file = pd$phenotype,
    genotype_id_col = "NAME", phenotype_id_col = "NAME", poly_trait_col = "yield",
    ploidy = 4, n_crosses = 8, max_crosses_per_parent = 4,
    dominance = TRUE, gain = "usefulness", double_reduction = 0.08,
    grm_method = "vanraden", selection_prop = 0.1, run_qc = TRUE,
    method = "greedy_local", strategy = "balanced", seed = 1)
  cfgp <- file.path(rd, "config.json"); resp <- file.path(rd, "result.json")
  jsonlite::write_json(cfgj, cfgp, auto_unbox = TRUE, null = "null", pretty = TRUE)
  runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                        package = "nextgenCrossWorkbench")
  system2("Rscript", c(runner, cfgp, resp), stdout = FALSE, stderr = FALSE)

  res <- jsonlite::fromJSON(resp, simplifyVector = TRUE)
  expect_true(isTRUE(res$ok))
  expect_true(isTRUE(res$poly_design))
  expect_equal(as.integer(res$ploidy), 4L)
  expect_equal(nrow(res$selected_crosses), 8L)
  # dominance columns present
  expect_true(all(c("parent1", "parent2", "cross_usefulness", "heterosis") %in%
                    names(res$poly_plan$crosses)))
  # QC + summary
  expect_true(!is.null(res$poly_plan$qc$n_markers_clean))
  expect_true(is.finite(res$plan_summary$mean_gain))
  # a gain-diversity frontier comes along for free
  expect_true(is.data.frame(res$plan_summary$frontier))
})
