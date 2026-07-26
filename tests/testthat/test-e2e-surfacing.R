# End-to-end verification: single-trait run with trait_checks + use_ocs=TRUE against the
# real backend produces the risk/portfolio and trait-check surfacing added for the workbench
# (per-cross risk_bin/portfolio_profile/cross_level/cross_upside/threshold_* columns, the
# priority_risk_diagnostics + trait_check_diagnostics blocks) and that the report-side helpers
# (ngcd_portfolio_plotly, ngcd_diagnostics) consume them without error.
# Backend-gated (runs the real prediction) and requires backend >= 0.14.0, which is when these
# fields were added.

test_that("single-trait run with trait_checks surfaces risk/portfolio + trait-check fields", {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the backend e2e surfacing check.")
  skip_if(!backend_available(), "Backend not available.")
  skip_if_not(packageVersion("nextgenCrossDesign") >= "0.14.0",
              "Needs backend >= 0.14.0 for risk/portfolio + trait-check surfacing.")

  cfg  <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wbe2e"))
  demo <- nextgenCrossWorkbench:::ngcd_demo_files(cfg)
  rd   <- tempfile("run"); dir.create(rd, recursive = TRUE)

  cfgj <- list(schema = "ng_run_config.v1",
    phenotype_file = demo$phenotype, genotype_file = demo$genotype,
    map_file = demo$map, direction_file = demo$direction,
    phenotype_id_col = "NAME", genotype_id_col = "NAME", direction_trait_col = "Trait",
    direction_column_col = "Trait", direction_direction_col = "Selection_direction",
    map_marker_col = "SNP_code", map_chr_col = "Chromosome",
    map_pos_bp_col = "Position_BP", map_position_unit = "bp",
    prediction_mode = "trait_by_trait", traits_to_use = list("yield"),
    trait_value_metric = "var_complex", uc_variance_source = "pmv",
    progeny = "DH", assume_inbred = TRUE, duplicate_action = "none",
    n_crosses = 8, max_crosses_per_parent = 4, optimizer = "greedy_local",
    allocation_method = "ocs", use_ocs = TRUE, seed = 1,
    # one trait-check line: reject crosses whose yield mid-parent is on the wrong side
    # of elite parent P01 (direction left NULL -> resolved from trait_direction).
    trait_checks = list(list(trait = "yield", check = "P01", direction = NULL, basis = "gebv")))

  cfgp <- file.path(rd, "config.json"); resp <- file.path(rd, "result.json")
  jsonlite::write_json(cfgj, cfgp, auto_unbox = TRUE, null = "null", pretty = TRUE)
  runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                        package = "nextgenCrossWorkbench")
  system2("Rscript", c(runner, cfgp, resp), stdout = FALSE, stderr = FALSE)

  res <- jsonlite::fromJSON(resp, simplifyVector = TRUE)
  expect_true(isTRUE(res$ok))

  sc <- res$selected_crosses
  expect_true(is.data.frame(sc) && nrow(sc) > 0)

  # single-trait risk/portfolio columns
  expect_true(all(c("cross_level", "cross_upside", "risk_bin", "portfolio_profile")
                  %in% names(sc)))

  # trait-check columns: threshold_ok/threshold_violation + a per-trait "<trait>_check_*" column
  expect_true(all(c("threshold_ok", "threshold_violation") %in% names(sc)))
  expect_true(any(grepl("^yield_check_", names(sc))))

  # top-level diagnostics blocks
  expect_false(is.null(res$priority_risk_diagnostics))
  expect_false(is.null(res$trait_check_diagnostics))

  # report-side helpers consume the live result without error
  p <- nextgenCrossWorkbench:::ngcd_portfolio_plotly(res)
  expect_true(inherits(p, "plotly") || inherits(p, "htmlwidget"))

  d <- nextgenCrossWorkbench:::ngcd_diagnostics(res)
  areas <- vapply(d, function(x) x$area, character(1))
  expect_true("priority_risk" %in% areas)
  expect_true("portfolio" %in% areas)
  expect_true("trait_check" %in% areas)
})
