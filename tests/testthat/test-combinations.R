# Backend combination sweep. Skipped unless the backend is installed AND the
# env var NGCD_RUN_COMBINATIONS=1 is set (keeps R CMD check fast).
test_that("combination smoke sweep: core valid combos all succeed", {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the backend combination sweep.")
  skip_if(!backend_available(), "Backend (Rscript + nextgenCrossDesign) not available.")

  res <- run_combination_tests(tempfile("wbcombo"), level = "smoke", random = 60,
                               workers = 2, seed = 7, verbose = FALSE, results_csv = NULL)
  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 20)

  # No UNEXPECTED failures. (Infeasible-plan refusals from extreme random
  # values are expected and allowed.)
  failed <- res[res$unexpected_fail, ]
  expect_equal(nrow(failed), 0,
    info = paste("Unexpected failures:\n",
                 paste(sprintf("%s=%s: %s", failed$varied, failed$value, failed$message), collapse = "\n")))

  # Every varied dimension produced at least one successful run.
  ok_dims <- unique(res$varied[res$ok])
  for (dim in c("trait_value_metric", "optimizer", "progeny", "grm_method",
                "diversity", "prediction_mode", "allocation_method"))
    expect_true(dim %in% ok_dims, info = paste("no successful run for", dim))
})
