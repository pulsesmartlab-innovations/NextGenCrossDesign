# Extracted from test-config.R:3

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "nextgenCrossWorkbench", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
expect_equal(cfg$rscript_path, "Rscript")
