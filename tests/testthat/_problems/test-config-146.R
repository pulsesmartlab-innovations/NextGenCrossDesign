# Extracted from test-config.R:146

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "nextgenCrossWorkbench", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
blocker <- tempfile("blocker")
file.create(blocker)
bad <- file.path(blocker, "nope")
withr::with_envvar(c(NGCD_DATA_DIR = bad), {
    cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
    b <- nextgenCrossWorkbench:::ngcd_check_backend(cfg)
    expect_true(any(grepl("not writable", b$messages, fixed = TRUE)))
  })
