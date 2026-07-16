test_that("config loads defaults and resolves package resources", {
  cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
  expect_equal(cfg$rscript_path, "Rscript")
  expect_equal(cfg$required_backend_version, "0.4.0")
  expect_true(file.exists(cfg$runner_script))
  expect_true(dir.exists(cfg$demo_data_dir))
  expect_true(dir.exists(cfg$runs_dir))
})

test_that("NGCD_ env vars override config", {
  old <- Sys.getenv("NGCD_RSCRIPT_PATH", unset = NA)
  Sys.setenv(NGCD_RSCRIPT_PATH = "/custom/Rscript")
  on.exit(if (is.na(old)) Sys.unsetenv("NGCD_RSCRIPT_PATH") else Sys.setenv(NGCD_RSCRIPT_PATH = old))
  cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
  expect_equal(cfg$rscript_path, "/custom/Rscript")
})

test_that("init_workbench_dir writes a config template and runs dir", {
  d <- tempfile("wbinit"); dir.create(d)
  cfgp <- init_workbench_dir(d)
  expect_true(file.exists(cfgp))
  expect_true(dir.exists(file.path(d, "runs")))
  expect_match(paste(readLines(cfgp), collapse = "\n"), "rscript_path")
})

test_that("workbench_app returns a shiny app object", {
  a <- workbench_app(tempfile("wbapp"))
  expect_s3_class(a, "shiny.appobj")
})

test_that("run_workbench() returns the app object instead of nesting runApp()", {
  # RStudio's "Run App" sources the file inside its own runApp(); a
  # run_workbench() call there must NOT call runApp() again (which errors
  # "Can't call runApp() from within runApp()") - it returns the app object.
  runApp <- function(expr) force(expr)          # stand-in for the outer runApp
  res <- runApp(run_workbench(tempfile("wbnest")))
  expect_s3_class(res, "shiny.appobj")
})

test_that("developer_mode defaults off and gates Setup + settings tools", {
  d <- tempfile("wbdev"); dir.create(d); init_workbench_dir(d)
  cfg <- nextgenCrossWorkbench:::ngcd_load_config(d)
  expect_false(isTRUE(cfg$developer_mode))          # off by default

  h_user <- suppressWarnings(as.character(nextgenCrossWorkbench:::workbench_ui(cfg, dev = FALSE)))
  h_dev  <- suppressWarnings(as.character(nextgenCrossWorkbench:::workbench_ui(cfg, dev = TRUE)))

  # end-user view hides Setup and the Save/Load-settings JSON tools
  expect_false(grepl("setup_status", h_user))
  expect_false(grepl("Save / load settings", h_user))
  expect_false(grepl("download_settings", h_user))
  expect_false(grepl("upload_settings", h_user))
  # developer view shows them
  expect_true(grepl("setup_status", h_dev))
  expect_true(grepl("Save / load settings", h_dev))
  expect_true(grepl("download_settings", h_dev))
  # the analysis workflow is present in both
  expect_true(grepl("NextGenCrossDesign", h_user))
})

test_that("developer_mode coerces a truthy env-var string", {
  d <- tempfile("wbdev2"); dir.create(d); init_workbench_dir(d)
  withr::with_envvar(c(NGCD_DEVELOPER_MODE = "true"), {
    cfg <- nextgenCrossWorkbench:::ngcd_load_config(d)
    expect_true(isTRUE(cfg$developer_mode))
  })
})
