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

test_that("data_dir defaults under tempdir and derives the mutable areas", {
  cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
  expect_true(startsWith(normalizePath(cfg$data_dir, mustWork = FALSE),
                         normalizePath(tempdir(), mustWork = FALSE)))
  expect_equal(cfg$runs_dir,    file.path(cfg$data_dir, "runs"))
  expect_equal(cfg$report_dir,  file.path(cfg$data_dir, "_report"))
  expect_equal(cfg$presets_dir, file.path(cfg$data_dir, "presets"))
  expect_true(dir.exists(cfg$runs_dir))
  expect_true(dir.exists(cfg$report_dir))
  expect_true(dir.exists(cfg$presets_dir))
  expect_null(cfg$data_dir_warning)
})

test_that("NGCD_DATA_DIR override is honored (not clobbered after the loop)", {
  base <- tempfile("ngcd-data")
  withr::with_envvar(c(NGCD_DATA_DIR = base), {
    cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
    expect_equal(normalizePath(cfg$data_dir, mustWork = FALSE),
                 normalizePath(base, mustWork = FALSE))
    expect_equal(cfg$runs_dir, file.path(cfg$data_dir, "runs"))
  })
})

test_that("an unwritable data_dir falls back to tempdir with a warning", {
  blocker <- tempfile("blocker"); file.create(blocker)
  bad <- file.path(blocker, "cannot", "exist")   # parent is a file, not a dir
  withr::with_envvar(c(NGCD_DATA_DIR = bad), {
    cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
    expect_false(is.null(cfg$data_dir_warning))
    expect_true(dir.exists(cfg$data_dir))
    expect_true(startsWith(normalizePath(cfg$data_dir, mustWork = FALSE),
                           normalizePath(tempdir(), mustWork = FALSE)))
  })
})

test_that("ngcd_dir_writable probes with a real file", {
  d <- tempfile("wtest"); dir.create(d)
  expect_true(nextgenCrossWorkbench:::ngcd_dir_writable(d))
  # A path whose parent is a regular file cannot be created -> not writable.
  blocker <- tempfile("blk"); file.create(blocker)
  expect_false(nextgenCrossWorkbench:::ngcd_dir_writable(file.path(blocker, "x")))
})

test_that("report_dir and presets_dir live under data_dir, not work_dir", {
  wd <- tempfile("wb"); dir.create(wd)
  cfg <- nextgenCrossWorkbench:::ngcd_load_config(wd)
  expect_false(startsWith(normalizePath(cfg$report_dir, mustWork = FALSE),
                          normalizePath(wd, mustWork = FALSE)))
  expect_false(startsWith(normalizePath(cfg$presets_dir, mustWork = FALSE),
                          normalizePath(wd, mustWork = FALSE)))
  expect_true(startsWith(normalizePath(cfg$report_dir, mustWork = FALSE),
                         normalizePath(cfg$data_dir, mustWork = FALSE)))
})

test_that("check_backend surfaces the data_dir fallback warning", {
  blocker <- tempfile("blocker"); file.create(blocker)
  bad <- file.path(blocker, "nope")
  withr::with_envvar(c(NGCD_DATA_DIR = bad), {
    cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
    b <- nextgenCrossWorkbench:::ngcd_check_backend(cfg)
    expect_true(any(grepl("not writable", b$messages, fixed = TRUE)))
  })
})
