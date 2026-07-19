test_that("two run dirs in the same second are distinct and both exist", {
  cfg <- list(runs_dir = tempfile("runs")); dir.create(cfg$runs_dir)
  a <- nextgenCrossWorkbench:::ngcd_new_run_dir(cfg, label = "same_label")
  b <- nextgenCrossWorkbench:::ngcd_new_run_dir(cfg, label = "same_label")
  expect_true(dir.exists(a))
  expect_true(dir.exists(b))
  expect_false(identical(a, b))
})

test_that("run dir basename keeps the readable timestamp+slug prefix", {
  cfg <- list(runs_dir = tempfile("runs")); dir.create(cfg$runs_dir)
  d <- nextgenCrossWorkbench:::ngcd_new_run_dir(cfg, label = "trait_by_trait")
  expect_match(basename(d), "^[0-9]{8}-[0-9]{6}_trait_by_trait_")
})

test_that("a NULL label falls back to 'run'", {
  cfg <- list(runs_dir = tempfile("runs")); dir.create(cfg$runs_dir)
  d <- nextgenCrossWorkbench:::ngcd_new_run_dir(cfg, label = NULL)
  expect_match(basename(d), "^[0-9]{8}-[0-9]{6}_run_")
})

test_that("ngcd_prune_runs keeps the newest N and removes older ones", {
  runs <- tempfile("runs"); dir.create(runs)
  cfg <- list(runs_dir = runs, keep_runs = 3L)
  for (i in 1:5) {
    d <- file.path(runs, sprintf("run_%02d", i)); dir.create(d)
    Sys.setFileTime(d, Sys.time() + i)   # ascending mtime: run_05 newest
  }
  removed <- nextgenCrossWorkbench:::ngcd_prune_runs(cfg)
  expect_equal(removed, 2L)
  left <- sort(basename(list.dirs(runs, recursive = FALSE)))
  expect_equal(left, c("run_03", "run_04", "run_05"))
})

test_that("keep_runs = 0 disables pruning", {
  runs <- tempfile("runs"); dir.create(runs)
  cfg <- list(runs_dir = runs, keep_runs = 0L)
  for (i in 1:4) dir.create(file.path(runs, sprintf("run_%02d", i)))
  expect_equal(nextgenCrossWorkbench:::ngcd_prune_runs(cfg), 0L)
  expect_length(list.dirs(runs, recursive = FALSE), 4L)
})

test_that("new run dir triggers pruning and is itself never pruned", {
  runs <- tempfile("runs"); dir.create(runs)
  cfg <- list(runs_dir = runs, keep_runs = 2L)
  for (i in 1:3) {
    d <- file.path(runs, sprintf("old_%02d", i)); dir.create(d)
    Sys.setFileTime(d, Sys.time() - 100 + i)   # all older than the new one
  }
  fresh <- nextgenCrossWorkbench:::ngcd_new_run_dir(cfg, label = "x")
  expect_true(dir.exists(fresh))
  expect_length(list.dirs(runs, recursive = FALSE), 2L)
})

test_that("NGCD_KEEP_RUNS is an integer config override", {
  withr::with_envvar(c(NGCD_KEEP_RUNS = "5"), {
    cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
    expect_equal(suppressWarnings(as.integer(cfg$keep_runs)), 5L)
  })
})
