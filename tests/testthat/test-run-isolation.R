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
