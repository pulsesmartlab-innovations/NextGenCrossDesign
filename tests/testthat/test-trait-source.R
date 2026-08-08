# The traits a user selects come from the uploaded PHENOTYPE file's own columns
# (not the trait-direction file). ngcd_trait_columns() is the pure source of that
# set; the direction file only annotates increase/decrease for the chosen traits.

test_that("ngcd_trait_columns returns the phenotype's non-ID columns", {
  tc <- nextgenCrossWorkbench:::ngcd_trait_columns
  ph <- data.frame(NAME = c("P1", "P2"), yield = c(1, 2), protein = c(3, 4),
                   disease = c(5, 6), check.names = FALSE)
  expect_equal(tc(ph, "NAME"), c("yield", "protein", "disease"))
  # ID auto-detected when not given
  expect_equal(tc(ph), c("yield", "protein", "disease"))
  # a different ID column is excluded
  ph2 <- data.frame(line = c("a", "b"), y = c(1, 2))
  expect_equal(tc(ph2, "line"), "y")
})

test_that("ngcd_import_state reports per-file status", {
  st <- nextgenCrossWorkbench:::ngcd_import_state
  expect_equal(st(NULL, NULL)$state, "empty")
  expect_equal(st(list(name = "x.csv"), NULL)$state, "error")          # uploaded but unreadable
  expect_equal(st(list(name = "x.csv"), data.frame(a = 1))$state, "warn")   # single column
  ok <- st(list(name = "x.csv"), data.frame(NAME = "P1", y = 1))
  expect_equal(ok$state, "ok")
  expect_match(ok$label, "1 rows × 2 columns")
})

test_that("ngcd_trait_columns is robust to empty / NULL / no-trait input", {
  tc <- nextgenCrossWorkbench:::ngcd_trait_columns
  expect_equal(tc(NULL), character(0))
  expect_equal(tc(data.frame()), character(0))
  expect_equal(tc(data.frame(NAME = "P1")), character(0))   # only an ID column
})
