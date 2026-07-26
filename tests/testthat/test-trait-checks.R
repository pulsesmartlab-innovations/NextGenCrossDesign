ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")

test_that("ngcd_diag_trait_check reports flags/exclusions and not-evaluable", {
  res <- list(trait_check_diagnostics = list(
    n_flagged = 3L, n_excluded = 2L, n_not_evaluable = 1L,
    active = data.frame(trait = c("yield","protein"), check = c("Ck1","Ck2"),
                        reject_if = c("below","above"), basis = c("gebv","phenotype"))))
  items <- ng("ngcd_diag_trait_check")(res)
  expect_true(length(items) >= 1)
  expect_true(any(vapply(items, function(x) grepl("check", x$title, ignore.case = TRUE), logical(1))))
})
test_that("ngcd_diag_trait_check no-ops when absent", {
  expect_length(ng("ngcd_diag_trait_check")(list()), 0)
})
