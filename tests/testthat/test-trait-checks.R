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

test_that("ngcd_build_trait_checks assembles a spec, dropping traits with no check", {
  s <- ng("ngcd_build_trait_checks")(traits = c("yield","protein","oil"),
                               checks = list(yield = "Ck1", protein = "", oil = "Ck3"),
                               directions = list(yield = "auto", protein = "auto", oil = "above"),
                               bases = list(yield = "gebv", protein = "gebv", oil = "phenotype"))
  expect_s3_class(s, "data.frame")
  expect_equal(sort(s$trait), c("oil","yield"))            # protein dropped (no check)
  expect_equal(s$check[s$trait == "oil"], "Ck3")
  expect_equal(s$basis[s$trait == "oil"], "phenotype")
  expect_true(is.na(s$direction[s$trait == "yield"]) || s$direction[s$trait == "yield"] == "")  # auto -> unset, backend defaults
  expect_null(ng("ngcd_build_trait_checks")(c("yield"), list(yield = ""), list(yield="auto"), list(yield="gebv")))
})
