test_that("build_trait_checks emits trait/check/direction and no basis", {
  out <- ngcd_build_trait_checks(
    traits = c("yield", "matur"),
    checks = list(yield = "CHK_A", matur = "CHK_B"),
    directions = list(yield = "auto", matur = "above"))
  expect_equal(nrow(out), 2L)
  expect_setequal(names(out), c("trait", "check", "direction"))
  expect_false("basis" %in% names(out))
  expect_true(is.na(out$direction[out$trait == "yield"]))   # auto -> NA, backend resolves it
  expect_equal(out$direction[out$trait == "matur"], "above")
})

test_that("traits with no check chosen are dropped, and all-empty gives NULL", {
  out <- ngcd_build_trait_checks("yield", list(yield = ""), list(yield = "auto"))
  expect_null(out)
  out2 <- ngcd_build_trait_checks(c("yield", "matur"),
                                  list(yield = "CHK_A", matur = ""),
                                  list(yield = "auto", matur = "auto"))
  expect_equal(nrow(out2), 1L)
  expect_equal(out2$check, "CHK_A")
})
