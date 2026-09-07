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

test_that("the trait-check panel no longer offers a basis or an exclude toggle", {
  cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
  html <- suppressWarnings(as.character(nextgenCrossWorkbench:::workbench_ui(cfg, dev = FALSE)))
  expect_false(grepl("exclude_threshold_violators", html, fixed = TRUE))
  expect_false(grepl("check_basis", html, fixed = TRUE))
  expect_false(grepl("Comparison basis", html, fixed = TRUE))
  expect_false(grepl("Exclude violating crosses from the plan", html, fixed = TRUE))
  # the check-line copy must describe a reference, not a veto (scoped to the check
  # feature's own copy - the unrelated lethal-allele guard legitimately still
  # "excludes carrier x carrier matings", so a blanket "excludes" grep would be a
  # false positive against that untouched feature)
  expect_false(grepl("Trait-check veto", html, fixed = TRUE))
  expect_false(grepl("excludes) crosses whose mid-parent", html, fixed = TRUE))
})
