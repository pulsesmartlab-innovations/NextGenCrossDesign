# A check line (benchmark) must never also be a candidate parent. The backend
# hard-errors on any overlap between check_geno and geno rownames, but names
# neither ID; ngcd_check_parent_clash() is the pure frontend-side comparison
# that catches it earlier, with a message that names the offenders.

ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")

test_that("disjoint check/parent IDs are refused nothing (NULL, no clash)", {
  clash <- ng("ngcd_check_parent_clash")
  expect_null(clash(c("CHK_A", "CHK_B"), c("P01", "P02", "P03")))
  expect_null(clash(character(0), c("P01", "P02")))
  expect_null(clash(c("CHK_A"), character(0)))
})

test_that("an overlapping ID is named in the refusal message", {
  clash <- ng("ngcd_check_parent_clash")
  msg <- clash(c("CHK_A", "CONLON"), c("P01", "CONLON", "P03"))
  expect_false(is.null(msg))
  expect_match(msg, "CONLON", fixed = TRUE)
  expect_match(msg, "candidate parent")
})

test_that("more than max_shown clashing IDs are capped with an 'and N more' tail", {
  clash <- ng("ngcd_check_parent_clash")
  ids <- sprintf("SHARED_%02d", 1:8)
  msg <- clash(ids, ids, max_shown = 5L)
  expect_match(msg, "SHARED_01")
  expect_match(msg, "and 3 more")
  expect_false(grepl("SHARED_08", msg, fixed = TRUE))  # beyond the cap, not spelled out
})

test_that("blank/NA IDs never manufacture a false-positive clash", {
  clash <- ng("ngcd_check_parent_clash")
  expect_null(clash(c("", NA_character_), c("", NA_character_, "P01")))
})

# ---- reactive wiring: the demo genotype's own IDs vs. an injected check file ----
library(shiny)
srv_cfg <- function() nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
srv <- function() nextgenCrossWorkbench:::workbench_server(srv_cfg())

test_that("a check ID that duplicates a parent ID is refused, naming that ID", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs())
    # Demo genotype IDs are P01..P10; reuse one of them as a check line.
    rv$data$check_geno <- data.frame(NAME = "P01", SNP_001 = 0, SNP_002 = 2,
                                     stringsAsFactors = FALSE)
    session$setInputs(check_id_col = "NAME")
    msg <- check_id_clash_message()
    expect_false(is.null(msg))
    expect_match(msg, "P01", fixed = TRUE)
  })
})

test_that("a check file whose IDs are disjoint from the parents is never refused", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs())
    rv$data$check_geno <- data.frame(NAME = "CHK_A", SNP_001 = 0, SNP_002 = 2,
                                     stringsAsFactors = FALSE)
    session$setInputs(check_id_col = "NAME")
    expect_null(check_id_clash_message())
  })
})

test_that("no check file at all is never refused (the check feature is optional)", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs())
    expect_null(rv$data$check_geno)
    expect_null(check_id_clash_message())
    expect_true(data_ready())   # the no-check path stays fully functional
  })
})
