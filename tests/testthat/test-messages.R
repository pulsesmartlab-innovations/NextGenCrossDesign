# Backend advisory/error messages must be presented to breeders professionally:
# developer notices hidden, known advisories given friendly titles, unknown
# messages preserved. (ngcd_humanize_message / ngcd_advisory_ui)

test_that("humanize hides developer notices and friendlies known advisories", {
  hm <- nextgenCrossWorkbench:::ngcd_humanize_message

  dep <- hm("`assume_inbred` is deprecated; use parent_type = 'inbred'/'dh'/'ril'.")
  expect_true(isTRUE(dep$hide))                       # deprecations never shown to breeders

  ril <- hm(paste("parent_type = 'ril': treating 1 parent(s) as RILs with residual",
                  "heterozygosity (expected for finite selfing). ... biased low ..."))
  expect_false(ril$hide)
  expect_match(ril$title, "Conservative")
  expect_false(grepl("parent_type =", ril$body))      # raw technical wording not surfaced

  het <- hm(paste("3 / 30 parents carry heterozygous loci beyond tolerance, but parent_type =",
                  "'dh' declares fully fixed lines. ... BLOCKED"))
  expect_false(het$hide)
  expect_match(het$title, "Heterozygous")
  expect_match(het$body, "RIL")                        # actionable guidance

  unk <- hm("some unexpected backend condition")
  expect_false(unk$hide)
  expect_equal(unk$title, "Notice")
  expect_match(unk$body, "unexpected")                 # unknown message preserved verbatim
})

test_that("advisory UI consolidates and drops developer-only sets", {
  aui <- nextgenCrossWorkbench:::ngcd_advisory_ui

  # only a deprecation -> nothing worth showing the breeder
  expect_null(aui(list("assume_inbred is deprecated; use parent_type.")))

  # a real advisory -> a single rendered callout
  ui <- aui(list(paste("parent_type = 'ril': ... residual heterozygosity ... ril ...")))
  expect_false(is.null(ui))
  html <- as.character(ui)
  expect_match(html, "Conservative variance estimate")
  expect_match(html, "advisor")                        # the "N advisories" heading
})
