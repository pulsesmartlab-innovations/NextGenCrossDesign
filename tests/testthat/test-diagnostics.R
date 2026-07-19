# The tuning-diagnostics engine: each procedure should explain itself and name
# a parameter to change. No backend needed (hand-built + fixture results).

ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")
fixture <- function() jsonlite::fromJSON(
  testthat::test_path("fixtures", "example_result.json"), simplifyVector = TRUE)

sweep <- function(recommended_k, k_range = 3:15) {
  n <- length(k_range)
  list(recommended_k = as.integer(recommended_k), criterion = "elbow_relative",
       k_range = k_range,
       curve = data.frame(K = k_range,
         total_gain = cumsum(seq(1, 0.3, length.out = n)),
         mean_gain = seq(1.5, 0.9, length.out = n),
         group_coancestry = seq(0.4, 0.1, length.out = n),
         unique_parents = k_range,
         marginal_gain = c(NA, seq(1, 0.3, length.out = n - 1)),
         relative_marginal = c(NA, seq(1, 0.3, length.out = n - 1)),
         Ne_estimate = seq(1, 6, length.out = n)))
}
areas <- function(items) vapply(items, function(x) x$area, character(1))
sev_of <- function(items, area) {
  hit <- Filter(function(x) x$area == area, items)
  if (!length(hit)) NA_character_ else hit[[1]]$severity
}

test_that("empty result yields no diagnostics and a friendly HTML placeholder", {
  expect_length(ng("ngcd_diagnostics")(list()), 0L)
  expect_length(ng("ngcd_diagnostics")(NULL), 0L)  # NULL -> empty list
  h <- ng("ngcd_diagnostics_html")(list())
  expect_match(h, "No tuning diagnostics")
})

test_that("a boundary-hit cross-number sweep is flagged as a warning to widen the range", {
  r <- list(cross_number_sweep = sweep(15, 3:15))
  d <- ng("ngcd_diag_cross_number")(r)
  top <- Filter(function(x) grepl("TOP", x$title), d)
  expect_true(length(top) >= 1)
  expect_equal(top[[1]]$severity, "warn")
  expect_match(top[[1]]$suggestion, "K max")
})

test_that("a bottom-of-range recommendation is a note to lower K min", {
  r <- list(cross_number_sweep = sweep(3, 3:15))
  d <- ng("ngcd_diag_cross_number")(r)
  bot <- Filter(function(x) grepl("BOTTOM", x$title), d)
  expect_true(length(bot) >= 1)
  expect_equal(bot[[1]]$severity, "note")
  expect_match(bot[[1]]$suggestion, "K min")
})

test_that("an interior elbow is reported OK with the marginal-gain flatness and a trade-off", {
  r <- list(cross_number_sweep = sweep(8, 3:15))
  d <- ng("ngcd_diag_cross_number")(r)
  interior <- Filter(function(x) grepl("interior elbow", x$title), d)
  expect_true(length(interior) >= 1)
  expect_equal(interior[[1]]$severity, "ok")
  # a trade-off note quantifying gain given up vs the largest plan
  tradeoff <- Filter(function(x) grepl("Trade-off", x$title), d)
  expect_true(length(tradeoff) >= 1)
  expect_match(tradeoff[[1]]$detail, "gives up")
})

test_that("ne_target criterion reports Ne and how to steer it", {
  sw <- sweep(8); sw$criterion <- "ne_target"
  d <- ng("ngcd_diag_cross_number")(list(cross_number_sweep = sw))
  ne <- Filter(function(x) grepl("Ne", x$title), d)
  expect_true(length(ne) >= 1)
  expect_match(ne[[1]]$suggestion, "Minimum Ne")
})

test_that("binding allocation constraints are detected (kinship, parent-use, unique-parents)", {
  r <- list(
    plan_summary = list(max_parent_use = 3, unique_parents = 4),
    settings = list(max_pair_kinship = 0.2, max_uses_per_parent = 3, min_unique_parents = 4),
    selected_crosses = data.frame(parent1 = "A", parent2 = "B", pair_kinship = 0.199))
  d <- ng("ngcd_diag_allocation")(r)
  a <- areas(d)
  expect_true(all(a == "allocation"))
  expect_true(any(grepl("kinship cap is binding", vapply(d, `[[`, "", "title"))))
  expect_true(any(grepl("parent-use cap is binding", vapply(d, `[[`, "", "title"))))
  expect_true(any(grepl("unique-parents floor is binding", vapply(d, `[[`, "", "title"))))
})

test_that("a slack kinship cap produces no binding warning", {
  r <- list(plan_summary = list(),
            settings = list(max_pair_kinship = 0.9),
            selected_crosses = data.frame(parent1 = "A", parent2 = "B", pair_kinship = 0.1))
  d <- ng("ngcd_diag_allocation")(r)
  expect_false(any(grepl("kinship cap is binding", vapply(d, `[[`, "", "title"))))
})

test_that("robust plan diagnostics distinguish agreement, change, and error", {
  agree <- ng("ngcd_diag_robust")(list(robust_plan = list(n_changed = 0L, n_crosses = 10L)))
  expect_equal(agree[[1]]$severity, "ok")
  changed <- ng("ngcd_diag_robust")(list(robust_plan = list(
    n_changed = 4L, n_crosses = 10L, summary = list(robustness_quantile = 0.25))))
  expect_equal(changed[[1]]$severity, "note")
  expect_match(changed[[1]]$suggestion, "robustness quantile")
  err <- ng("ngcd_diag_robust")(list(robust_plan = list(error = "no posterior scores")))
  expect_equal(err[[1]]$severity, "warn")
})

test_that("low trait reliability is flagged with a de-weight / drop suggestion", {
  r <- list(effect_summary = data.frame(
    trait = c("yield", "disease"), marker_effect_reliability = c(0.7, 0.002)),
    settings = list(min_effect_reliability = 0.35))
  d <- ng("ngcd_diag_reliability")(r)
  expect_length(d, 1L)
  expect_equal(d[[1]]$severity, "warn")
  expect_match(d[[1]]$title, "disease")
  expect_match(d[[1]]$suggestion, "reliability")
})

test_that("QC blockers and warnings surface at the right severity", {
  blk <- ng("ngcd_diag_qc")(list(qc = list(counts = list(blockers = 2, warnings = 0))))
  expect_equal(blk[[1]]$severity, "warn")
  wrn <- ng("ngcd_diag_qc")(list(qc = list(counts = list(blockers = 0, warnings = 3))))
  expect_equal(wrn[[1]]$severity, "note")
  expect_length(ng("ngcd_diag_qc")(list(qc = list(counts = list(blockers = 0, warnings = 0)))), 0L)
})

test_that("diagnostics are ordered warnings-first and render to labelled HTML", {
  r <- fixture()                       # fixture has disease reliability ~0.002 (a warn)
  r$cross_number_sweep <- sweep(15)    # boundary warn
  items <- ng("ngcd_diagnostics")(r)
  sev <- vapply(items, function(x) x$severity, character(1))
  rank <- c(warn = 1, note = 2, ok = 3)[sev]
  expect_false(is.unsorted(rank))      # non-decreasing severity rank
  html <- ng("ngcd_diagnostics_html")(r)
  expect_match(html, "What to change")
  expect_match(html, "ngcd-diagnostics")
})

test_that("the full result adds diagnostics without breaking the report", {
  r <- fixture()
  out <- tempfile(fileext = ".html")
  ng("ngcd_report_html")(r, out)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Diagnostics &amp; tuning")
  expect_match(html, "id='diagnostics'")
})
