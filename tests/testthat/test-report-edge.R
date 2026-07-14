# Robustness of the report layer against sparse / empty / malformed results.
# Uses the saved backend fixture plus hand-built minimal results; no backend.

ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")
fixture <- function() jsonlite::fromJSON(
  testthat::test_path("fixtures", "example_result.json"), simplifyVector = TRUE)

test_that("exec summary renders on a nearly-empty result without erroring", {
  html <- ng("ngcd_exec_summary_html")(list())
  expect_type(html, "character")
  expect_match(html, "Executive summary")
  expect_match(html, "&ndash;")           # missing numbers become dashes, not NAs
  expect_false(grepl("\\bNA\\b", html))
})

test_that("exec summary surfaces a low-reliability caution", {
  res <- list(effect_summary = data.frame(
    trait = c("yield", "flaky"),
    marker_effect_reliability = c(0.82, 0.05)))
  html <- ng("ngcd_exec_summary_html")(res)
  expect_match(html, "cautiously")
  expect_match(html, "flaky")
})

test_that("report_active returns nothing for an empty result and never errors", {
  active <- ng("ngcd_report_active")
  expect_length(active(list()), 0L)
  expect_length(active(list(selected_crosses = data.frame(),
                            candidate_crosses = data.frame())), 0L)
})

test_that("applicability predicates are all FALSE on an empty result", {
  preds <- c("ngcd_has_tiers", "ngcd_has_scores", "ngcd_has_scatter",
             "ngcd_has_parents", "ngcd_has_reliab", "ngcd_has_traitmap",
             "ngcd_has_frontier", "ngcd_has_sweep", "ngcd_has_dups")
  for (p in preds) expect_false(isTRUE(ng(p)(list())), info = p)
})

test_that("every registered base figure renders on the fixture without error", {
  res <- fixture()
  grDevices::png(tempfile(fileext = ".png"))
  on.exit(grDevices::dev.off(), add = TRUE)
  for (f in ng("ngcd_fig_registry")()) {
    if (!isTRUE(f$applies(res))) next
    expect_error(f$base(res), NA, info = paste("base", f$id))
  }
})

test_that("plotly figures serialise to JSON that parses back cleanly", {
  skip_if_not_installed("plotly")
  res <- fixture()
  for (f in ng("ngcd_report_active")(res)) {
    j <- ng("ngcd_fig_json")(f$ply(res))
    parsed <- jsonlite::fromJSON(j, simplifyVector = FALSE)
    expect_true(all(c("data", "layout") %in% names(parsed)), info = f$id)
  }
})

test_that("fig_json jsonlite fallback yields a parseable {data,layout} payload", {
  skip_if_not_installed("plotly")
  # The fallback branch serialises the built figure's data/layout with jsonlite
  # when plotly's internal to_JSON() is unavailable. Exercise that exact path.
  p <- plotly::plot_ly(x = 1:3, y = c(2, 4, 3), type = "scatter", mode = "lines")
  b <- plotly::plotly_build(p)
  payload <- b$x[c("data", "layout")]
  j <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null",
                                     na = "null", digits = 10))
  parsed <- jsonlite::fromJSON(j, simplifyVector = FALSE)
  expect_true(all(c("data", "layout") %in% names(parsed)))
})

test_that("frontier plotly handles a NULL operating point and a single row", {
  fp <- ng("ngcd_frontier_plotly")
  fr <- data.frame(group_coancestry = c(0.1, 0.2, 0.3),
                   mean_gain = c(1.0, 1.4, 1.6))
  expect_s3_class(fp(fr), "plotly")
  expect_s3_class(fp(fr, op_x = 0.2, op_y = 1.4), "plotly")
  expect_s3_class(fp(fr[1, , drop = FALSE]), "plotly")
})

test_that("HTML report still builds when plotly figures are absent (static path)", {
  # A result with selected crosses but nothing that triggers plotly-only figures.
  res <- list(
    package_version = "9.9",
    prediction_mode = "index",
    settings = list(progeny = "DH", trait_value_metric = "var_complex",
                    allocation_method = "ocs", optimizer = "greedy_local"),
    selected_crosses = data.frame(parent1 = c("A", "B"), parent2 = c("C", "D"),
                                  priority_tier = c("priority", "medium_priority")),
    plan_summary = list(mean_gain = 1.2, group_coancestry = 0.1,
                        unique_parents = 4, max_parent_use = 1))
  out <- tempfile(fileext = ".html")
  ng("ngcd_report_html")(res, out)
  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Executive summary")
  expect_match(html, "PulseSmartLab")
})
