# Report module: figure registry, applicability filtering, interactive HTML,
# PDF, and the PulseSmartLab logo. Uses a saved backend result (no backend
# needed at test time).

ngcd_read_fixture <- function() {
  f <- testthat::test_path("fixtures", "example_result.json")
  jsonlite::fromJSON(f, simplifyVector = TRUE)
}
ngcd_active_ids <- function(res)
  vapply(nextgenCrossWorkbench:::ngcd_report_active(res), function(f) f$id, character(1))

test_that("cross-number sweep figure appears only when the sweep is present", {
  res <- ngcd_read_fixture()
  expect_false("crossnum" %in% ngcd_active_ids(res))     # fixed-K run: no sweep figure

  res2 <- res
  res2$cross_number_sweep <- list(
    recommended_k = 8L, criterion = "elbow_relative",
    k_range = 3:15,
    curve = data.frame(K = 3:15, total_gain = cumsum(seq(1, 0.2, length.out = 13)),
                       mean_gain = seq(1.5, 0.9, length.out = 13),
                       group_coancestry = seq(0.4, 0.1, length.out = 13),
                       unique_parents = 3:15,
                       Ne_estimate = seq(1, 5, length.out = 13)))
  expect_true(nextgenCrossWorkbench:::ngcd_has_sweep(res2))
  expect_true("crossnum" %in% ngcd_active_ids(res2))
  # exec summary reports the automatic choice
  es <- nextgenCrossWorkbench:::ngcd_exec_summary_html(res2)
  expect_match(es, "automatically")
  expect_match(es, "K = 8")
  if (requireNamespace("plotly", quietly = TRUE)) {
    j <- nextgenCrossWorkbench:::ngcd_fig_json(nextgenCrossWorkbench:::ngcd_ply_cross_number(res2))
    expect_match(j, '"data"')
  }
})

test_that("figure registry entries are well formed", {
  reg <- nextgenCrossWorkbench:::ngcd_fig_registry()
  expect_gt(length(reg), 5)
  for (f in reg) {
    expect_true(all(c("id","title","desc","applies","base","ply") %in% names(f)))
    expect_true(is.function(f$applies) && is.function(f$base) && is.function(f$ply))
  }
  # ids unique
  ids <- vapply(reg, function(f) f$id, character(1))
  expect_equal(length(ids), length(unique(ids)))
})

test_that("only applicable figures are active; frontier & dups toggle correctly", {
  res <- ngcd_read_fixture()
  base_ids <- ngcd_active_ids(res)
  expect_true(all(c("tiers","scoredist","scatter","parents","reliab") %in% base_ids))

  # this fixture has a frontier -> included; drop it -> excluded
  expect_true(nextgenCrossWorkbench:::ngcd_has_frontier(res))
  expect_true("frontier" %in% base_ids)
  r2 <- res; r2$plan_summary$frontier <- NULL
  expect_false("frontier" %in% ngcd_active_ids(r2))

  # no putative duplicates in fixture -> excluded; inject some -> included
  expect_false("dups" %in% base_ids)
  r3 <- res
  r3$qc$putative_duplicates <- list(pairs = data.frame(
    a = c("P01","P02"), b = c("P03","P04"), ibs = c(0.98, 0.95)))
  expect_true("dups" %in% ngcd_active_ids(r3))

  # empty selection -> selection-dependent figures drop out, no error
  r4 <- res; r4$selected_crosses <- data.frame()
  drop_ids <- ngcd_active_ids(r4)
  expect_false("tiers" %in% drop_ids)
  expect_false("parents" %in% drop_ids)
})

test_that("every active plotly figure serialises to render-ready JSON", {
  skip_if_not_installed("plotly")
  res <- ngcd_read_fixture()
  for (f in nextgenCrossWorkbench:::ngcd_report_active(res)) {
    j <- nextgenCrossWorkbench:::ngcd_fig_json(f$ply(res))
    expect_true(is.character(j) && nchar(j) > 20, info = f$id)
    expect_match(j, '"data"')
    expect_match(j, '"layout"')
  }
})

test_that("HTML report is self-contained, interactive, cross-linked, branded", {
  res <- ngcd_read_fixture()
  out <- tempfile(fileext = ".html")
  nextgenCrossWorkbench:::ngcd_report_html(res, out)
  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")

  # branding: PulseSmartLab logo + Dr. Sikiru Atanda credit
  expect_match(html, "PulseSmartLab")
  expect_match(html, "Dr. Sikiru Atanda")
  # cross-linked table of contents + back-to-top anchors
  expect_match(html, "class='toc'")
  expect_match(html, "#fig-tiers")
  expect_match(html, "back to top")
  # a section per active figure
  for (id in ngcd_active_ids(res)) expect_match(html, sprintf("id='fig-%s'", id))

  if (requireNamespace("plotly", quietly = TRUE)) {
    # interactive + self-contained: inlined plotly.js and newPlot calls
    expect_match(html, "Plotly.newPlot")
    expect_true(nchar(html) > 1e6)  # plotly.js inlined
  }
})

test_that("PDF report builds and includes only applicable figures", {
  res <- ngcd_read_fixture()
  out <- tempfile(fileext = ".pdf")
  nextgenCrossWorkbench:::ngcd_report_pdf(res, out)
  expect_true(file.exists(out) && file.size(out) > 2000)
})

test_that("logo helper returns embeddable SVG markup", {
  svg <- nextgenCrossWorkbench:::ngcd_logo_svg()
  expect_match(svg, "<svg")
  expect_match(svg, "PulseSmartLab")
  expect_false(grepl("<\\?xml", svg))  # XML prolog stripped for inline embedding
})

# --- Task 8: per-trait check reference panel in the run report -------------
# A check is a benchmark genotype: reference-only, never crossed, never an
# exclusion. THE UNITS RULE forbids drawing a check line on multi_trait_score
# (an aggregate across traits, and possibly transformed), so this figure is a
# NEW registry entry -- not an addition to the existing "scatter"/"frontier"
# figures, which stay untouched.
ngcd_res_with_check <- function() {
  res <- ngcd_read_fixture()
  res$candidate_crosses$yield_check_value <- 60
  res$candidate_crosses$yield_check_ok <- res$candidate_crosses$yield_mean >= 60
  res$candidate_crosses$yield_p_beat_check <- c(0.9, 0.4)[
    (!res$candidate_crosses$yield_check_ok) + 1L]
  res$trait_check_reference <- list(
    active = data.frame(trait = "yield", check = "CHK_A", reject_if = "below",
                        stringsAsFactors = FALSE),
    values = list(yield = c(CHK_A = 60)),
    diagnostics = list(n_candidates = nrow(res$candidate_crosses)))
  res
}

test_that("the check-panel figure is absent when no check is configured", {
  res <- ngcd_read_fixture()
  expect_null(res$trait_check_reference)
  expect_false(nextgenCrossWorkbench:::ngcd_has_checkpanels(res))
  expect_false("checkpanels" %in% ngcd_active_ids(res))
})

test_that("the check-panel figure is active and renders when a check is configured", {
  res <- ngcd_res_with_check()
  expect_true(nextgenCrossWorkbench:::ngcd_has_checkpanels(res))
  expect_true("checkpanels" %in% ngcd_active_ids(res))

  # base-R (PDF/PNG) figure draws without error
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp); on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(nextgenCrossWorkbench:::ngcd_fig_check_panels(res))

  if (requireNamespace("plotly", quietly = TRUE)) {
    p <- nextgenCrossWorkbench:::ngcd_ply_check_panels(res)
    expect_s3_class(p, "plotly")
    j <- nextgenCrossWorkbench:::ngcd_fig_json(p)
    expect_match(j, '"data"')
  }
})

test_that("check-panel figure registry entry is well formed and reuses the shared chart builder", {
  reg <- nextgenCrossWorkbench:::ngcd_fig_registry()
  ids <- vapply(reg, function(f) f$id, character(1))
  expect_true("checkpanels" %in% ids)
  f <- reg[[match("checkpanels", ids)]]
  expect_true(all(c("id","title","desc","applies","base","ply") %in% names(f)))
  # honesty: the description may not imply a check excludes a cross
  expect_false(grepl("exclud|reject|drop|filter|disqualif", f$desc, ignore.case = TRUE))
})

test_that("HTML report gets a check-panel section only when checks are configured", {
  res_nochk <- ngcd_read_fixture()
  out1 <- tempfile(fileext = ".html")
  nextgenCrossWorkbench:::ngcd_report_html(res_nochk, out1)
  html1 <- paste(readLines(out1, warn = FALSE), collapse = "\n")
  expect_false(grepl("id='fig-checkpanels'", html1, fixed = TRUE))

  res_chk <- ngcd_res_with_check()
  out2 <- tempfile(fileext = ".html")
  nextgenCrossWorkbench:::ngcd_report_html(res_chk, out2)
  html2 <- paste(readLines(out2, warn = FALSE), collapse = "\n")
  expect_true(grepl("id='fig-checkpanels'", html2, fixed = TRUE))
})

test_that("PDF report still builds when a check is configured", {
  res <- ngcd_res_with_check()
  out <- tempfile(fileext = ".pdf")
  nextgenCrossWorkbench:::ngcd_report_pdf(res, out)
  expect_true(file.exists(out) && file.size(out) > 2000)
})
