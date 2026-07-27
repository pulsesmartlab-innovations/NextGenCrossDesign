ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")

# Minimal result with the per-trait value + mid-parent GEBV columns the report
# figures read, plus the tier/kinship/score fields the scatter needs.
mk_res <- function() {
  sc <- data.frame(
    parent1 = c("A", "A", "B"), parent2 = c("B", "C", "C"),
    yield_value = c(60, 58, 55), disease_value = c(2, 3, 1),
    yield_mean_gebv = c(59.1, 57.2, 54.3), disease_mean_gebv = c(2.1, 3.2, 1.1),
    multi_trait_score = c(1.5, 0.8, -0.2), pair_kinship = c(0.10, 0.05, 0.20),
    priority_tier = factor(c("highly_priority", "priority", "low_priority"),
      levels = c("highly_priority", "priority", "medium_priority", "low_priority")),
    stringsAsFactors = FALSE)
  cc <- data.frame(pair_kinship = c(0.1, 0.05, 0.2, 0.3),
                   multi_trait_score = c(1.5, 0.8, -0.2, -1))
  list(selected_crosses = sc, candidate_crosses = cc)
}

test_that("trait-rank heatmap hover carries the mid-parent GEBV", {
  hm <- plotly::plotly_build(ng("ngcd_ply_trait_heatmap")(mk_res()))
  tx <- hm$x$data[[1]]$text
  cell <- if (is.matrix(tx)) tx[1, 1] else tx[[1]][[1]]
  expect_match(cell, "mid-parent GEBV: 59.10", fixed = TRUE)   # yield_mean_gebv[1]
  expect_match(cell, "rank (z-score)", fixed = TRUE)
})

test_that("priority-tier scatter hover carries per-trait mid-parent GEBV and score", {
  sca <- plotly::plotly_build(ng("ngcd_ply_scatter")(mk_res()))
  txts <- unlist(lapply(sca$x$data, function(tr) if (!is.null(tr$text)) tr$text[1] else NULL))
  all_txt <- paste(txts, collapse = " ")
  expect_match(all_txt, "yield mid-parent GEBV: 59.10", fixed = TRUE)
  expect_match(all_txt, "disease mid-parent GEBV: 2.10", fixed = TRUE)
  expect_match(all_txt, "score: 1.50", fixed = TRUE)
})

test_that("ngcd_cross_hover degrades gracefully when a GEBV column is absent", {
  df <- data.frame(parent1 = "A", parent2 = "B", multi_trait_score = 1.2,
                   stringsAsFactors = FALSE)
  h <- ng("ngcd_cross_hover")(df, character(0))
  expect_match(h, "A x B", fixed = TRUE)
  expect_match(h, "score: 1.20", fixed = TRUE)
})

test_that("heatmap shows -- for a trait lacking a mid-parent GEBV column", {
  r <- mk_res(); r$selected_crosses$disease_mean_gebv <- NULL
  hm <- plotly::plotly_build(ng("ngcd_ply_trait_heatmap")(r))
  tx <- hm$x$data[[1]]$text
  flat <- paste(as.vector(if (is.matrix(tx)) tx else unlist(tx)), collapse = " ")
  expect_match(flat, "mid-parent GEBV: --", fixed = TRUE)      # disease column
  expect_match(flat, "mid-parent GEBV: 59.10", fixed = TRUE)   # yield still shown
})

test_that("ngcd_gebv_text_matrix formats values and fills -- for missing", {
  sc <- data.frame(a_mean_gebv = c(1.234, NA), stringsAsFactors = FALSE)
  m <- ng("ngcd_gebv_text_matrix")(sc, c("a", "b"), digits = 2)
  expect_equal(m[1, 1], "1.23")
  expect_equal(m[2, 1], "--")            # NA value
  expect_true(all(m[, 2] == "--"))       # trait b has no _mean_gebv column
})

test_that("PDF heatmap and scatter render without error", {
  pf <- tempfile(fileext = ".pdf"); grDevices::pdf(pf)
  on.exit({ grDevices::dev.off(); unlink(pf) }, add = TRUE)
  expect_no_error(ng("ngcd_fig_trait_heatmap")(mk_res()))
  expect_no_error(ng("ngcd_fig_scatter")(mk_res()))
})
