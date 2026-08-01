# Phase 3 Task 2: plotly builders for staged-pipeline figures.
#   * ngcd_ply_trait_distribution - NEW pre-run trait-distribution preview
#   * ngcd_ply_parent_use_counts  - NEW allocate.json parent_use bar
#   * ngcd_stage_figure           - dispatcher adapting each stage's JSON to
#     the existing post-run plotly builders (report.R:293-453)
# No backend needed - pure functions over hand-crafted / fixture JSON shapes.

ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")

# Real staged-run artifacts, when available in this job's scratch dir; the
# tests fall back to hand-crafted minimal shapes (matching task-2-brief.md)
# when the env var doesn't resolve, so this file is portable across machines.
fixture_dir <- function() {
  base <- Sys.getenv("CLAUDE_JOB_DIR", "")
  if (!nzchar(base)) return(NULL)
  d <- file.path(base, "tmp", "p3_artifacts", "artifacts")
  if (dir.exists(d)) d else NULL
}

load_stage_json <- function(stage) {
  d <- fixture_dir()
  if (!is.null(d) && file.exists(file.path(d, paste0(stage, ".json"))))
    return(jsonlite::fromJSON(file.path(d, paste0(stage, ".json")), simplifyDataFrame = TRUE))

  switch(stage,
    qc = list(status = "pass", issues = list(),
              putative_duplicates = list(pairs = list())),
    predict = list(
      effect_summary = data.frame(
        trait = c("yield", "disease"), column = c("yield", "disease"),
        direction = c("maximize", "minimize"),
        value_column = c("yield_value", "disease_value"),
        marker_effect_reliability = c(0.83, 0.62), stringsAsFactors = FALSE),
      ld_pruning_report = NULL, n_candidates = 45L),
    index = list(multi_trait_score = c(1.06, 0.25, 0.61, -0.27, -0.95),
                 objective = list(method = "auto")),
    allocate = list(
      plan_summary = list(n_crosses = 8L, mean_gain = 1.08, group_coancestry = 0.35),
      parent_use = data.frame(
        parent = c("P08", "P02", "P01"), crosses = c(5L, 4L, 2L),
        stringsAsFactors = FALSE)))
}

expect_valid_plotly <- function(p, info = NULL) {
  expect_s3_class(p, "plotly")
  expect_error(plotly::plotly_build(p), NA, info = info)
}

# plotly's `layout$title` is either a bare string or a list(text = ...)
# depending on how it was set - normalise before matching against it.
title_text <- function(b) {
  t <- b$x$layout$title
  if (is.list(t)) t$text else t
}

# ---------------------------------------------------------------------------
# ngcd_ply_trait_distribution
# ---------------------------------------------------------------------------

test_that("trait distribution builds a histogram trace for a numeric column", {
  fn <- ng("ngcd_ply_trait_distribution")
  df <- data.frame(NAME = paste0("L", 1:6), yield = c(10, 12, 9, 15, 11, NA))
  p <- fn(df, "yield")
  expect_valid_plotly(p)
  b <- plotly::plotly_build(p)
  types <- vapply(b$x$data, function(tr) tr$type %||% NA_character_, character(1))
  expect_true("histogram" %in% types)
  expect_match(title_text(b), "Distribution: yield")
})

test_that("trait distribution never errors on NULL / non-numeric / missing column", {
  fn <- ng("ngcd_ply_trait_distribution")
  df <- data.frame(NAME = c("L1", "L2"), yield = c(10, 12), notes = c("a", "b"))

  expect_valid_plotly(fn(NULL, "yield"), info = "NULL df")
  expect_valid_plotly(fn(df, "missing_col"), info = "missing column")
  expect_valid_plotly(fn(df, "notes"), info = "non-numeric column (all NA after coercion)")
  expect_valid_plotly(fn(df, NULL), info = "NULL col")
  expect_valid_plotly(fn(data.frame(), "yield"), info = "empty df")

  # the annotation carries the guidance message for the empty case
  b <- plotly::plotly_build(fn(NULL, "yield"))
  ann <- b$x$layout$annotations[[1]]$text
  expect_match(ann, "phenotype")
})

# ---------------------------------------------------------------------------
# ngcd_ply_parent_use_counts
# ---------------------------------------------------------------------------

test_that("parent use counts builds a horizontal bar from allocate.json's shape", {
  fn <- ng("ngcd_ply_parent_use_counts")
  pu <- data.frame(parent = c("P08", "P02", "P01"), crosses = c(5L, 4L, 2L))
  p <- fn(pu)
  expect_valid_plotly(p)
  b <- plotly::plotly_build(p)
  expect_identical(b$x$data[[1]]$type, "bar")
  expect_identical(b$x$data[[1]]$orientation, "h")
})

test_that("parent use counts is a valid empty plot for NULL/empty/malformed input", {
  fn <- ng("ngcd_ply_parent_use_counts")
  expect_valid_plotly(fn(NULL))
  expect_valid_plotly(fn(data.frame()))
  expect_valid_plotly(fn(data.frame(parent = "P1")))  # missing crosses col
})

# ---------------------------------------------------------------------------
# ngcd_stage_figure dispatcher
# ---------------------------------------------------------------------------

test_that("qc stage figure builds on the real fixture (empty duplicate pairs)", {
  fn <- ng("ngcd_stage_figure")
  qc <- load_stage_json("qc")
  p <- fn("qc", qc)
  expect_valid_plotly(p)
  b <- plotly::plotly_build(p)
  ann <- b$x$layout$annotations[[1]]$text %||% ""
  expect_match(ann, "duplicate")
})

test_that("qc stage figure builds the duplicate heatmap when pairs are present", {
  fn <- ng("ngcd_stage_figure")
  qc <- list(status = "warning", issues = list(),
             putative_duplicates = list(pairs = data.frame(
               id1 = c("L1", "L3"), id2 = c("L2", "L4"), similarity = c(0.98, 0.95),
               stringsAsFactors = FALSE)))
  p <- fn("qc", qc)
  expect_valid_plotly(p)
  b <- plotly::plotly_build(p)
  expect_identical(b$x$data[[1]]$type, "heatmap")
})

test_that("predict stage figure builds a reliability bar", {
  fn <- ng("ngcd_stage_figure")
  p <- fn("predict", load_stage_json("predict"))
  expect_valid_plotly(p)
  b <- plotly::plotly_build(p)
  expect_identical(b$x$data[[1]]$type, "bar")
})

test_that("index stage figure builds a histogram titled for the computed score", {
  fn <- ng("ngcd_stage_figure")
  p <- fn("index", load_stage_json("index"))
  expect_valid_plotly(p)
  b <- plotly::plotly_build(p)
  types <- vapply(b$x$data, function(tr) tr$type %||% NA_character_, character(1))
  expect_true("histogram" %in% types)
  expect_match(title_text(b), "Index distribution")
})

test_that("allocate stage figure returns a frontier + parents pair, robust to no frontier", {
  fn <- ng("ngcd_stage_figure")
  al <- load_stage_json("allocate")  # fixture/hand-craft has no plan_summary$frontier
  figs <- fn("allocate", al)
  expect_type(figs, "list")
  expect_named(figs, c("frontier", "parents"))

  expect_valid_plotly(figs$frontier, info = "frontier (absent -> empty-plot note)")
  b <- plotly::plotly_build(figs$frontier)
  ann <- b$x$layout$annotations[[1]]$text %||% ""
  expect_match(ann, "frontier")

  expect_valid_plotly(figs$parents, info = "parents (builds from parent_use)")
  bp <- plotly::plotly_build(figs$parents)
  expect_identical(bp$x$data[[1]]$type, "bar")
})

test_that("allocate stage figure draws the real frontier when plan_summary carries one", {
  fn <- ng("ngcd_stage_figure")
  al <- list(
    plan_summary = list(
      mean_gain = 1.08, group_coancestry = 0.35,
      frontier = data.frame(group_coancestry = c(0.1, 0.35, 0.6),
                            mean_gain = c(0.6, 1.08, 1.4))),
    parent_use = data.frame(parent = c("P1", "P2"), crosses = c(3L, 1L)))
  figs <- fn("allocate", al)
  expect_valid_plotly(figs$frontier)
  b <- plotly::plotly_build(figs$frontier)
  types <- vapply(b$x$data, function(tr) tr$type %||% NA_character_, character(1))
  expect_true("scatter" %in% types)
})

test_that("stage figure dispatcher never errors on empty/partial/unknown stage input", {
  fn <- ng("ngcd_stage_figure")
  expect_valid_plotly(fn("qc", list()))
  expect_valid_plotly(fn("predict", list()))
  expect_valid_plotly(fn("index", list()))
  figs <- fn("allocate", list())
  expect_valid_plotly(figs$frontier)
  expect_valid_plotly(figs$parents)
  expect_valid_plotly(fn("unknown_stage", list(a = 1)))
  expect_valid_plotly(fn("qc", NULL))
})
