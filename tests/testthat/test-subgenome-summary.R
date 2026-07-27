ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")

# A minimal poly-style result mirroring the disomic-subgenome backend shape:
# plan_summary carries only n_crosses + mean_gain, settings are subgenome-specific,
# and the standard metric/method/coancestry/priority/qc fields are absent.
sg_result <- function() {
  list(
    package_version = "0.14.0",
    prediction_mode = "subgenome_design",
    poly_design = TRUE,
    plan_summary = list(n_crosses = 4L, mean_gain = 28.68),
    settings = list(grm_method = "vanraden", progeny = "disomic_subgenome",
                    variance_model = "recombination_aware"),
    subgenome = list(names = list("A", "B"),
                     markers_per_subgenome = list(A = 24L, B = 24L),
                     mode = "ocs", variance_model = "recombination_aware",
                     progeny_target = "DH"),
    selected_crosses = data.frame(
      parent1 = c("L01", "L01", "L02", "L03"),
      parent2 = c("L02", "L03", "L04", "L04"),
      pair_kinship = c(-0.05, 0.10, -0.02, 0.03),
      stringsAsFactors = FALSE))
}

test_that("ngcd_enrich_result backfills derivable plan-summary fields", {
  enr <- ng("ngcd_enrich_result")(sg_result())
  ps <- enr$plan_summary
  expect_equal(ps$unique_parents, 4L)          # L01..L04
  expect_equal(ps$max_parent_use, 2L)          # L01 and L04 each appear twice
  expect_equal(round(ps$mean_pair_kinship, 3), 0.015)
  # existing fields are never overwritten
  expect_equal(ps$n_crosses, 4L)
  expect_equal(ps$mean_gain, 28.68)
})

test_that("ngcd_enrich_result is a no-op without a usable selected-crosses table", {
  r <- list(plan_summary = list(n_crosses = 1L))
  expect_identical(ng("ngcd_enrich_result")(r), r)
  expect_identical(ng("ngcd_enrich_result")(NULL), NULL)
})

test_that("subgenome executive summary has no placeholders and is subgenome-specific", {
  enr <- ng("ngcd_enrich_result")(sg_result())
  html <- ng("ngcd_exec_summary_html")(enr)
  # the generic summary would emit "?" for metric/method/optimizer and &ndash; for
  # the missing counts; the subgenome branch must emit neither.
  expect_false(grepl(">?<", html, fixed = TRUE))
  expect_false(grepl("&ndash;", html, fixed = TRUE))
  expect_true(grepl("disomic-subgenome", html))
  expect_true(grepl("subgenomes (A, B)", html, fixed = TRUE))
  expect_true(grepl("A=24, B=24", html, fixed = TRUE))
  expect_true(grepl("recombination-aware", html))
  expect_true(grepl("4</b> unique parents", html) || grepl("<b>4</b> unique parents", html))
})
