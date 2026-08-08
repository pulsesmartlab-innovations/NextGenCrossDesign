# Stage-centric Run pipeline: per-stage one-line summaries (ngcd_stage_summary)
# render from the stored stage JSON, so each Run card can show its own result.

test_that("ngcd_stage_summary renders a one-line summary per stage from stage JSON", {
  ss <- nextgenCrossWorkbench:::ngcd_stage_summary

  # not-run / empty
  expect_equal(ss("qc", NULL), "Not run yet.")
  expect_equal(ss("predict", list()), "Not run yet.")

  # qc: blockers + warnings counted from issues (each issue has a severity)
  qc <- list(status = "warning",
             issues = list(list(severity = "warning", message = "x"),
                           list(severity = "warning", message = "y")))
  expect_match(ss("qc", qc), "0 blocker")
  expect_match(ss("qc", qc), "2 warning")

  qcb <- list(status = "blocker",
              issues = list(list(severity = "blocker", message = "dup")))
  expect_match(ss("qc", qcb), "1 blocker")

  # predict: traits scored + candidates
  pr <- list(effect_summary = data.frame(trait = c("yield", "protein", "disease")),
             n_candidates = 45)
  expect_match(ss("predict", pr), "3 traits")
  expect_match(ss("predict", pr), "45 candidate")

  # index: method
  ix <- list(multi_trait_score = c(1, 2, 3), objective = list(method = "economic_index"))
  expect_match(ss("index", ix), "economic_index")

  # allocate: crosses + mean gain
  al <- list(plan_summary = list(n_crosses = 12, mean_gain = 3.4, group_coancestry = 0.21))
  expect_match(ss("allocate", al), "12 cross")
  expect_match(ss("allocate", al), "3.4")
})
