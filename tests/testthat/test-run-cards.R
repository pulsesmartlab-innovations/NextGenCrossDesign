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

test_that("changing an allocation-only setting never re-runs completed earlier steps (compute-once)", {
  srv <- nextgenCrossWorkbench:::workbench_server(
    nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb")))
  testServer(srv, {
    do.call(session$setInputs, demo_inputs())

    # mark QC, predict, index as already completed, each stamped with the exact
    # config subset it ran under (what the staleness engine compares against).
    for (s in c("qc", "predict", "index")) {
      rv$pipeline$stages[[s]]$status <- "done"
      rv$pipeline$stages[[s]]$cfg    <-
        nextgenCrossWorkbench:::ngcd_stage_cfg_subset(build_params(), s)
    }

    # change an ALLOCATION-only input (n_crosses lives only in the allocate
    # stage's key set) and let the staleness observer recompute.
    session$setInputs(n_crosses = 999)
    session$flushReact()

    # the completed upstream steps stay done -> they will NOT be recomputed
    expect_equal(rv$pipeline$stages$qc$status,      "done")
    expect_equal(rv$pipeline$stages$predict$status, "done")
    expect_equal(rv$pipeline$stages$index$status,   "done")
    # only the affected stage (and downstream) is stale
    expect_true(rv$pipeline$stages$allocate$status %in% c("stale", "blocked"))
  })
})

test_that("the Run area renders adaptive stepped cards (multi=4, single=3, poly=single)", {
  srv <- nextgenCrossWorkbench:::workbench_server(
    nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb")))
  rahtml <- function(output) {
    ra <- output$run_area
    paste(as.character(ra$html %||% ra), collapse = " ")
  }
  testServer(srv, {
    # multi-trait -> four cards incl. the Build selection index step + run_all
    do.call(session$setInputs, demo_inputs(objective_mode = "multi"))
    session$flushReact()
    h <- rahtml(output)
    expect_match(h, "Quality control")
    expect_match(h, "Fit effects")
    expect_match(h, "Build selection index")
    expect_match(h, "Allocate")
    expect_match(h, "run_all")

    # single-trait -> the index card is hidden (3 steps)
    session$setInputs(objective_mode = "single"); session$flushReact()
    expect_false(grepl("Build selection index", rahtml(output), fixed = TRUE))
    expect_true(grepl("Quality control", rahtml(output), fixed = TRUE))

    # polyploid -> the single one-shot Run card, no stepped pipeline
    session$setInputs(workflow = "polyploid"); session$flushReact()
    expect_false(grepl("Quality control", rahtml(output), fixed = TRUE))
  })
})
