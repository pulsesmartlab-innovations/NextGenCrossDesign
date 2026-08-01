# Staged pipeline (Phase 2, Task 3): the pure do_run_pipeline() stage-walk
# helper ngcd_next_stages(). No shiny, no backend, no I/O - see R/helpers.R.

ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")

# Build a pipeline whose stages carry the given statuses (unspecified stages
# stay at the init default of "stale").
mk_pipeline <- function(statuses = list()) {
  p <- ng("ngcd_pipeline_init")()
  for (s in names(statuses)) p$stages[[s]]$status <- statuses[[s]]
  p
}

test_that("ngcd_next_stages: a fresh pipeline runs the whole chain", {
  ns <- ng("ngcd_next_stages")
  out <- ns(ng("ngcd_pipeline_init")())
  expect_false(out$blocked)
  expect_identical(out$stages, c("qc", "predict", "index", "allocate", "rank"))
})

test_that("ngcd_next_stages: resumes at the first non-done stage, skipping done", {
  ns <- ng("ngcd_next_stages")
  p <- mk_pipeline(list(qc = "done", predict = "done",
                        index = "stale", allocate = "stale", rank = "stale"))
  expect_identical(ns(p)$stages, c("index", "allocate", "rank"))
})

test_that("ngcd_next_stages: everything done -> nothing to run", {
  ns <- ng("ngcd_next_stages")
  p <- mk_pipeline(list(qc = "done", predict = "done", index = "done",
                        allocate = "done", rank = "done"))
  out <- ns(p)
  expect_false(out$blocked)
  expect_identical(out$stages, character(0))
})

test_that("ngcd_next_stages: qc blocked -> blocked, nothing runs", {
  ns <- ng("ngcd_next_stages")
  out <- ns(mk_pipeline(list(qc = "blocked")))
  expect_true(out$blocked)
  expect_identical(out$stages, character(0))
})

test_that("ngcd_next_stages: a stale upstream restarts there even if a downstream is done", {
  ns <- ng("ngcd_next_stages")
  p <- mk_pipeline(list(qc = "done", predict = "stale",
                        index = "done", allocate = "done", rank = "done"))
  expect_identical(ns(p)$stages, c("predict", "index", "allocate", "rank"))
})

test_that("ngcd_next_stages: a qc error (not blocker) restarts at qc", {
  ns <- ng("ngcd_next_stages")
  out <- ns(mk_pipeline(list(qc = "error")))
  expect_false(out$blocked)
  expect_identical(out$stages, c("qc", "predict", "index", "allocate", "rank"))
})
