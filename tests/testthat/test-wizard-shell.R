# Tests for the guided-wizard shell (redesign Task 1).

test_that("pure navigation helpers clamp and never wrap", {
  expect_equal(ngcd_wiz_clamp(0, 10), 1L)
  expect_equal(ngcd_wiz_clamp(5, 10), 5L)
  expect_equal(ngcd_wiz_clamp(99, 10), 10L)
  expect_equal(ngcd_wiz_clamp(NA, 10), 1L)

  expect_equal(ngcd_wiz_go(1, -1, 10), 1L)   # can't go before first
  expect_equal(ngcd_wiz_go(1,  1, 10), 2L)
  expect_equal(ngcd_wiz_go(10, 1, 10), 10L)  # can't go past last
  expect_equal(ngcd_wiz_go(5,  3, 10), 8L)
})

test_that("completion marks strictly-earlier steps", {
  expect_equal(ngcd_wiz_completed(1, 4), c(FALSE, FALSE, FALSE, FALSE))
  expect_equal(ngcd_wiz_completed(3, 4), c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(ngcd_wiz_completed(4, 4), c(TRUE, TRUE, TRUE, FALSE))
})

test_that("step model is well-formed and index lookup works", {
  steps <- ngcd_wizard_steps()
  expect_true(length(steps) >= 8)
  ids <- vapply(steps, function(s) s$id, character(1))
  expect_false(any(duplicated(ids)))                 # stable, unique keys
  expect_equal(ngcd_wiz_index_of(steps, "run"), match("run", ids))
  expect_true(is.na(ngcd_wiz_index_of(steps, "nope")))
})

test_that("UI builders return Shiny tags without error", {
  steps <- ngcd_wizard_steps()
  st <- ngcd_wizard_stepper(steps, current = 3)
  expect_s3_class(st, "shiny.tag")
  html <- as.character(st)
  expect_true(grepl("is-current", html))             # step 3 marked current
  expect_true(grepl("is-done", html))                # steps 1-2 done
  expect_true(grepl("&#10003;", html, fixed = TRUE))  # completed dots show a check

  # summary: empty state and populated state both render
  expect_s3_class(ngcd_wizard_summary(list()), "shiny.tag")
  grp <- list(list(title = "Basic information",
                   rows = list(list(k = "Traits", v = c("A", "B")))))
  sm <- as.character(ngcd_wizard_summary(grp))
  expect_true(grepl("ngcd-wiz-chip", sm))            # multi-value -> chips

  nav_mid <- as.character(ngcd_wizard_nav(3, length(steps)))
  expect_true(grepl("ngcd_wiz_next", nav_mid))
  nav_end <- as.character(ngcd_wizard_nav(length(steps), length(steps)))
  expect_true(grepl("ngcd_wiz_finish", nav_end))     # last step -> Finish
  expect_false(grepl("ngcd_wiz_next", nav_end))

  expect_s3_class(ngcd_wizard_css(), "shiny.tag")
  expect_s3_class(ngcd_wizard_outlet(), "shiny.tag.list")
})

test_that("server wiring advances, retreats, clamps, and jumps by id", {
  steps <- ngcd_wizard_steps()
  n <- length(steps)
  server <- function(input, output, session) {
    w <- ngcd_wizard_init(input, output, session, steps,
                          body_fn = function(id) shiny::span(id))
    exportTestValues(cur = w$current(), sid = w$step_id())
    session$userData$w <- w
  }
  shiny::testServer(server, {
    expect_equal(session$getReturned(), NULL)
    # starts at step 1
    expect_equal(w <- session$userData$w, session$userData$w)
    expect_equal(w$current(), 1L)
    expect_equal(w$step_id(), "start")

    session$setInputs(ngcd_wiz_next = 1); expect_equal(w$current(), 2L)
    session$setInputs(ngcd_wiz_next = 2); expect_equal(w$current(), 3L)
    session$setInputs(ngcd_wiz_back = 1); expect_equal(w$current(), 2L)
    # cannot retreat before the first step
    session$setInputs(ngcd_wiz_back = 2)
    session$setInputs(ngcd_wiz_back = 3)
    expect_equal(w$current(), 1L)
    # jump by id, and clamp on overshoot
    w$go("run"); expect_equal(w$step_id(), "run")
    for (k in 1:20) session$setInputs(ngcd_wiz_next = 100 + k)
    expect_equal(w$current(), n)                      # clamped at last
    expect_equal(w$step_id(), "results")
  })
})
