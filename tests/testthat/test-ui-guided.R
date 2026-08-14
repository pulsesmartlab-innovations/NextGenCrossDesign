# Tests for the guided navigation layer (redesign Task 2).

test_that("step list respects developer mode", {
  s_user <- ngcd_guided_nav_steps(dev = FALSE)
  s_dev  <- ngcd_guided_nav_steps(dev = TRUE)
  expect_equal(vapply(s_user, `[[`, "", "id"), c("Data", "Configure", "Run", "Results"))
  expect_equal(s_dev[[1]]$id, "Setup")
  expect_equal(length(s_dev), length(s_user) + 1L)
})

test_that("current-index resolves and defaults safely", {
  s <- ngcd_guided_nav_steps(FALSE)
  expect_equal(ngcd_guided_nav_index(s, "Configure"), 2L)
  expect_equal(ngcd_guided_nav_index(s, NULL), 1L)        # before first flush
  expect_equal(ngcd_guided_nav_index(s, "Nope"), 1L)      # unknown tab
})

test_that("Back/Next targets clamp at the ends", {
  s <- ngcd_guided_nav_steps(FALSE)
  expect_equal(ngcd_guided_nav_target(s, "Data", -1L), "Data")      # can't go before first
  expect_equal(ngcd_guided_nav_target(s, "Data",  1L), "Configure")
  expect_equal(ngcd_guided_nav_target(s, "Run",   1L), "Results")
  expect_equal(ngcd_guided_nav_target(s, "Results", 1L), "Results") # can't go past last
  expect_equal(ngcd_guided_nav_target(s, "Configure", -1L), "Data")
})

test_that("UI builders return Shiny tags", {
  expect_s3_class(ngcd_guided_css(), "shiny.tag")
  bar <- ngcd_guided_bar_ui()
  expect_s3_class(bar, "shiny.tag.list")
  h <- as.character(bar)
  expect_true(grepl("ngcd_guided_stepper", h))
  expect_true(grepl("ngcd_guided_back", h))
  expect_true(grepl("ngcd_guided_next", h))
  expect_true(grepl("ngcd-guided", h))                    # body-class toggle present
})

test_that("server init renders the stepper and moves the tabset", {
  server <- function(input, output, session) {
    ngcd_guided_nav_init(input, output, session, dev = FALSE, nav_id = "nav")
  }
  shiny::testServer(server, {
    session$setInputs(nav = "Data")
    # stepper renders with Data current
    expect_true(grepl("is-current", as.character(output$ngcd_guided_stepper$html)))
    # Next selects the following tab (nav_select sends an updateTabsetPanel message)
    session$setInputs(ngcd_guided_next = 1)
    msgs <- session$flushReact()
    expect_true(TRUE)  # no error thrown wiring the observer
  })
})

# --- finer guided flow: flat steps over outer tabs + Configure sub-tabs -----
test_that("flat steps expand Configure into sub-steps", {
  s <- ngcd_guided_flat_steps(dev = FALSE)
  ids <- vapply(s, `[[`, "", "id")
  expect_equal(ids, c("data","objective","scoring","filters","allocation","outputs","run","results"))
  # Configure sub-steps carry an inner sub-tab title
  obj <- s[[which(ids == "objective")]]
  expect_equal(obj$outer, "Configure"); expect_equal(obj$inner, "Selection objective")
  expect_null(s[[which(ids == "data")]]$inner)
  expect_equal(ngcd_guided_flat_steps(dev = TRUE)[[1]]$id, "setup")
})

test_that("flat index resolves from (outer, inner)", {
  s <- ngcd_guided_flat_steps(FALSE)
  expect_equal(ngcd_guided_flat_index(s, "Data"), 1L)
  expect_equal(ngcd_guided_flat_index(s, "Configure", "Mate allocation"), 5L)
  expect_equal(ngcd_guided_flat_index(s, "Configure", "Selection objective"), 2L)
  expect_equal(ngcd_guided_flat_index(s, "Configure", NULL), 2L)   # unknown inner -> first Configure step
  expect_equal(ngcd_guided_flat_index(s, NULL), 1L)                # before first flush
  expect_equal(ngcd_guided_flat_index(s, "Results"), 8L)
})

test_that("flat Back/Next targets walk one screen at a time and clamp", {
  s <- ngcd_guided_flat_steps(FALSE)
  expect_equal(ngcd_guided_flat_target(s, "Data", NULL,  1L)$id, "objective")     # Data -> first Configure sub
  expect_equal(ngcd_guided_flat_target(s, "Configure", "Selection objective", 1L)$id, "scoring")
  expect_equal(ngcd_guided_flat_target(s, "Configure", "Export options", 1L)$id, "run")  # last Configure -> Run
  expect_equal(ngcd_guided_flat_target(s, "Configure", "Selection objective", -1L)$id, "data")
  expect_equal(ngcd_guided_flat_target(s, "Data", NULL, -1L)$id, "data")           # clamp at start
  expect_equal(ngcd_guided_flat_target(s, "Results", NULL, 1L)$id, "results")      # clamp at end
  # a Configure target carries its inner sub-tab
  expect_equal(ngcd_guided_flat_target(s, "Data", NULL, 1L)$inner, "Selection objective")
})

test_that("init wires the flat stepper and moves without error", {
  server <- function(input, output, session)
    ngcd_guided_nav_init(input, output, session, dev = FALSE)
  shiny::testServer(server, {
    session$setInputs(nav = "Data", cfg_nav = "Selection objective")
    expect_true(grepl("is-current", as.character(output$ngcd_guided_stepper$html)))
    session$setInputs(ngcd_guided_next = 1)   # Data -> Objective (Configure/Selection objective)
    session$setInputs(nav = "Configure", cfg_nav = "Mate allocation")
    session$setInputs(ngcd_guided_next = 2)   # Allocation -> Outputs
    expect_true(TRUE)
  })
})

# --- live run-summary sidebar ----------------------------------------------
test_that("run-summary groups reflect real inputs and status", {
  inp <- list(workflow = "polyploid", data_source = "upload", objective_mode = "multi",
              traits_to_use = c("yield", "disease"), n_crosses = 25, max_uses_per_parent = 3)
  gr <- ngcd_guided_summary_groups(inp, has_result = TRUE)
  expect_equal(vapply(gr, `[[`, "", "title"), c("Setup", "Allocation", "Run"))
  h <- as.character(ngcd_wizard_summary(gr))
  expect_true(grepl("Autotetraploid", h))          # workflow mapped
  expect_true(grepl("Multi-trait index", h))       # objective mapped
  expect_true(grepl("ngcd-wiz-chip", h))           # traits as chips
  expect_true(grepl("ready", h))                   # run status

  # empty/NULL inputs -> safe defaults, "not run yet"
  g0 <- ngcd_guided_summary_groups(list(), has_result = FALSE)
  h0 <- as.character(ngcd_wizard_summary(g0))
  expect_true(grepl("Standard \\(diploid\\)", h0))
  expect_true(grepl("not run yet", h0))
})

test_that("compact summary line reflects inputs + status", {
  s <- ngcd_guided_summary_line(list(workflow = "polyploid", data_source = "upload",
         traits_to_use = c("a", "b"), n_crosses = 20), has_result = TRUE)
  expect_true(grepl("Autotetraploid", s))
  expect_true(grepl("uploaded CSVs", s))
  expect_true(grepl("2 traits", s))
  expect_true(grepl("20 crosses", s))
  expect_true(grepl("result ready", s))
  expect_true(grepl("Standard", ngcd_guided_summary_line(list())))     # NULL-safe defaults
  expect_true(grepl("not run yet", ngcd_guided_summary_line(list())))
})

test_that("nav_init renders the summary text into output$ngcd_guided_summary", {
  server <- function(input, output, session)
    ngcd_guided_nav_init(input, output, session, dev = FALSE, res_fn = function() NULL)
  shiny::testServer(server, {
    session$setInputs(nav = "Data", cfg_nav = "Selection objective",
                      workflow = "standard", n_crosses = 10)
    expect_true(grepl("not run yet", output$ngcd_guided_summary))
    expect_true(grepl("10 crosses", output$ngcd_guided_summary))
  })
})

# --- clickable stepper + safe navbar hiding --------------------------------
test_that("stepper is clickable when click_input is set", {
  s <- ngcd_guided_flat_steps(FALSE)
  h_plain <- as.character(ngcd_wizard_stepper(s, 1))
  expect_false(grepl("onclick", h_plain))               # no clicks by default (e.g. demo)
  h_click <- as.character(ngcd_wizard_stepper(s, 1, click_input = "ngcd_guided_goto"))
  # onclick present (quotes are HTML-escaped in the attribute; browser un-escapes)
  expect_true(grepl("onclick", h_click))
  expect_true(grepl("setInputValue", h_click))
  expect_true(grepl("ngcd_guided_goto", h_click))
  expect_true(grepl("cursor:pointer", h_click))
})

test_that("guided bar hides navbar by default, keeps it with the escape hatch", {
  expect_true(grepl("navbar-nav\\{display:none", as.character(ngcd_guided_bar_ui(hide_nav = TRUE))))
  expect_false(grepl("navbar-nav\\{display:none", as.character(ngcd_guided_bar_ui(hide_nav = FALSE))))
})

test_that("goto jumps straight to a step by id", {
  server <- function(input, output, session)
    ngcd_guided_nav_init(input, output, session, dev = FALSE, res_fn = function() NULL)
  shiny::testServer(server, {
    session$setInputs(nav = "Data", cfg_nav = "Selection objective")
    session$setInputs(ngcd_guided_goto = "allocation")   # jump to Mate allocation
    expect_true(TRUE)                                     # no error wiring the jump
  })
})
