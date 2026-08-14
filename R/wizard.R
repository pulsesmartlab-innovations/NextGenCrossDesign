# ---------------------------------------------------------------------------
# Guided-wizard shell (redesign Task 1)
#
# Reusable, self-contained pieces that later steps compose:
#   - a step model + pure navigation helpers (unit-testable without Shiny)
#   - a numbered stepper, a live run-summary panel, nav buttons, a 2-col shell
#   - ngcd_wizard_init(): wires current-step state + Back/Next observers and
#     renders stepper/body/summary into a single (non-modular) server
#   - ngcd_wizard_demo(): a runnable demo app so the shell can be seen in isolation
#
# Nothing here touches the backend or any existing input id. It is guarded in the
# app behind getOption("ngcd.wizard"); the classic tab UI is unchanged when off.
# ---------------------------------------------------------------------------

# ---- step model ------------------------------------------------------------

# Default top-level steps for the workbench (see the redesign design spec section 4).
# Each step: id (stable key), label (shown), optional (skippable/soft step).
ngcd_wizard_steps <- function() {
  list(
    list(id = "start",      label = "Start",               optional = FALSE),
    list(id = "data",       label = "Data",                optional = FALSE),
    list(id = "qc",         label = "Quality control",     optional = FALSE),
    list(id = "objective",  label = "Objective",           optional = FALSE),
    list(id = "scoring",    label = "Prediction & scoring",optional = FALSE),
    list(id = "filters",    label = "Cross filters",       optional = TRUE),
    list(id = "allocation", label = "Mate allocation",     optional = FALSE),
    list(id = "outputs",    label = "Outputs",             optional = FALSE),
    list(id = "run",        label = "Run",                 optional = FALSE),
    list(id = "results",    label = "Results",             optional = FALSE)
  )
}

# ---- pure navigation helpers (no Shiny; unit-tested directly) ---------------

# Clamp a 1-based step index into [1, n].
ngcd_wiz_clamp <- function(i, n) {
  i <- suppressWarnings(as.integer(i))
  if (length(i) != 1L || is.na(i)) i <- 1L
  max(1L, min(as.integer(i), as.integer(n)))
}

# Move `delta` steps from `cur` within [1, n] (clamped; never wraps).
ngcd_wiz_go <- function(cur, delta, n) ngcd_wiz_clamp(cur + as.integer(delta), n)

# Which steps are "complete": every step strictly before the current one is
# considered visited/complete. Returns a logical vector of length n.
ngcd_wiz_completed <- function(cur, n) seq_len(n) < ngcd_wiz_clamp(cur, n)

# Resolve a step id -> 1-based index (NA if absent).
ngcd_wiz_index_of <- function(steps, id) {
  ids <- vapply(steps, function(s) s$id, character(1))
  m <- match(id, ids)
  if (is.na(m)) NA_integer_ else as.integer(m)
}

# ---- styling ---------------------------------------------------------------

# Scoped CSS for the stepper + summary. Uses the app's Bootstrap primary var so
# it inherits the NDSU-green accent from the existing 0.24.0 theme.
ngcd_wizard_css <- function() {
  shiny::tags$style(shiny::HTML("
  .ngcd-wiz-steps{list-style:none;display:flex;gap:0;margin:0 0 4px;padding:0;
    align-items:flex-start;overflow-x:auto}
  .ngcd-wiz-step{flex:1 1 0;min-width:64px;text-align:center;position:relative;
    font-size:.78rem;color:var(--bs-secondary-color,#5c6b64)}
  .ngcd-wiz-step .dot{width:30px;height:30px;border-radius:50%;margin:0 auto 6px;
    display:flex;align-items:center;justify-content:center;font-weight:600;
    border:2px solid var(--bs-border-color,#d5dbd8);background:var(--bs-body-bg,#fff);
    color:var(--bs-secondary-color,#5c6b64);transition:all .15s ease}
  .ngcd-wiz-step:not(:last-child)::after{content:\"\";position:absolute;top:15px;
    left:50%;width:100%;height:2px;background:var(--bs-border-color,#d5dbd8);z-index:0}
  .ngcd-wiz-step .dot{position:relative;z-index:1}
  .ngcd-wiz-step.is-current .dot{border-color:var(--bs-primary,#00583d);
    background:var(--bs-primary,#00583d);color:#fff}
  .ngcd-wiz-step.is-done .dot{border-color:var(--bs-primary,#00583d);
    color:var(--bs-primary,#00583d)}
  .ngcd-wiz-step.is-current{color:var(--bs-primary,#00583d);font-weight:600}
  .ngcd-wiz-summary .grp{font-size:.72rem;text-transform:uppercase;letter-spacing:.04em;
    color:var(--bs-secondary-color,#5c6b64);margin:14px 0 4px}
  .ngcd-wiz-summary .row-kv{display:flex;justify-content:space-between;gap:12px;
    padding:3px 0;border-bottom:1px solid var(--bs-border-color,#eef1f0);font-size:.85rem}
  .ngcd-wiz-summary .row-kv .k{color:var(--bs-secondary-color,#5c6b64)}
  .ngcd-wiz-summary .row-kv .v{font-weight:600;text-align:right}
  .ngcd-wiz-chip{display:inline-block;background:var(--bs-tertiary-bg,#eef2f0);
    border-radius:10px;padding:1px 8px;margin:0 2px 2px 0;font-size:.78rem}
  .ngcd-wiz-nav{display:flex;justify-content:space-between;gap:8px;margin-top:14px}
  "))
}

# ---- UI builders -----------------------------------------------------------

# Numbered stepper. `current` is 1-based. Completed steps show a check (done
# style); the current step is filled; later steps are muted.
ngcd_wizard_stepper <- function(steps, current) {
  n <- length(steps)
  current <- ngcd_wiz_clamp(current, n)
  done <- ngcd_wiz_completed(current, n)
  items <- lapply(seq_len(n), function(i) {
    cls <- "ngcd-wiz-step"
    if (i == current) cls <- paste(cls, "is-current")
    else if (done[i]) cls <- paste(cls, "is-done")
    label <- steps[[i]]$label
    if (isTRUE(steps[[i]]$optional)) label <- paste0(label, "*")
    shiny::tags$li(class = cls,
      shiny::div(class = "dot", if (done[i]) shiny::HTML("&#10003;") else as.character(i)),
      shiny::div(class = "lab", label))
  })
  shiny::tags$ol(class = "ngcd-wiz-steps", items)
}

# Run-summary panel. `groups` is a list of list(title=, rows=list(list(k=,v=)))
# where a row's `v` may be a character vector (rendered as chips).
ngcd_wizard_summary <- function(groups = list()) {
  render_v <- function(v) {
    if (length(v) > 1) lapply(v, function(x) shiny::tags$span(class = "ngcd-wiz-chip", x))
    else shiny::tags$span(class = "v", if (length(v)) as.character(v) else "—")
  }
  body <- if (!length(groups)) {
    shiny::div(class = "text-muted", style = "font-size:.85rem",
               "Your choices will appear here as you go.")
  } else {
    lapply(groups, function(g) {
      rows <- lapply(g$rows, function(r)
        shiny::div(class = "row-kv",
          shiny::span(class = "k", r$k), shiny::div(render_v(r$v))))
      shiny::tagList(shiny::div(class = "grp", g$title), rows)
    })
  }
  shiny::div(class = "ngcd-wiz-summary", body)
}

# Back / Next (or Finish on the last step) buttons. `ns` namespaces ids when used
# inside a module; defaults to identity for the app's single (non-modular) server.
ngcd_wizard_nav <- function(current, n, ns = identity, finish_label = "Finish") {
  current <- ngcd_wiz_clamp(current, n)
  back <- shiny::actionButton(ns("ngcd_wiz_back"), "← Back",
    class = "btn-outline-secondary btn-sm", disabled = if (current <= 1) NA else NULL)
  fwd <- if (current >= n)
    shiny::actionButton(ns("ngcd_wiz_finish"), finish_label, class = "btn-primary btn-sm")
  else
    shiny::actionButton(ns("ngcd_wiz_next"), "Next →", class = "btn-primary btn-sm")
  shiny::div(class = "ngcd-wiz-nav", back, fwd)
}

# The two-column shell: left = stepper + step body + nav; right = summary card.
ngcd_wizard_shell <- function(stepper, body, nav, summary) {
  bslib::layout_columns(
    col_widths = c(8, 4),
    bslib::card(
      bslib::card_header("Guided setup"),
      bslib::card_body(stepper, shiny::hr(), body, nav)),
    bslib::card(
      bslib::card_header("Run summary"),
      bslib::card_body(summary))
  )
}

# ---- server wiring ---------------------------------------------------------

# Wire the wizard into a (non-modular) server. Renders three outputs
# ("ngcd_wiz_stepper", "ngcd_wiz_body", "ngcd_wiz_summary") and handles
# Back/Next/Finish. Returns a list with the reactive `current` (1-based index)
# and `step_id` (character), plus `go(id)` to jump to a step by id.
#
#   body_fn(step_id)  -> UI for that step's body
#   summary_fn()      -> groups list for ngcd_wizard_summary() (reactive-safe)
#   on_finish()       -> optional callback when Finish is pressed
ngcd_wizard_init <- function(input, output, session, steps = ngcd_wizard_steps(),
                             body_fn, summary_fn = function() list(),
                             on_finish = NULL) {
  n <- length(steps)
  cur <- shiny::reactiveVal(1L)

  shiny::observeEvent(input$ngcd_wiz_next,  cur(ngcd_wiz_go(cur(),  1L, n)))
  shiny::observeEvent(input$ngcd_wiz_back,  cur(ngcd_wiz_go(cur(), -1L, n)))
  if (!is.null(on_finish))
    shiny::observeEvent(input$ngcd_wiz_finish, on_finish())

  step_id <- shiny::reactive(steps[[ngcd_wiz_clamp(cur(), n)]]$id)

  output$ngcd_wiz_stepper <- shiny::renderUI(ngcd_wizard_stepper(steps, cur()))
  output$ngcd_wiz_body    <- shiny::renderUI(body_fn(step_id()))
  output$ngcd_wiz_summary <- shiny::renderUI(ngcd_wizard_summary(summary_fn()))
  output$ngcd_wiz_nav     <- shiny::renderUI(ngcd_wizard_nav(cur(), n))

  list(
    current = cur,
    step_id = step_id,
    go = function(id) {
      i <- ngcd_wiz_index_of(steps, id)
      if (!is.na(i)) cur(ngcd_wiz_clamp(i, n))
    }
  )
}

# UI slot the app drops into a nav_panel/page to host the wizard outputs.
ngcd_wizard_outlet <- function() {
  shiny::tagList(
    ngcd_wizard_css(),
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        bslib::card_header("Guided setup"),
        bslib::card_body(
          shiny::uiOutput("ngcd_wiz_stepper"),
          shiny::hr(),
          shiny::uiOutput("ngcd_wiz_body"),
          shiny::uiOutput("ngcd_wiz_nav"))),
      bslib::card(
        bslib::card_header("Run summary"),
        bslib::card_body(shiny::uiOutput("ngcd_wiz_summary"))))
  )
}

# ---- in-app preview (Task 1 seam; bodies filled by Tasks 2-9) --------------

# Placeholder step body used while the guided flow is landed incrementally. Each
# later task swaps a real step body in here (composing the existing input
# builders); until then this shows what the step will hold.
ngcd_wizard_preview_body <- function(id, steps = ngcd_wizard_steps()) {
  i <- ngcd_wiz_index_of(steps, id)
  lab <- if (!is.na(i)) steps[[i]]$label else id
  shiny::tagList(
    shiny::h5(lab),
    shiny::div(class = "help-hint",
      "This step is being migrated into the guided flow. Its controls (and every ",
      "backend parameter) live on the classic tabs today and will appear here as ",
      "the redesign lands — no functionality is removed.")
  )
}

# Live run-summary built from REAL inputs where present (safe on NULLs), so the
# preview reflects the user's actual choices rather than fake data.
ngcd_wizard_preview_summary <- function(input) {
  g <- function(id, default = NULL) {
    v <- tryCatch(input[[id]], error = function(e) NULL)
    if (is.null(v) || (is.character(v) && !length(v))) default else v
  }
  wf <- switch(as.character(g("workflow", "standard")),
               standard = "Standard (diploid)",
               polyploid = "Autotetraploid",
               subgenome = "Disomic-subgenome",
               as.character(g("workflow", "standard")))
  rows_basic <- list(list(k = "Workflow", v = wf),
                     list(k = "Data source", v = g("data_source", "—")))
  traits <- g("traits_to_use")
  if (!is.null(traits)) rows_basic <- c(rows_basic, list(list(k = "Traits", v = traits)))
  ncr <- g("n_crosses")
  rows_alloc <- if (!is.null(ncr)) list(list(k = "Crosses", v = as.character(ncr))) else list()
  groups <- list(list(title = "Basic information", rows = rows_basic))
  if (length(rows_alloc)) groups <- c(groups, list(list(title = "Allocation", rows = rows_alloc)))
  groups
}

# ---- runnable demo ---------------------------------------------------------

# Standalone demo so the shell can be seen without the rest of the app or a
# backend. `shiny::runApp(ngcd_wizard_demo())` or paste into an app.R.
ngcd_wizard_demo <- function() {
  steps <- ngcd_wizard_steps()
  ui <- bslib::page_fillable(
    theme = bslib::bs_theme(version = 5, primary = "#00583d"),
    ngcd_wizard_outlet())
  server <- function(input, output, session) {
    body_fn <- function(id) {
      lab <- steps[[ngcd_wiz_index_of(steps, id)]]$label
      shiny::tagList(
        shiny::h5(lab),
        shiny::p(class = "text-muted",
                 sprintf("Placeholder body for the '%s' step.", lab)))
    }
    summary_fn <- function() list(
      list(title = "Basic information",
           rows = list(list(k = "Workflow", v = "Standard"),
                       list(k = "Traits", v = c("Protein", "B_glucan")))),
      list(title = "Data",
           rows = list(list(k = "Genotype", v = "demo (10x13)"))))
    ngcd_wizard_init(input, output, session, steps, body_fn, summary_fn)
  }
  shiny::shinyApp(ui, server)
}
