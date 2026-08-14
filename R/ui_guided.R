# ---------------------------------------------------------------------------
# Guided navigation layer (redesign Task 2, low-risk track)
#
# Turns the EXISTING top-level tabset (page_navbar id = "nav") into a guided,
# one-screen-at-a-time flow: a numbered stepper + Back/Next drive tab selection
# and the raw navbar links are hidden. No input is moved, renamed, or duplicated,
# so every one of the 141 parameters keeps working exactly as today and the
# server is untouched. Off unless getOption("ngcd.wizard", FALSE).
#
# Depends on ngcd_wizard_stepper()/ngcd_wiz_* from R/wizard.R.
# ---------------------------------------------------------------------------

# Ordered guided steps = the real top-level nav targets, with friendly labels.
# `dev` mirrors workbench_ui(dev=): the Setup tab only exists in developer mode.
ngcd_guided_nav_steps <- function(dev = FALSE) {
  steps <- list()
  if (isTRUE(dev)) steps <- c(steps, list(list(id = "Setup", label = "Setup")))
  c(steps, list(
    list(id = "Data",      label = "Data"),
    list(id = "Configure", label = "Configure"),
    list(id = "Run",       label = "Run"),
    list(id = "Results",   label = "Results")))
}

# Index (1-based) of the currently-selected nav value within the steps; 1 if the
# value is unknown/NULL (e.g. before the first flush).
ngcd_guided_nav_index <- function(steps, current) {
  ids <- vapply(steps, function(s) s$id, character(1))
  m <- if (is.null(current)) NA_integer_ else match(current, ids)
  if (is.na(m)) 1L else as.integer(m)
}

# The nav target `delta` steps away from `current` (clamped within the steps).
ngcd_guided_nav_target <- function(steps, current, delta) {
  n <- length(steps)
  i <- ngcd_wiz_clamp(ngcd_guided_nav_index(steps, current) + as.integer(delta), n)
  steps[[i]]$id
}

# CSS for the guided bar only. IMPORTANT: this layer is purely ADDITIVE - the
# real navbar stays fully visible and clickable, so a user can never be stranded
# even if programmatic tab-switching behaves differently across bslib versions.
ngcd_guided_css <- function() {
  shiny::tags$style(shiny::HTML("
  .ngcd-guided-bar{background:var(--bs-body-bg,#fff);
    border-bottom:1px solid var(--bs-border-color,#e6eae8);
    padding:10px 16px 8px;margin-bottom:10px}
  .ngcd-guided-bar .ngcd-guided-row{display:flex;justify-content:space-between;
    align-items:center;gap:12px;margin-top:8px;flex-wrap:wrap}
  .ngcd-guided-bar .ngcd-guided-nav{display:flex;gap:8px}
  .ngcd-guided-bar .ngcd-guided-summary{font-size:.85rem;
    color:var(--bs-secondary-color,#5c6b64)}
  .ngcd-guided-bar .ngcd-guided-summary-label{font-weight:600;
    color:var(--bs-primary,#00583d);margin-right:4px}
  "))
}

# The guided bar: a progress stepper + Back/Next, shown above the tab content.
# Rendered from a uiOutput the server refreshes as the current tab changes.
# `hide_nav` hides the raw navbar links for the minimalist look (the clickable
# stepper + Back/Next remain, so you are never stranded). Escape hatch:
# options(ngcd.wizard.hidenav = FALSE) keeps the tabs visible.
ngcd_guided_bar_ui <- function(hide_nav = isTRUE(getOption("ngcd.wizard.hidenav", TRUE))) {
  hide_css <- if (isTRUE(hide_nav))
    shiny::tags$style(shiny::HTML(".navbar .navbar-nav{display:none !important}")) else NULL
  shiny::tagList(
    ngcd_wizard_css(),   # the stepper's dot/flex styling (defined in wizard.R)
    ngcd_guided_css(),
    hide_css,
    shiny::div(class = "ngcd-guided-bar",
      shiny::uiOutput("ngcd_guided_stepper"),
      shiny::div(class = "ngcd-guided-row",
        shiny::div(class = "ngcd-guided-summary",
          shiny::tags$span(class = "ngcd-guided-summary-label", "Summary:"),
          shiny::textOutput("ngcd_guided_summary", inline = TRUE)),
        shiny::div(class = "ngcd-guided-nav",
          shiny::actionButton("ngcd_guided_back", "< Back",
            class = "btn-outline-secondary btn-sm"),
          shiny::actionButton("ngcd_guided_next", "Next >",
            class = "btn-primary btn-sm"))))
  )
}

# Move the tabset by `delta`, robustly across bslib versions: prefer nav_select,
# fall back to updateTabsetPanel if that errors.
ngcd_guided_move <- function(session, steps, nav_id, current, delta) {
  target <- ngcd_guided_nav_target(steps, current, delta)
  tryCatch(bslib::nav_select(nav_id, target, session = session),
           error = function(e)
             shiny::updateTabsetPanel(session, nav_id, selected = target))
}

# ---- finer guided flow: flat steps spanning outer tabs + Configure sub-tabs --
#
# One-screen-at-a-time across BOTH the top-level tabset ("nav") and the Configure
# sub-tabset ("cfg_nav"). Each flat step is list(id, label, outer, inner) where
# `inner` (a Configure sub-tab title) is NULL for top-level-only steps. Still
# purely additive: the real tabs stay clickable.

ngcd_guided_flat_steps <- function(dev = FALSE) {
  s <- list()
  if (isTRUE(dev)) s <- c(s, list(list(id = "setup", label = "Setup", outer = "Setup", inner = NULL)))
  c(s, list(
    list(id = "data",       label = "Data",       outer = "Data",      inner = NULL),
    list(id = "objective",  label = "Objective",  outer = "Configure", inner = "Selection objective"),
    list(id = "scoring",    label = "Scoring",     outer = "Configure", inner = "Prediction & scoring"),
    list(id = "filters",    label = "Filters",    outer = "Configure", inner = "Cross filters & genetic constraints"),
    list(id = "allocation", label = "Allocation", outer = "Configure", inner = "Mate allocation"),
    list(id = "outputs",    label = "Outputs",    outer = "Configure", inner = "Export options"),
    list(id = "run",        label = "Run",        outer = "Run",       inner = NULL),
    list(id = "results",    label = "Results",    outer = "Results",   inner = NULL)))
}

# Current flat index from the two nav values. Exact (outer, inner) match wins;
# then an outer-only step; then the first step of that outer; else 1.
ngcd_guided_flat_index <- function(steps, outer, inner = NULL) {
  if (is.null(outer)) return(1L)
  for (i in seq_along(steps)) { s <- steps[[i]]
    if (identical(s$outer, outer) && !is.null(s$inner) && identical(s$inner, inner)) return(i) }
  for (i in seq_along(steps)) { s <- steps[[i]]
    if (identical(s$outer, outer) && is.null(s$inner)) return(i) }
  for (i in seq_along(steps)) if (identical(steps[[i]]$outer, outer)) return(i)
  1L
}

# The flat step `delta` away (clamped). Pure.
ngcd_guided_flat_target <- function(steps, outer, inner, delta) {
  i <- ngcd_wiz_clamp(ngcd_guided_flat_index(steps, outer, inner) + as.integer(delta), length(steps))
  steps[[i]]
}

# Apply a target: select the outer tab and (if any) the Configure sub-tab,
# robustly across bslib versions.
ngcd_guided_flat_apply <- function(session, target, nav_id = "nav", cfg_id = "cfg_nav") {
  sel <- function(id, val) tryCatch(bslib::nav_select(id, val, session = session),
    error = function(e) tryCatch(shiny::updateTabsetPanel(session, id, selected = val),
                                 error = function(e2) NULL))
  sel(nav_id, target$outer)
  if (!is.null(target$inner)) sel(cfg_id, target$inner)
}

# Wire the guided bar into a (non-modular) server: renders the stepper reflecting
# the current (outer, inner) tab and moves through the flat steps on Back/Next.
ngcd_guided_nav_init <- function(input, output, session, dev = FALSE,
                                 nav_id = "nav", cfg_id = "cfg_nav",
                                 res_fn = function() NULL) {
  steps <- ngcd_guided_flat_steps(dev)
  output$ngcd_guided_stepper <- shiny::renderUI(
    ngcd_wizard_stepper(steps, ngcd_guided_flat_index(steps, input[[nav_id]], input[[cfg_id]]),
                        click_input = "ngcd_guided_goto"))
  # compact run-summary line in the bar (safe: header-only, no sidebar)
  output$ngcd_guided_summary <- shiny::renderText(
    ngcd_guided_summary_line(input, has_result = !is.null(res_fn())))
  # clickable stepper: jump straight to a step by id
  shiny::observeEvent(input$ngcd_guided_goto, {
    i <- match(input$ngcd_guided_goto, vapply(steps, function(s) s$id, character(1)))
    if (!is.na(i)) ngcd_guided_flat_apply(session, steps[[i]], nav_id, cfg_id)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$ngcd_guided_next,
    ngcd_guided_flat_apply(session,
      ngcd_guided_flat_target(steps, input[[nav_id]], input[[cfg_id]],  1L), nav_id, cfg_id),
    ignoreInit = TRUE)
  shiny::observeEvent(input$ngcd_guided_back,
    ngcd_guided_flat_apply(session,
      ngcd_guided_flat_target(steps, input[[nav_id]], input[[cfg_id]], -1L), nav_id, cfg_id),
    ignoreInit = TRUE)
  invisible(steps)
}

# ---- live run-summary (guided sidebar) -------------------------------------
# Build grouped key/value rows for ngcd_wizard_summary() from the user's REAL
# inputs + run status. Safe on NULL inputs (renders "-" / omits absent rows).
ngcd_guided_summary_groups <- function(input, has_result = FALSE) {
  g <- function(id, d = NULL) {
    v <- tryCatch(input[[id]], error = function(e) NULL)
    if (is.null(v) || (is.character(v) && !length(v))) d else v
  }
  wf <- switch(as.character(g("workflow", "standard")),
               standard = "Standard (diploid)", polyploid = "Autotetraploid",
               subgenome = "Disomic-subgenome", as.character(g("workflow", "standard")))
  ds <- switch(as.character(g("data_source", "demo")),
               demo = "Bundled demo", upload = "Uploaded CSVs", as.character(g("data_source", "-")))
  obj <- switch(as.character(g("objective_mode", "single")),
                single = "Single trait", multi = "Multi-trait index",
                as.character(g("objective_mode", "-")))
  basic <- list(list(k = "Workflow", v = wf),
                list(k = "Data source", v = ds),
                list(k = "Objective", v = obj))
  traits <- g("traits_to_use")
  if (!is.null(traits) && length(traits)) basic <- c(basic, list(list(k = "Traits", v = traits)))

  alloc <- list()
  nc <- g("n_crosses"); if (!is.null(nc)) alloc <- c(alloc, list(list(k = "Crosses", v = as.character(nc))))
  mu <- g("max_uses_per_parent"); if (!is.null(mu)) alloc <- c(alloc, list(list(k = "Max uses / parent", v = as.character(mu))))

  groups <- list(list(title = "Setup", rows = basic))
  if (length(alloc)) groups <- c(groups, list(list(title = "Allocation", rows = alloc)))
  groups <- c(groups, list(list(title = "Run",
    rows = list(list(k = "Result", v = if (isTRUE(has_result)) "ready" else "not run yet")))))
  groups
}

# Compact one-line run summary for the guided bar (ASCII separators). Safe on
# NULL inputs. Rendered as text in the header - no layout risk (unlike a
# page_navbar sidebar, which broke on newer bslib).
ngcd_guided_summary_line <- function(input, has_result = FALSE) {
  g <- function(id, d = NULL) {
    v <- tryCatch(input[[id]], error = function(e) NULL)
    if (is.null(v) || (is.character(v) && !length(v))) d else v
  }
  wf <- switch(as.character(g("workflow", "standard")),
               standard = "Standard", polyploid = "Autotetraploid",
               subgenome = "Disomic-subgenome", as.character(g("workflow", "standard")))
  ds <- switch(as.character(g("data_source", "demo")),
               demo = "demo data", upload = "uploaded CSVs", as.character(g("data_source", "-")))
  parts <- c(wf, ds)
  tr <- g("traits_to_use")
  if (!is.null(tr) && length(tr))
    parts <- c(parts, paste0(length(tr), " trait", if (length(tr) != 1) "s" else ""))
  nc <- g("n_crosses"); if (!is.null(nc)) parts <- c(parts, paste0(nc, " crosses"))
  parts <- c(parts, if (isTRUE(has_result)) "result ready" else "not run yet")
  paste(parts, collapse = "  |  ")
}
