#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# record-guided-demo.R  -  serve the guided workbench UI, populated for capture
#
# Launches the REAL workbench UI in the guided view (options(ngcd.wizard = TRUE))
# and feeds a small, schema-correct demo result into the Results / Modelling
# graphics panel, so every screen renders populated WITHOUT a live backend. Use
# it to regenerate the README/vignette screenshots and the demo movies.
#
# Usage:
#   NGCD_REC_PORT=7799 Rscript tools/record-guided-demo.R      # serves the app
#
# then, in a separate shell, drive a headless browser (Chrome/Chromium) against
# http://127.0.0.1:7799/ and screenshot each guided step. Any driver works
# (Playwright, Puppeteer, shinytest2/chromote, or a manual browser). Navigate by
# setting the guided step from the page console, e.g.
#   Shiny.setInputValue('ngcd_guided_goto','allocation',{priority:'event'})
# with step ids: data, objective, scoring, filters, allocation, outputs, run,
# results (a leading 'setup' exists in developer mode).
#
# For LIVE results with real backend numbers, run the app normally
# (run_workbench()) with nextgenCrossDesign installed and capture a real run;
# this harness is only for the front-end / guided-view figures.
# ---------------------------------------------------------------------------

options(ngcd.wizard = TRUE)
suppressMessages(library(nextgenCrossWorkbench))
ns  <- getNamespace("nextgenCrossWorkbench")
dir <- Sys.getenv("NGCD_REC_DIR", unset = getwd())
port <- as.integer(Sys.getenv("NGCD_REC_PORT", unset = "7799"))
dev  <- identical(Sys.getenv("NGCD_REC_DEV", unset = "0"), "1")

# --- schema-correct demo result (parents P01..P10, traits yield/disease) -----
make_demo_result <- function() {
  set.seed(7)
  ids <- sprintf("P%02d", 1:10)
  pr  <- t(utils::combn(ids, 2)); colnames(pr) <- c("parent1", "parent2")
  n   <- nrow(pr)
  yield   <- round(rnorm(n, 62, 4.2), 2)
  disease <- round(rnorm(n, 3.1, 0.8), 2)
  score   <- round(scale(yield)[, 1] * 1.6 - scale(disease)[, 1] * 1.0 + rnorm(n, 0, 0.3) + 6, 2)
  kin     <- round(pmin(pmax(rbeta(n, 2, 6), 0.02), 0.85), 3)
  cc <- data.frame(pr, multi_trait_score = score, pair_kinship = kin,
                   yield_value = yield, disease_value = disease,
                   stringsAsFactors = FALSE)
  sel <- order(score - 3 * kin, decreasing = TRUE)[1:10]
  sc  <- cc[sel, , drop = FALSE]
  sc$cross_confidence <- round(pmin(pmax(rnorm(10, 0.72, 0.12), 0.3), 0.98), 2)
  sc$cross_upside     <- round(abs(rnorm(10, 2.1, 0.6)), 2)
  sc$risk_bin         <- factor(sample(c("low", "moderate", "elevated"), 10, TRUE, c(.5, .35, .15)))
  sc$priority_tier    <- rep(c("Tier 1", "Tier 2", "Tier 3"), length.out = 10)
  es <- data.frame(trait = c("yield", "disease"),
                   marker_effect_reliability = c(0.78, 0.63),
                   direction = c("maximize", "minimize"), stringsAsFactors = FALSE)
  list(candidate_crosses = cc, selected_crosses = sc, effect_summary = es)
}
DEMO <- make_demo_result()

cfg <- ns$ngcd_load_config(dir)
cfg$backend_registry <- NULL              # UI falls back to built-in choices
shiny::addResourcePath("ngcd_www", cfg$www_dir)
if (!dir.exists(cfg$report_dir)) dir.create(cfg$report_dir, recursive = TRUE, showWarnings = FALSE)
shiny::addResourcePath("ngcd_report", cfg$report_dir)

ui <- ns$workbench_ui(cfg, dev = dev)

server <- function(input, output, session) {
  `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
  ns$ngcd_guided_nav_init(input, output, session, dev = dev, res_fn = function() DEMO)
  # guided sequential modelling-graphics navigation (mirrors workbench_server)
  mg_step <- shiny::reactiveVal(1L); mg_n <- length(ns$ngcd_mg_steps())
  shiny::observeEvent(input$mg_next, mg_step(min(mg_n, mg_step() + 1L)))
  shiny::observeEvent(input$mg_prev, mg_step(max(1L, mg_step() - 1L)))
  shiny::observeEvent(input$mg_goto, { i <- suppressWarnings(as.integer(input$mg_goto))
    if (length(i) == 1L && !is.na(i)) mg_step(max(1L, min(mg_n, i))) })
  output$mg_guided <- shiny::renderUI(ns$ngcd_mg_guided_panel(mg_step()))
  output$mg_trait_ui <- shiny::renderUI({
    traits <- sub("_value$", "", ns$ngcd_trait_value_cols(DEMO$candidate_crosses))
    shiny::selectInput("mg_trait", "Distribution trait",
      choices = c("Overall cross score" = "", stats::setNames(traits, traits)), width = "320px")
  })
  output$mg_dist  <- plotly::renderPlotly({
    tr <- if (isTRUE(nzchar(input$mg_trait %||% ""))) input$mg_trait else NULL
    ns$ngcd_chart_cross_scores(DEMO$candidate_crosses, trait = tr) })
  output$mg_ridge <- plotly::renderPlotly(ns$ngcd_chart_cross_scores_ridge(DEMO$candidate_crosses))
  output$mg_conf  <- plotly::renderPlotly(ns$ngcd_chart_cross_confidence(DEMO$selected_crosses))
  output$mg_div   <- plotly::renderPlotly(ns$ngcd_chart_cross_diversity(DEMO$candidate_crosses, DEMO$selected_crosses))
  output$mg_reliab<- plotly::renderPlotly(ns$ngcd_chart_trait_reliability(DEMO$effect_summary))
  output$res_selected  <- DT::renderDT(DT::datatable(DEMO$selected_crosses,
    options = list(dom = "t", pageLength = 10), rownames = FALSE))
  output$res_candidate <- DT::renderDT(DT::datatable(DEMO$candidate_crosses,
    options = list(pageLength = 12), rownames = FALSE))
}

message("Serving guided demo on http://127.0.0.1:", port, "  (dev=", dev, ")")
shiny::runApp(shiny::shinyApp(ui, server), launch.browser = FALSE, port = port, host = "127.0.0.1")
