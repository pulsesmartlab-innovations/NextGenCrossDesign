# Stage-centric Run pipeline — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: use superpowers:executing-plans (inline) to implement
> this plan task-by-task. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Make the Run tab a stepped pipeline where each stage (QC · Fit & score · Build index ·
Allocate & rank) has its own run button, status, one-line summary, and its figure(s) shown inline —
reusing the existing compute-once engine untouched — and move per-stage diagnostics out of the buried
Configure tags and out of Results.

**Architecture:** UI reorganization of `R/app.R` (Run/Configure/Results panels) + two pure helpers in
`R/helpers.R`. The pipeline engine (`ngcd_pipeline_mark`, `ngcd_next_stages`, `run_stage_manual`,
`ngcd_run_stage`, stage JSON) is NOT modified. The Run tab switches between the stepped cards and the
single-shot Run card via a server `run_mode` reactive feeding `conditionalPanel`.

**Tech stack:** R, Shiny, bslib, plotly, testthat (`testServer`).

## Global Constraints

- **Compute-once (load-bearing):** never re-run a completed stage. Reuse `run_stage_manual()` (one
  stage) and `do_run_pipeline()` (walks only non-done stages). Do NOT touch `R/helpers.R` pipeline
  functions or the staleness observer at `app.R:1320`.
- **Diagnostics stay at stage level:** QC-duplicate, trait-reliability, index-distribution figures live
  ONLY in their cards. Only final-plan-relevant output stays in Results.
- **No new hard dependency.** Button enable/disable stays via `disable_if()` (no shinyjs). Figures use
  `uiOutput`/`plotlyOutput`.
- **Input-ID conservation:** the only NEW Shiny input id is `run_all`. `run_qc`/`run_predict`/
  `run_index`/`run` keep their ids and are declared exactly once (in their existing `output$*_ui`
  renderers — do not duplicate them). Snapshot ids before, diff after; expect only `run_all` added.
- **Reuse existing server outputs:** `run_qc_ui`, `run_predict_ui`, `run_index_ui`, `run_button_ui`,
  `fig_qc`, `fig_predict`, `fig_index`, `fig_allocate`, and their `*_plot` renderers already exist —
  the cards reference them; do not redefine them.

---

### Task 1: Pure helper `ngcd_stage_summary()` + tests

One-line summary per stage from its stored stage JSON (`rv$pipeline$stages[[s]]$json`), pure and
unit-testable. Shapes are the documented ones (phase3-plan.md): qc(`status`,`issues`), predict
(`effect_summary` df, `n_candidates`), index(`multi_trait_score`, `objective$method`), allocate
(`plan_summary` list incl. `mean_gain`,`group_coancestry`; `parent_use` df).

**Files:**
- Modify: `R/helpers.R` (add function near the other `ngcd_stage_*` helpers, ~line 746)
- Test: `tests/testthat/test-run-cards.R` (new)

**Interfaces:**
- Produces: `ngcd_stage_summary(stage, json) -> character(1)`. `stage ∈ {"qc","predict","index",
  "allocate"}`. `NULL`/empty json → `"Not run yet."`

- [ ] **Step 1: Write the failing test** — `tests/testthat/test-run-cards.R`

```r
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
  pr <- list(effect_summary = data.frame(trait = c("yield","protein","disease")),
             n_candidates = 45)
  expect_match(ss("predict", pr), "3 traits")
  expect_match(ss("predict", pr), "45 candidate")

  # index: method
  ix <- list(multi_trait_score = c(1,2,3), objective = list(method = "economic_index"))
  expect_match(ss("index", ix), "economic_index")

  # allocate: crosses + mean gain
  al <- list(plan_summary = list(n_crosses = 12, mean_gain = 3.4, group_coancestry = 0.21))
  expect_match(ss("allocate", al), "12 cross")
  expect_match(ss("allocate", al), "3.4")
})
```

- [ ] **Step 2: Run it, verify it fails** — `Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-run-cards.R")'` → FAIL ("could not find function ngcd_stage_summary").

- [ ] **Step 3: Implement** — add to `R/helpers.R`:

```r
# One-line, human summary of a stage's outcome from its stored stage JSON
# (rv$pipeline$stages[[s]]$json). Pure. NULL / empty json -> "Not run yet.".
ngcd_stage_summary <- function(stage, json) {
  if (is.null(json) || !length(json)) return("Not run yet.")
  n_issue <- function(sev) {
    iss <- json$issues
    if (is.data.frame(iss)) return(sum(iss$severity %in% sev))
    if (is.list(iss)) return(sum(vapply(iss, function(x) isTRUE(x$severity %in% sev), logical(1))))
    0L
  }
  switch(stage,
    qc = sprintf("%d blocker%s, %d warning%s",
                 n_issue("blocker"), if (n_issue("blocker") == 1) "" else "s",
                 n_issue("warning"), if (n_issue("warning") == 1) "" else "s"),
    predict = {
      nt <- if (is.data.frame(json$effect_summary)) nrow(json$effect_summary) else length(json$effect_summary)
      nc <- json$n_candidates %||% NA
      sprintf("%d trait%s scored%s", nt, if (nt == 1) "" else "s",
              if (is.na(nc)) "" else sprintf(" · %d candidate crosses", nc))
    },
    index = sprintf("Selection index (%s)", (json$objective$method %||% "index")),
    allocate = {
      ps <- json$plan_summary %||% list()
      k  <- ps$n_crosses %||% NA; g <- ps$mean_gain %||% NA; c <- ps$group_coancestry %||% NA
      paste(c(if (!is.na(k)) sprintf("%d crosses", k),
              if (!is.na(g)) sprintf("mean gain %.3g", g),
              if (!is.na(c)) sprintf("coancestry %.3g", c)), collapse = " · ")
    },
    "")
}
```

- [ ] **Step 4: Run tests, verify pass.**
- [ ] **Step 5: Commit** — `feat: ngcd_stage_summary one-line stage summaries + tests`

---

### Task 2: Stepped Run tab (cards + summaries + run_all)

Replace the Run panel body with a stepped-cards layout gated by a `run_mode` reactive. Reference the
existing per-stage button/figure outputs; add per-stage summary outputs and the `run_all` button.

**Files:**
- Modify: `R/app.R` — Run `nav_panel` body (currently ~556-586); server: add `output$run_mode`,
  `output$sum_qc/sum_predict/sum_index/sum_allocate`, `run_all` observer (near the other run observers
  ~1724).

**Interfaces:**
- Consumes: `run_qc_ui`,`run_predict_ui`,`run_index_ui`,`run_button_ui`,`fig_qc`,`fig_predict`,
  `fig_index`,`fig_allocate` (existing); `is_poly()`,`is_subgenome()`,`ngcd_run_uses_staged()`,
  `build_params()`,`do_run_pipeline()`,`ngcd_stage_summary()`.

- [ ] **Step 1: Add the `run_mode` reactive + summary outputs + run_all observer** (server, near line 1724):

```r
    # "staged" -> stepped cards; "single" -> one-shot Run card (poly/subgenome,
    # or standard runs that cannot stage: auto cross-number sweep / artifact export).
    output$run_mode <- shiny::reactive({
      if (is_poly() || is_subgenome() || !ngcd_run_uses_staged(build_params())) "single" else "staged"
    })
    shiny::outputOptions(output, "run_mode", suspendWhenHidden = FALSE)

    stage_summary_ui <- function(stage)
      shiny::div(class = "help-hint", style = "margin:4px 0;",
                 ngcd_stage_summary(stage, rv$pipeline$stages[[stage]]$json))
    output$sum_qc       <- shiny::renderUI(stage_summary_ui("qc"))
    output$sum_predict  <- shiny::renderUI(stage_summary_ui("predict"))
    output$sum_index    <- shiny::renderUI(stage_summary_ui("index"))
    output$sum_allocate <- shiny::renderUI(stage_summary_ui("allocate"))

    # one-click walk of the remaining (non-done) stages — compute-once
    shiny::observeEvent(input$run_all, do_run_pipeline())
```

- [ ] **Step 2: Replace the Run `nav_panel` body** (app.R ~556-586). Keep the guide + Save/load block
  (dev) + `run_gate`/`error_panel`/`run_log`; replace the single-button `card` with the cards. New body:

```r
      bslib::nav_panel("Run",
        ngcd_section("Run the analysis", "Run each step in order — each shows its own result and figure."),
        ngcd_guide("Run", "Run", shiny::tagList(
          shiny::tags$p("Run the pipeline one step at a time; each step keeps its result so the next step never re-runs it."),
          shiny::tags$ul(
            shiny::tags$li(shiny::tags$b("Quality control"), " runs first and blocks the design only on blockers."),
            shiny::tags$li("Each step unlocks when the step above it is done; its figure appears in the step."),
            shiny::tags$li(shiny::tags$b("Run all remaining steps"), " walks whatever is left in one click.")),
          shiny::tags$p(class = "help-hint", "Change a setting and only the affected step (and the steps after it) need re-running.")),
          next_hint = "Results — the final plan."),
        if (isTRUE(dev)) bslib::card(bslib::card_header("Save / load settings"),
          shiny::div(class = "help-hint",
            "Save all current settings as a named profile to reload later."),
          bslib::layout_columns(col_widths = c(5, 3, 4),
            shiny::textInput("preset_name", "Profile name", "my-settings"),
            shiny::div(shiny::tags$label(class = "form-label", " "),
              shiny::div(shiny::actionButton("save_preset", "Save profile", class = "btn-ndsu"))),
            shiny::div(shiny::tags$label(class = "form-label", " "),
              shiny::div(shiny::downloadButton("download_settings", "Download .json", class = "btn-outline-secondary")))),
          bslib::layout_columns(col_widths = c(5, 3, 4),
            shiny::uiOutput("preset_pick_ui"),
            shiny::div(shiny::tags$label(class = "form-label", " "),
              shiny::div(shiny::actionButton("load_preset", "Load profile", class = "btn-ndsu"))),
            shiny::fileInput("upload_settings", "...or load a .json file", accept = ".json"))),

        # ---- SINGLE-SHOT card (poly/subgenome or non-stageable standard run) ----
        shiny::conditionalPanel("output.run_mode == 'single'",
          bslib::card(bslib::card_header("Run"),
            shiny::uiOutput("run_button_ui"))),

        # ---- STEPPED pipeline (standard, stageable) ----
        shiny::conditionalPanel("output.run_mode == 'staged'",
          shiny::div(style = "margin-bottom:10px;",
            shiny::actionButton("run_all", "Run all remaining steps", class = "btn-outline-secondary")),
          bslib::card(bslib::card_header("1 · Quality control"),
            shiny::uiOutput("run_qc_ui"), shiny::uiOutput("sum_qc"), shiny::uiOutput("fig_qc")),
          bslib::card(bslib::card_header("2 · Fit effects & score"),
            shiny::uiOutput("run_predict_ui"), shiny::uiOutput("sum_predict"), shiny::uiOutput("fig_predict")),
          shiny::conditionalPanel("input.objective_mode == 'multi'",
            bslib::card(bslib::card_header("3 · Build selection index"),
              shiny::uiOutput("run_index_ui"), shiny::uiOutput("sum_index"), shiny::uiOutput("fig_index"))),
          bslib::card(bslib::card_header("4 · Allocate & rank"),
            shiny::uiOutput("run_button_ui"), shiny::uiOutput("sum_allocate"), shiny::uiOutput("fig_allocate"))),

        bslib::card(
          shiny::uiOutput("run_gate"),
          shiny::uiOutput("error_panel"),
          shiny::tags$hr(), shiny::tags$b("Runner log"),
          shiny::verbatimTextOutput("run_log"))),
```

Note: `run_button_ui` appears in BOTH conditionalPanels but only one is visible at a time, and Shiny
renders one `uiOutput("run_button_ui")` binding per DOM location — that is fine (an output can drive
multiple DOM nodes; both show the same button). The `run` input id is still declared once, inside the
`run_button_ui` renderer.

- [ ] **Step 3: Load the app, smoke-render** — `Rscript -e 'pkgload::load_all("."); cfg <- ngcd_load_config(tempfile("wb")); invisible(nextgenCrossWorkbench:::workbench_ui(cfg))'` → no error.
- [ ] **Step 4: `testServer` smoke** — existing `test-pipeline-state`/`test-server-logic` still pass.
- [ ] **Step 5: Commit** — `feat: stepped Run tab — per-stage cards with summary + inline figure + run_all`

---

### Task 3: Configure/Data → options only; trim Results

Remove the relocated run buttons + figure tags from Data/Configure, and drop the now-duplicated
standalone frontier tab from Results. Keep the live pre-run trait/index preview.

**Files:** Modify `R/app.R` (Data quality ~219-220, Selection objective ~266-268, Prediction &
scoring ~343-344, Mate allocation ~531, Results navset ~609).

- [ ] **Step 1: Remove from Data quality panel** the line `shiny::uiOutput("run_qc_ui"),` and the
  `ngcd_figure_tag("fig_qc", ...)` call (keep the rest of the Data-quality controls).
- [ ] **Step 2: Remove from Selection objective** the `shiny::uiOutput("run_index_ui"),` and
  `ngcd_figure_tag("fig_index", ...)`. KEEP `ngcd_figure_tag("fig_trait_dist", ...)` (live preview).
- [ ] **Step 3: Remove from Prediction & scoring** `shiny::uiOutput("run_predict_ui"),` and
  `ngcd_figure_tag("fig_predict", ...)`.
- [ ] **Step 4: Remove from Mate allocation** `ngcd_figure_tag("fig_allocate", ...)`.
- [ ] **Step 5: Trim Results** — delete the `bslib::nav_panel("Gain-diversity frontier",
  shiny::uiOutput("res_frontier_ui")),` line from the Results navset (now shown in the Allocate card).
  Leave `res_frontier_ui`/`res_frontier` server code in place (harmless, unbound) OR delete it too;
  simplest: leave the server output (no dangling binding error since a render without an output is a
  no-op). Keep Portfolio & risk, Pareto, QC audit, everything else.
- [ ] **Step 6: Verify** app renders (Step 3 of Task 2 command) and grep confirms the removed ids are
  gone from the UI section but the button ids still exist once (in the server renderers):
  `grep -c 'uiOutput("run_qc_ui")' R/app.R` → 1 (only the Run card).
- [ ] **Step 7: Commit** — `refactor: Configure = options only; move stage figures to Run cards; trim Results`

---

### Task 4: Verification — compute-once test + input-id conservation + gate

**Files:** `tests/testthat/test-run-cards.R` (extend), plus run the full gate.

- [ ] **Step 1: Snapshot baseline input ids** BEFORE any UI change was a prerequisite — capture now from
  `origin/main` for reference:
  `git show origin/main:R/app.R > /tmp/app-main.R` then extract input ids (actionButton/selectInput/…
  first-arg) from both and diff. Expect: only `run_all` added, nothing removed (the moved buttons keep
  their ids).

- [ ] **Step 2: Write the compute-once `testServer` test** (append to `test-run-cards.R`):

```r
test_that("running a later step never re-runs completed earlier steps", {
  srv <- nextgenCrossWorkbench:::workbench_server(
    nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb")))
  testthat::skip_on_cran()   # drives the backend via Rscript
  testServer(srv, {
    session$setInputs(!!!demo_inputs())          # load demo data + defaults
    # simulate qc + predict already done by stamping pipeline state directly,
    # then assert an allocation-only input change marks allocate/rank stale but
    # leaves qc/predict done (compute-once), via the existing staleness engine.
    rv$pipeline$stages$qc$status      <- "done"
    rv$pipeline$stages$qc$cfg         <- nextgenCrossWorkbench:::ngcd_stage_cfg_subset(build_params(), "qc")
    rv$pipeline$stages$predict$status <- "done"
    rv$pipeline$stages$predict$cfg    <- nextgenCrossWorkbench:::ngcd_stage_cfg_subset(build_params(), "predict")
    session$setInputs(n_crosses = 999)           # allocation-only setting
    session$flushReact()
    expect_equal(rv$pipeline$stages$qc$status, "done")       # untouched
    expect_equal(rv$pipeline$stages$predict$status, "done")  # untouched
    expect_true(rv$pipeline$stages$allocate$status %in% c("stale","blocked"))
  })
})
```

(If `n_crosses` is not in the allocate cfg subset, use the actual allocate-only key from
`ngcd_stage_key_patterns$allocate`; verify with
`Rscript -e 'pkgload::load_all("."); print(nextgenCrossWorkbench:::ngcd_stage_key_patterns$allocate)'`
and pick one, e.g. `max_pair_kinship`.)

- [ ] **Step 3: Run the new tests** — `Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-run-cards.R")'` → PASS.
- [ ] **Step 4: Full suite** — `Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat")'`; no NEW failures vs the known pre-existing ones.
- [ ] **Step 5: R CMD check via CI** — push branch, open PR; the R-CMD-check workflow must be green.
- [ ] **Step 6: Manual smoke** — build the app object; confirm the served HTML has the four card headers
  ("1 · Quality control" … "4 · Allocate & rank") and the `run_all` button, and no longer renders the
  moved figure tags in Configure.
- [ ] **Step 7: Commit** — `test: compute-once + input-id conservation for stepped Run`

---

## Self-review

- **Spec coverage:** stepped cards (T2), per-stage run+figure+summary (T1/T2), move-out-of-Configure
  (T3), Results trim (T3), compute-once preserved+tested (T4), adaptive single/multi/poly (T2
  conditionalPanel), run_all (T2). ✓
- **Placeholders:** none — code is concrete; the one lookup (allocate cfg key) has an explicit verify
  command and fallback.
- **Type consistency:** `ngcd_stage_summary(stage, json) -> character(1)` used consistently; card
  outputs are `uiOutput`/`plotlyOutput` (no new input ids beyond `run_all`).
