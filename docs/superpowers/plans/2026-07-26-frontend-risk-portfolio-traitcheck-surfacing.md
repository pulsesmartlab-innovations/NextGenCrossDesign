# Frontend surfacing: risk/portfolio + trait-check veto — implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: use superpowers:subagent-driven-development or
> superpowers:executing-plans to implement task-by-task. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Surface, in the Shiny frontend (`NextGenCrossDesign`), the two backend features whose UI
was deferred: (A) the **risk/portfolio decision layer** (backend 0.13.0) and (B) the **per-trait
check-threshold veto** (backend 0.14.0). The backend already emits the data; this adds the runner
passthrough, diagnostics helpers, a portfolio scatter, and the trait-check inputs.

**Architecture:** Same patterns the app already uses — `R/diagnostics.R` engine (new
`ngcd_diag_*` helpers auto-render in the Report via `ngcd_diagnostics_html`), **plotly** for the
scatter (mirroring `R/report.R`'s existing candidate scatter), **DT** tables (new per-cross columns
ride on `selected_crosses`/`candidate_crosses` automatically), `build_params` reactive
(R/app.R:1010) for run inputs, and the frontend runner envelope
(`inst/app/tools/run_cross_prediction_json.R`) `pick()` list. Tests are **testthat**
(`tests/testthat/`), mirroring `test-diagnostics.R`.

**Tech stack:** R, Shiny, bslib, DT, plotly, testthat.

## Global Constraints

- **Forward-compatible / graceful absence:** every helper must no-op (return `list()` / hide the
  panel) when its field/columns are absent — risk/portfolio columns (`cross_level`, `cross_upside`,
  `risk_bin`, `portfolio_profile`, `cross_confidence`, `confidence_method`) exist **only for
  single-trait runs** (backend 0.13.0 gate); `priority_risk_diagnostics` / `trait_check_diagnostics`
  are NULL on older backends or when the feature is unused.
- **No backend logic in the frontend** — read the JSON result contract only. The app reads
  `res$selected_crosses` columns + the two diagnostics blocks.
- **Portfolio axes (match backend/design):** x = `cross_upside` (√VPM genetic SD), y = `cross_level`
  (`cross_mean_gebv`), colour = `risk_bin` (low=green/med=amber/high=red), hover = cross id + tier +
  `portfolio_profile` + `cross_confidence`. **Iso-genetic-usefulness contours** `μ = U − i·σ`
  (slope −i), `i = ng_selection_intensity(selection_prop)` computed frontend-side as
  `dnorm(qnorm(1−p))/p` from `res$settings$selection_prop`. Label them "iso-genetic-usefulness
  (UC-VPM)"; state on the plot that upside = genetic opportunity, colour = estimation confidence
  (never conflate). Do NOT claim the plane reproduces a UC-PMV merit.
- **`Co-Authored-By: Claude` trailer MUST NOT appear** on any commit (user is sole contributor of
  this repo — overrides the default). Plain commit messages only.
- **Frontend has no CI; `main` is unprotected.** Verify locally: `R CMD INSTALL` + testthat + boot
  HTTP 200 + runner e2e; then push to `main` directly (no PR).
- Full e2e (Task 5) requires **backend `nextgenCrossDesign` 0.14.0 installed** so the app produces
  the new fields; unit tests use synthetic `res` objects and need no backend.

## File Structure

- **Modify** `inst/app/tools/run_cross_prediction_json.R` — add `priority_risk_diagnostics` +
  `trait_check_diagnostics` to the envelope `pick()` list (near `constraint_diagnostics`, ~line 759).
- **Modify** `R/diagnostics.R` — new helpers `ngcd_diag_priority_risk`, `ngcd_diag_portfolio`,
  `ngcd_diag_trait_check`, wired into the `ngcd_diagnostics()` dispatcher.
- **Modify** `R/report.R` — a pure `ngcd_portfolio_plotly(res)` returning a plotly widget (or NULL).
- **Modify** `R/app.R` — a "Portfolio & risk" Results nav_panel (scatter output); trait-check inputs
  (per-trait check picker + `check_basis` + `exclude_threshold_violators` + `include_trait_gebv`) and
  their `build_params` wiring; a small pure helper `ngcd_build_trait_checks()` for the spec assembly.
- **Modify/Create** `tests/testthat/test-diagnostics.R` (+ `test-portfolio.R`, `test-trait-checks.R`).
- **Modify** `DESCRIPTION` — version `0.15.5` → `0.16.0`.

**Interfaces produced:**
- `ngcd_diag_priority_risk(res)`, `ngcd_diag_portfolio(res)`, `ngcd_diag_trait_check(res)` →
  `list()` of `ngcd_diag_item(...)`.
- `ngcd_portfolio_plotly(res)` → a plotly htmlwidget or `NULL` (when columns absent).
- `ngcd_build_trait_checks(traits, checks, directions, bases)` → a `data.frame(trait, check,
  direction, basis)` (dropping traits with no check chosen), or `NULL` if none.

---

### Task 1: Runner passthrough + risk/portfolio diagnostics helpers

**Files:** `inst/app/tools/run_cross_prediction_json.R`, `R/diagnostics.R`; Test
`tests/testthat/test-diagnostics.R`.

- [ ] **Step 1: Envelope passthrough** — in the `pick()` list add, next to
  `constraint_diagnostics = pick("constraint_diagnostics")`:

```r
    priority_risk_diagnostics = pick("priority_risk_diagnostics"),
    trait_check_diagnostics = pick("trait_check_diagnostics"),
```

- [ ] **Step 2: Write failing tests** (append to `tests/testthat/test-diagnostics.R`)

```r
test_that("ngcd_diag_priority_risk flags a high-risk top tier", {
  res <- list(
    selected_crosses = data.frame(
      priority_tier = factor(c("highly_priority","highly_priority","priority"),
                             levels = c("highly_priority","priority","medium_priority","low_priority")),
      risk_bin = factor(c("high","high","low"), levels = c("low","med","high"), ordered = TRUE),
      portfolio_profile = factor(c("breakthrough","long_shot","workhorse")),
      confidence_method = rep("midparent_pev_partial", 3), stringsAsFactors = FALSE),
    priority_risk_diagnostics = list(confidence_method = "midparent_pev_partial"))
  items <- ngcd_diag_priority_risk(res)
  expect_true(length(items) >= 1)
  expect_true(any(vapply(items, function(x) grepl("risk", x$title, ignore.case = TRUE), logical(1))))
})

test_that("ngcd_diag_portfolio notes un-ranked upside for mean metric", {
  res <- list(settings = list(trait_value_metric = "mean"),
    selected_crosses = data.frame(portfolio_profile = factor(c("long_shot","workhorse")),
                                  stringsAsFactors = FALSE))
  expect_true(length(ngcd_diag_portfolio(res)) >= 1)
})

test_that("risk/portfolio helpers no-op when columns absent (multi-trait/old backend)", {
  expect_length(ngcd_diag_priority_risk(list(selected_crosses = data.frame(a = 1))), 0)
  expect_length(ngcd_diag_portfolio(list(selected_crosses = data.frame(a = 1))), 0)
})
```

- [ ] **Step 3: Run to verify they fail** — `Rscript -e 'testthat::test_file("tests/testthat/test-diagnostics.R")'`.

- [ ] **Step 4: Implement** (append to `R/diagnostics.R`, before the `ngcd_diagnostics` dispatcher)

```r
# -- risk / portfolio (backend 0.13.0; single-trait only) -------------------
ngcd_diag_priority_risk <- function(res) {
  sc <- res$selected_crosses
  if (!is.data.frame(sc) || !all(c("risk_bin", "priority_tier") %in% names(sc))) return(list())
  out <- list()
  top <- levels(sc$priority_tier)[[1L]]
  in_top <- as.character(sc$priority_tier) == top
  hi <- as.character(sc$risk_bin) == "high"
  k <- sum(in_top & hi, na.rm = TRUE); n <- sum(in_top, na.rm = TRUE)
  if (n > 0 && k > 0)
    out <- c(out, list(ngcd_diag_item("priority_risk", if (k >= ceiling(n/2)) "warn" else "note",
      sprintf("%d of %d top-tier crosses rely on low-confidence predictions", k, n),
      "These crosses rank highly on the point estimate but their predictions are uncertain (high risk).",
      "Hedge with lower-risk 'workhorse' crosses, or enable posterior prediction to sharpen the estimates.")))
  cm <- res$priority_risk_diagnostics$confidence_method %||% (sc$confidence_method[[1L]] %||% NA)
  if (isTRUE(cm %in% c("midparent_pev_partial", "reliability_coarse_variance", "reliability")))
    out <- c(out, list(ngcd_diag_item("priority_risk", "note",
      "Risk is estimated from a partial/coarse signal",
      sprintf("confidence_method = '%s' — the variance-estimation part of risk is not fully captured.", cm),
      "Enable posterior prediction for a complete per-cross risk column.")))
  out
}

# -- portfolio profile (metric-aware guidance) ------------------------------
ngcd_diag_portfolio <- function(res) {
  sc <- res$selected_crosses
  if (!is.data.frame(sc) || !("portfolio_profile" %in% names(sc))) return(list())
  metric <- res$settings$trait_value_metric %||% "usefulness"
  n_up <- sum(as.character(sc$portfolio_profile) %in% c("breakthrough", "long_shot"), na.rm = TRUE)
  msg <- if (identical(metric, "mean"))
    sprintf("You scored on 'mean'; %d cross(es) carry within-family variance (upside) you did not rank.", n_up)
  else "The portfolio shows the genetic decomposition (level x within-family SD) of your plan (relative to this run)."
  list(ngcd_diag_item("portfolio", "note", "Portfolio view available",
    msg, "Open the 'Portfolio & risk' tab: x = genetic SD (upside), y = mean (level), colour = risk."))
}
```

- [ ] **Step 5: Wire into the dispatcher** — add to the `items <- c(...)` in `ngcd_diagnostics()`:
  `ngcd_diag_priority_risk(res), ngcd_diag_portfolio(res),`

- [ ] **Step 6: Run to verify pass. Commit** — `git commit -am "feat(diagnostics): surface priority-risk + portfolio run notes"`

---

### Task 2: Portfolio & risk scatter (plotly) + Results nav panel

**Files:** `R/report.R` (pure plot helper), `R/app.R` (nav panel + output); Test
`tests/testthat/test-portfolio.R`.

- [ ] **Step 1: Write failing test** (`tests/testthat/test-portfolio.R`)

```r
test_that("ngcd_portfolio_plotly builds a widget when columns present, else NULL", {
  res <- list(settings = list(selection_prop = 0.1),
    selected_crosses = data.frame(
      cross = c("A/B","C/D","E/F"), cross_level = c(3,2,1), cross_upside = c(2,1,2),
      cross_confidence = c(.9,.5,.7),
      risk_bin = factor(c("low","high","med"), levels = c("low","med","high"), ordered = TRUE),
      portfolio_profile = factor(c("breakthrough","long_shot","workhorse")),
      priority_tier = factor(c("highly_priority","priority","medium_priority")),
      stringsAsFactors = FALSE))
  p <- ngcd_portfolio_plotly(res)
  expect_true(inherits(p, "plotly") || inherits(p, "htmlwidget"))
  expect_null(ngcd_portfolio_plotly(list(selected_crosses = data.frame(a = 1))))
})
```

- [ ] **Step 2: Run to verify fail.**
- [ ] **Step 3: Implement** (append to `R/report.R`)

```r
# Portfolio scatter: level (y) x upside (x), colour = risk_bin, with iso-genetic-usefulness
# contours (UC-VPM = mu + i*sigma; lines mu = U - i*sigma). Returns NULL if columns absent.
ngcd_portfolio_plotly <- function(res) {
  sc <- res$selected_crosses
  need <- c("cross_level", "cross_upside", "risk_bin")
  if (!is.data.frame(sc) || !all(need %in% names(sc)) || !nrow(sc)) return(NULL)
  p <- suppressWarnings(as.numeric(res$settings$selection_prop %||% 0.1))
  if (!is.finite(p) || p <= 0 || p >= 1) p <- 0.1
  i <- stats::dnorm(stats::qnorm(1 - p)) / p                 # selection intensity
  cols <- c(low = "#1f7a4d", med = "#8a6d1a", high = "#a3341f")
  lab <- if ("cross" %in% names(sc)) sc$cross else seq_len(nrow(sc))
  hov <- sprintf("%s<br>tier: %s<br>profile: %s<br>confidence: %s",
                 lab, as.character(sc$priority_tier %||% NA),
                 as.character(sc$portfolio_profile %||% NA),
                 ngcd_diag_num(sc$cross_confidence %||% NA, 2))
  fig <- plotly::plot_ly()
  # iso-usefulness contour lines across the plotted range
  xr <- range(sc$cross_upside, na.rm = TRUE); yr <- range(sc$cross_level, na.rm = TRUE)
  Us <- seq(min(yr) + i * min(xr), max(yr) + i * max(xr), length.out = 4)
  for (U in Us) fig <- plotly::add_lines(fig, x = xr, y = U - i * xr,
      line = list(dash = "dot", color = "#bbbbbb"), showlegend = FALSE, hoverinfo = "skip")
  for (rb in c("low","med","high")) {
    idx <- as.character(sc$risk_bin) == rb
    if (any(idx)) fig <- plotly::add_markers(fig, x = sc$cross_upside[idx], y = sc$cross_level[idx],
      name = paste0(rb, " risk"), text = hov[idx], hoverinfo = "text",
      marker = list(color = cols[[rb]], size = 9))
  }
  plotly::layout(fig, title = "Portfolio: genetic level x upside (colour = estimation risk)",
    xaxis = list(title = "within-family genetic SD (upside)"),
    yaxis = list(title = "expected mean (level)"),
    annotations = list(list(text = "dotted = iso-genetic-usefulness", showarrow = FALSE,
      xref = "paper", yref = "paper", x = 1, y = 1.05)))
}
```

- [ ] **Step 4: Add the nav panel + server output** in `R/app.R` — a Results `nav_panel` after
  "Candidate scores" (~line 536):

```r
          bslib::nav_panel("Portfolio & risk", plotly::plotlyOutput("res_portfolio", height = "520px")),
```

  and the server render (near the other `output$res_*`):

```r
    output$res_portfolio <- plotly::renderPlotly({
      r <- res(); shiny::req(r)
      p <- ngcd_portfolio_plotly(r)
      shiny::validate(shiny::need(!is.null(p),
        "Portfolio view is available for single-trait runs (needs cross_level / cross_upside / risk_bin)."))
      p
    })
```

- [ ] **Step 5: Run testthat + Commit** — `git commit -am "feat(ui): portfolio & risk plotly scatter tab"`

---

### Task 3: Trait-check diagnostics helper + runner passthrough

**Files:** `R/diagnostics.R` (helper + dispatcher); Test `tests/testthat/test-trait-checks.R`.
(The envelope `pick("trait_check_diagnostics")` was added in Task 1 Step 1.)

- [ ] **Step 1: Write failing test** (`tests/testthat/test-trait-checks.R`)

```r
test_that("ngcd_diag_trait_check reports flags/exclusions and not-evaluable", {
  res <- list(trait_check_diagnostics = list(
    n_flagged = 3L, n_excluded = 2L, n_not_evaluable = 1L,
    active = data.frame(trait = c("yield","protein"), check = c("Ck1","Ck2"),
                        reject_if = c("below","above"), basis = c("gebv","phenotype"))))
  items <- ngcd_diag_trait_check(res)
  expect_true(length(items) >= 1)
  expect_true(any(vapply(items, function(x) grepl("check", x$title, ignore.case = TRUE), logical(1))))
})
test_that("ngcd_diag_trait_check no-ops when absent", {
  expect_length(ngcd_diag_trait_check(list()), 0)
})
```

- [ ] **Step 2: Run to verify fail.**
- [ ] **Step 3: Implement** (append to `R/diagnostics.R`)

```r
# -- per-trait check-threshold veto (backend 0.14.0) ------------------------
ngcd_diag_trait_check <- function(res) {
  d <- res$trait_check_diagnostics
  if (is.null(d)) return(list())
  out <- list()
  nf <- suppressWarnings(as.integer(d$n_flagged %||% 0))
  nx <- suppressWarnings(as.integer(d$n_excluded %||% 0))
  ne <- suppressWarnings(as.integer(d$n_not_evaluable %||% 0))
  ntr <- if (is.data.frame(d$active)) nrow(d$active) else 0L
  if (is.finite(nf) && nf > 0)
    out <- c(out, list(ngcd_diag_item("trait_check", if (nx > 0) "warn" else "note",
      sprintf("%d cross(es) fail a per-trait check%s", nf,
              if (nx > 0) sprintf("; %d excluded from the plan", nx) else " (flagged, kept)"),
      sprintf("Their mid-parent is on the worse side of your check line for %d trait(s).", ntr),
      "Turn on 'exclude threshold violators' to drop them, or relax/remove the trait check.")))
  if (is.finite(ne) && ne > 0)
    out <- c(out, list(ngcd_diag_item("trait_check", "note",
      sprintf("%d trait-check comparison(s) were not evaluable", ne),
      "A parent or the check line had no value on the chosen basis (e.g. phenotype missing).",
      "Use a GEBV basis, or pick a check line/traits that are measured.")))
  out
}
```

- [ ] **Step 4: Wire into the dispatcher** — add `ngcd_diag_trait_check(res),` to the `items <- c(...)`.
- [ ] **Step 5: Run testthat + Commit** — `git commit -am "feat(diagnostics): surface trait-check veto run notes"`

---

### Task 4: Trait-check inputs (per-trait check picker) + build_params wiring

**Files:** `R/app.R` (UI panel + `renderUI` + `build_params`), a pure helper
`ngcd_build_trait_checks`; Test `tests/testthat/test-trait-checks.R`.

- [ ] **Step 1: Write failing test** for the pure assembler (append to `test-trait-checks.R`)

```r
test_that("ngcd_build_trait_checks assembles a spec, dropping traits with no check", {
  s <- ngcd_build_trait_checks(traits = c("yield","protein","oil"),
                               checks = list(yield = "Ck1", protein = "", oil = "Ck3"),
                               directions = list(yield = "auto", protein = "auto", oil = "above"),
                               bases = list(yield = "gebv", protein = "gebv", oil = "phenotype"))
  expect_s3_class(s, "data.frame")
  expect_equal(sort(s$trait), c("oil","yield"))            # protein dropped (no check)
  expect_equal(s$check[s$trait == "oil"], "Ck3")
  expect_equal(s$basis[s$trait == "oil"], "phenotype")
  expect_true(is.na(s$direction[s$trait == "yield"]) || s$direction[s$trait == "yield"] == "")  # auto -> unset, backend defaults
  expect_null(ngcd_build_trait_checks(c("yield"), list(yield = ""), list(yield="auto"), list(yield="gebv")))
})
```

- [ ] **Step 2: Run to verify fail.**
- [ ] **Step 3: Implement the assembler** (in `R/helpers.R` or `R/app.R`)

```r
# Assemble the trait_checks spec data.frame from per-trait UI picks. Drops traits with no check
# chosen; "auto" direction -> unset (NA), letting the backend default from the breeding direction.
ngcd_build_trait_checks <- function(traits, checks, directions, bases) {
  rows <- lapply(traits, function(t) {
    ck <- as.character(checks[[t]] %||% "")
    if (!nzchar(ck)) return(NULL)
    dir <- as.character(directions[[t]] %||% "auto")
    data.frame(trait = t, check = ck,
               direction = if (identical(dir, "auto")) NA_character_ else dir,
               basis = as.character(bases[[t]] %||% "gebv"), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}
```

- [ ] **Step 4: Add the UI** — a Results-independent input group. Add a `nav_panel("Trait checks", ...)`
  in the input navset (after "Scoring", ~line 242) with a `check_basis` selector, an
  `exclude_threshold_violators` checkbox, an `include_trait_gebv` checkbox, and a `uiOutput` for the
  dynamic per-trait pickers:

```r
      bslib::nav_panel("Trait checks",
        shiny::helpText("Flag/exclude crosses whose mid-parent for a trait is on the worse side of a check line."),
        shiny::selectInput("check_basis", "Comparison basis", c("GEBV" = "gebv", "Phenotype" = "phenotype")),
        shiny::checkboxInput("exclude_threshold_violators", "Exclude violating crosses from the plan", FALSE),
        shiny::checkboxInput("include_trait_gebv", "Include per-trait mid-parent GEBV in the Excel workbook", FALSE),
        shiny::uiOutput("trait_check_pickers")),
```

  and the server `renderUI` (parents = the loaded genotype ids; traits = the active traits):

```r
    output$trait_check_pickers <- shiny::renderUI({
      d <- rv$data; shiny::req(d)
      traits <- as.character(d$direction[[input$direction_trait_col %||% "trait"]] %||% d$direction[[1L]])
      ids <- as.character(d$genotype[[input$genotype_id_col %||% names(d$genotype)[1]]])
      lapply(traits, function(t) shiny::fluidRow(
        shiny::column(4, shiny::selectInput(paste0("chk_", t), paste("Check for", t),
                        choices = c("(none)" = "", stats::setNames(ids, ids)))),
        shiny::column(4, shiny::selectInput(paste0("dir_", t), "Reject if",
                        choices = c("auto (from breeding direction)" = "auto", "above check" = "above", "below check" = "below"))),
        shiny::column(4, shiny::selectInput(paste0("basis_", t), "Basis",
                        choices = c("(run default)" = "", "GEBV" = "gebv", "Phenotype" = "phenotype")))))
    })
```

- [ ] **Step 5: Wire into `build_params`** (R/app.R:1010 list) — assemble from the dynamic inputs:

```r
        trait_checks = local({
          d <- rv$data
          traits <- if (!is.null(d)) as.character(d$direction[[input$direction_trait_col %||% "trait"]]) else character(0)
          ngcd_build_trait_checks(traits,
            checks = stats::setNames(lapply(traits, function(t) input[[paste0("chk_", t)]]), traits),
            directions = stats::setNames(lapply(traits, function(t) input[[paste0("dir_", t)]]), traits),
            bases = stats::setNames(lapply(traits, function(t) { b <- input[[paste0("basis_", t)]]; if (nzchar(b %||% "")) b else input$check_basis }), traits))
        }),
        check_basis = input$check_basis %||% "gebv",
        exclude_threshold_violators = isTRUE(input$exclude_threshold_violators),
        include_trait_gebv = isTRUE(input$include_trait_gebv),
```

- [ ] **Step 6: Run testthat + Commit** — `git commit -am "feat(ui): per-trait check-line pickers + run wiring"`

---

### Task 5: Verify end-to-end (install backend 0.14.0, testthat, boot, e2e)

**Files:** none new; verification only.

- [ ] **Step 1: Install backend 0.14.0** so the app produces the new fields
  (from the backend working tree or the mirror): `R CMD INSTALL <backend> --no-multiarch --no-docs`.
  Confirm `packageVersion("nextgenCrossDesign") == "0.14.0"`.
- [ ] **Step 2: testthat suite** — `Rscript -e 'testthat::test_dir("tests/testthat")'` → all pass.
- [ ] **Step 3: Frontend install + boot** — `R CMD INSTALL .` then boot the app headless and assert
  HTTP 200 (mirror the existing boot check used for prior frontend pushes).
- [ ] **Step 4: Runner e2e** — run `inst/app/tools/run_cross_prediction_json.R` with a small
  single-trait config that sets `trait_checks` (one check) + `use_ocs=TRUE`; read the result JSON and
  assert: per-cross `risk_bin`/`portfolio_profile`/`cross_level`/`cross_upside` present on
  `selected_crosses`; `priority_risk_diagnostics` and `trait_check_diagnostics` present; and
  `ngcd_portfolio_plotly(res)` returns a widget, `ngcd_diagnostics(res)` includes priority-risk /
  portfolio / trait-check items.
- [ ] **Step 5: Commit** any fixups — `git commit -am "test(frontend): e2e verify risk/portfolio + trait-check surfacing"`

---

### Task 6: Version bump + push

- [ ] **Step 1:** `DESCRIPTION` `Version:` `0.15.5` → `0.16.0`.
- [ ] **Step 2:** Commit `chore: bump to 0.16.0 (risk/portfolio + trait-check surfacing)` (NO co-author trailer).
- [ ] **Step 3:** `git push origin main` (frontend main is unprotected; no PR/CI).

---

## Deferred / dependencies
- **Image rebuild** (backend 0.14.0 + frontend 0.16.0) is required for the deployed app to reflect
  this — parked, separate step.
- DT badge styling (colour `risk_bin`/`portfolio_profile` cells) is optional polish — the columns
  already display in the Selected/Candidate tables; skip unless time permits.

## Self-Review
- **Design coverage:** risk/portfolio §8 (scatter + iso-usefulness contours + risk colour separate
  from upside) → Task 2; risk/portfolio diagnostics → Task 1; trait-check surfacing (inputs + run
  note) → Tasks 3-4; runner passthrough → Task 1. Multi-trait/absent-field graceful no-op → Global
  Constraints + every helper's guard.
- **Placeholder scan:** none — concrete code per step.
- **Type consistency:** `ngcd_build_trait_checks(...) -> data.frame(trait,check,direction,basis)`
  used identically in Task 4 test + build_params; `ngcd_portfolio_plotly(res) -> plotly|NULL` in
  Task 2 test + server; diagnostics helpers return `list()`-of-items and are all added to the one
  dispatcher.
