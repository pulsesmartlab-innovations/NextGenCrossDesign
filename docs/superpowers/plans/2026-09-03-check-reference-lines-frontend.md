# Check Reference Lines — Frontend Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Surface check lines in the workbench as reference-only benchmarks — a dedicated check
file, per-trait check pickers fed from that file, a reference line on the results charts, and
run notes that report rather than warn about exclusions that no longer happen.

**Architecture:** A new per-file import card carries the check genotypes, kept separate from the
parent genotype file so a check can never become a mating candidate. The picker choices come
from that file. The backend returns `trait_check_reference`; the charts draw it as a horizontal
line and encode `p_beat_check` as marker opacity.

**Tech Stack:** R, Shiny, bslib, plotly, testthat (`tests/testthat/`).

**Spec:** `../cross_prediction/docs/design/2026-09-03-check-reference-lines-design.md`
(backend repo — read it before starting; it carries the reasoning this plan implements.)

## Global Constraints

- Package: `nextgenCrossWorkbench`, version `0.26.0` → **`0.27.0`**.
- **Backend floor: `nextgenCrossDesign >= 0.23.0`.** This plan cannot be tested end-to-end
  until the backend plan is merged; do the backend first.
- **No `Co-Authored-By: Claude` trailer on any commit in this project.**
- **Input-ID conservation guard.** The static input-ID set is baselined in
  `.superpowers/sdd/baseline-input-ids.txt`. This change deliberately **adds**
  `f_check`, `check_id_col`, and **removes** every `basis_<trait>` input plus the exclude
  toggle. After the last task, re-extract the baseline and review the diff deliberately — do
  not bypass the guard.
- Every config key must invalidate a stage or `tests/testthat/test-pipeline-state.R:262`
  reports it as orphaned.
- Removed backend parameters — do not send them: `check_basis`, `exclude_threshold_violators`.
- **Both check inputs are offered; the RUN decides which is consulted.** `check_geno` serves a
  GEBV mean source, `check_pheno` a phenotypic one. Note the backend currently **requires**
  `check_geno` whenever `trait_checks` is supplied (it also uses it for the parent-id collision
  check), so the genotype file is mandatory even on a phenotype-scored run while the phenotype
  file is what actually produces the value there. If that requirement is relaxed backend-side,
  loosen the UI validation to match — do not diverge from it unilaterally.
- **`check_progeny_size` has no default and must come from the user.** The backend hard-errors
  if `trait_checks` is supplied without it, because progeny per family scales P(beat check)
  directly — a default would report a probability computed from a number nobody chose. The UI
  must therefore collect it (Task 3) and block the run with a clear message rather than
  substituting a value.
- Run one test file with
  `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-<name>.R")'`.

---

### Task 1: `ngcd_build_trait_checks()` drops `bases`

Start at the pure function — it is testable without Shiny and everything else depends on its
shape.

**Files:**
- Modify: `R/helpers.R:562-575`
- Test: `tests/testthat/test-trait-checks.R`

**Interfaces:**
- Produces: `ngcd_build_trait_checks(traits, checks, directions)` → data frame with columns
  `trait`, `check`, `direction` (no `basis` column), or `NULL` when no check is chosen.

- [ ] **Step 1: Write the failing test**

Replace the contents of `tests/testthat/test-trait-checks.R` with:

```r
test_that("build_trait_checks emits trait/check/direction and no basis", {
  out <- ngcd_build_trait_checks(
    traits = c("yield", "matur"),
    checks = list(yield = "CHK_A", matur = "CHK_B"),
    directions = list(yield = "auto", matur = "above"))
  expect_equal(nrow(out), 2L)
  expect_setequal(names(out), c("trait", "check", "direction"))
  expect_false("basis" %in% names(out))
  expect_true(is.na(out$direction[out$trait == "yield"]))   # auto -> NA, backend resolves it
  expect_equal(out$direction[out$trait == "matur"], "above")
})

test_that("traits with no check chosen are dropped, and all-empty gives NULL", {
  out <- ngcd_build_trait_checks("yield", list(yield = ""), list(yield = "auto"))
  expect_null(out)
  out2 <- ngcd_build_trait_checks(c("yield", "matur"),
                                  list(yield = "CHK_A", matur = ""),
                                  list(yield = "auto", matur = "auto"))
  expect_equal(nrow(out2), 1L)
  expect_equal(out2$check, "CHK_A")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-trait-checks.R")'`
Expected: FAIL — `basis` is still in `names(out)`, and the current signature requires `bases`.

- [ ] **Step 3: Write the implementation**

Replace `R/helpers.R:562-575`:

```r
# Assemble the backend's trait_checks data.frame from the per-trait pickers. There is no basis
# column: the backend puts the check on the run's own mean_source, which is what keeps the
# reference line on the same scale as the cross means it is drawn against.
ngcd_build_trait_checks <- function(traits, checks, directions) {
  rows <- lapply(traits, function(t) {
    ck <- as.character(checks[[t]] %||% "")
    if (!nzchar(ck)) return(NULL)
    dir <- as.character(directions[[t]] %||% "auto")
    data.frame(trait = t, check = ck,
               direction = if (identical(dir, "auto")) NA_character_ else dir,
               stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-trait-checks.R")'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/helpers.R tests/testthat/test-trait-checks.R
git commit -m "refactor(checks): drop the basis argument from ngcd_build_trait_checks"
```

---

### Task 2: Check-file import card

**Files:**
- Modify: `R/app.R:182-205` (import cards), `R/app.R:770` (the file-reading block)
- Test: `tests/testthat/test-data-import.R`

**Interfaces:**
- Produces: input ids `f_check` and `f_check_pheno` (fileInputs) and `check_id_col`
  (selectInput);
  `rv$data$check_geno` and `rv$data$check_pheno` holding the parsed data frames; `output$check_step` rendering the
  preview + id-column picker + alignment badge.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-data-import.R`:

```r
test_that("the check file is a distinct optional input, separate from the parents", {
  ui <- ngcd_app_ui()
  html <- as.character(ui)
  expect_true(grepl("f_check", html, fixed = TRUE))
  expect_true(grepl("f_check_pheno", html, fixed = TRUE))
  expect_true(grepl("imp-check", html, fixed = TRUE))
  # the card must say plainly that checks are never crossed
  expect_true(grepl("never crossed", html, fixed = TRUE))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-data-import.R")'`
Expected: FAIL — `f_check` is not in the UI.

- [ ] **Step 3: Write the implementation**

In `R/app.R`, after the trait-direction card (the block ending at :201), insert:

```r
              shiny::conditionalPanel("input.workflow == 'standard'",
                shiny::tags$div(id = "imp-check",
                  bslib::card(bslib::card_header("5 · Check lines (optional)"),
                    shiny::fileInput("f_check", "Check genotype CSV",
                                     accept = c(".csv", ".txt", ".tsv")),
                    shiny::fileInput("f_check_pheno", "Check phenotype CSV",
                                     accept = c(".csv", ".txt", ".tsv")),
                    shiny::div(class = "help-hint",
                      "Standard varieties you benchmark against. They are ",
                      shiny::tags$b("never crossed"),
                      " — they appear as a reference line on the results charts and as ",
                      "reference columns in the workbook. The genotype file needs the same ",
                      "markers and coding as your parents; the phenotype file needs the same ",
                      "trait columns as your phenotype file. ",
                      shiny::tags$b("Supply both if you have them"),
                      " — the run decides which it needs, so the check is always measured the ",
                      "same way as the parents it is compared against. Without the phenotype ",
                      "file, a run that scores on phenotypes has no value for the check and ",
                      "reports it as not evaluable."),
                    shiny::uiOutput("check_step")))),
```

In the file-reading block at `R/app.R:770`, add the check file alongside the others:

```r
      rv$data$check_geno  <- rd(input$f_check$datapath)
      rv$data$check_pheno <- rd(input$f_check_pheno$datapath)
```

Add the step renderer near the other `*_step` outputs:

```r
    output$check_step <- shiny::renderUI({
      g <- rv$data$check_geno
      if (is.null(g)) return(shiny::div(class = "help-hint",
        "No check file loaded. Check lines are optional."))
      ids <- names(g)
      guess <- ngcd_guess_col(ids, c("NAME", "id", "line", "check"))
      pg <- rv$data$genotype
      shared <- if (is.null(pg)) NA_integer_ else length(intersect(names(g), names(pg)))
      shiny::tagList(
        shiny::selectInput("check_id_col", "Check ID column", choices = ids,
                           selected = guess),
        if (!is.na(shared)) ngcd_callout(
          kind = if (shared > 1L) "ok" else "warn",
          sprintf("%d column(s) shared with the genotype file.%s", shared,
                  if (shared > 1L) "" else
                    " The check file must carry the same markers as the genotype file.")),
        shiny::div(class = "help-hint",
                   sprintf("%d check line(s) loaded.", nrow(g))))
    })
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-data-import.R")'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/app.R tests/testthat/test-data-import.R
git commit -m "feat(import): check-line genotype file as its own guided import card"
```

---

### Task 3: Repoint the pickers at the check file; delete the basis control

**Files:**
- Modify: `R/app.R:1109` (`trait_check_pickers`), `R/app.R:1258` (the `ngcd_build_trait_checks` call), `R/app.R:366-372` (the panel copy)
- Test: `tests/testthat/test-trait-checks.R`

**Interfaces:**
- Consumes: `rv$data$check_geno`, `input$check_id_col`, `ngcd_build_trait_checks()` from Task 1.
- Produces: per-trait inputs `chk_<trait>` and `dir_<trait>`. **`basis_<trait>` is removed.**

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-trait-checks.R`:

```r
test_that("the trait-check panel no longer offers a basis or an exclude toggle", {
  html <- as.character(ngcd_app_ui())
  expect_false(grepl("exclude_threshold_violators", html, fixed = TRUE))
  expect_false(grepl("check_basis", html, fixed = TRUE))
  # the copy must describe a reference, not a veto
  expect_false(grepl("excludes", html, fixed = TRUE))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-trait-checks.R")'`
Expected: FAIL — the exclude toggle and basis selects are still in the UI.

- [ ] **Step 3: Write the implementation**

Replace the `trait_check_pickers` renderer at `R/app.R:1109`:

```r
    # Per-trait check-line pickers. Candidate ids come from the CHECK FILE, never the genotype
    # table: a check is a benchmark, not a mating candidate, and offering parents here is what
    # made the old veto require the check to be a parent.
    output$trait_check_pickers <- shiny::renderUI({
      traits <- full_trait_set()
      shiny::validate(shiny::need(length(traits) > 0,
        "Load a phenotype/direction file to pick trait checks."))
      g <- rv$data$check_geno
      shiny::validate(shiny::need(!is.null(g),
        "Load a check genotype file (Data > Check lines) to choose check lines."))
      cid <- input$check_id_col %||% ngcd_guess_col(names(g), c("NAME", "id", "line", "check"))
      if (is.null(cid) || !cid %in% names(g)) cid <- names(g)[1]
      ids <- as.character(g[[cid]])
      shiny::tagList(lapply(traits, function(t) shiny::fluidRow(
        shiny::column(6, shiny::selectInput(paste0("chk_", t), paste("Check for", t),
                        choices = c("(none)" = "", stats::setNames(ids, ids)))),
        shiny::column(6, shiny::selectInput(paste0("dir_", t), "Good side",
                        choices = c("auto (from breeding direction)" = "auto",
                                    "above the check" = "below",
                                    "below the check" = "above"))))))
    })
```

Note the value mapping: the backend's `direction` names the **rejecting** side, so "above the
check" (what the breeder wants) is sent as `below`.

Update the call at `R/app.R:1258`:

```r
          ngcd_build_trait_checks(traits,
            checks = stats::setNames(lapply(traits, function(t) input[[paste0("chk_", t)]]), traits),
            directions = stats::setNames(lapply(traits, function(t) input[[paste0("dir_", t)]]), traits))
```

Replace the panel copy at `R/app.R:366-372`:

```r
            shiny::tags$li(shiny::tags$b("Check lines"),
              ": shows, per trait, whether the mid-parent of each cross lands on the good side ",
              "of a standard variety you benchmark against — as a reference line on the charts ",
              "and as columns in the workbook. It never removes or reorders a cross."),
```

and the helpText below the panel header:

```r
        shiny::helpText("Reference only — checks are never crossed and never change the plan."),
```

Delete the `exclude_threshold_violators` checkbox, the `check_basis` select, and every
`basis_<trait>` input.

Add the progeny-size input immediately above the pickers — it has no default value, so the
breeder must type their own number:

```r
        shiny::numericInput("check_progeny_size",
          "Progeny per family you will raise", value = NA, min = 1, step = 10),
        shiny::div(class = "help-hint",
          "Used only for the ", shiny::tags$b("P(beat check)"), " column: the chance a cross ",
          "throws at least one line past the check. There is no default — the number has to be ",
          "yours, because raising 50 progeny and raising 500 give different answers."),
```

and gate the run on it in the same place the other required-input validations live:

```r
      if (!is.null(trait_checks_df)) {
        shiny::validate(shiny::need(
          isTRUE(is.finite(input$check_progeny_size)) && input$check_progeny_size >= 1,
          "Enter the progeny per family before running with check lines."))
      }
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-trait-checks.R")'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/app.R tests/testthat/test-trait-checks.R
git commit -m "feat(ui): check pickers read the check file; drop basis and exclude controls"
```

---

### Task 4: Send the check file to the backend

**Files:**
- Modify: `R/app.R` (the `build_params` / args assembly near :1258), `inst/app/tools/run_cross_prediction_json.R:548`
- Test: `tests/testthat/test-run-backend-edge.R`

**Interfaces:**
- Consumes: `rv$data$check_geno`, `ngcd_build_trait_checks()`.
- Produces: backend args `check_geno` (matrix, rownames = check ids) and `trait_checks`
  (data frame). `check_basis` and `exclude_threshold_violators` are never sent.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-run-backend-edge.R`:

```r
test_that("check args are forwarded and the removed parameters never are", {
  args <- list(trait_checks = list(list(trait = "yield", check = "CHK_A")),
               check_geno = list(list(id = "CHK_A", m1 = 0, m2 = 2)))
  out <- ngcd_coerce_backend_args(args)
  expect_s3_class(out$trait_checks, "data.frame")
  expect_true(is.matrix(out$check_geno))
  expect_equal(rownames(out$check_geno), "CHK_A")
  expect_null(out$check_basis)
  expect_null(out$exclude_threshold_violators)
  # progeny size is passed straight through, never defaulted on the way
  expect_equal(ngcd_coerce_backend_args(c(args, list(check_progeny_size = 250)))$check_progeny_size, 250)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-run-backend-edge.R")'`
Expected: FAIL — `check_geno` is not coerced to a matrix.

- [ ] **Step 3: Write the implementation**

In `inst/app/tools/run_cross_prediction_json.R`, beside the existing `trait_checks` coercion at
:548, add:

```r
  # check_geno arrives as JSON rows; the backend wants a numeric matrix keyed by check id.
  if (!is.null(args_in$check_geno)) {
    cg <- as_rows_df(args_in$check_geno, "check_geno")
    id_col <- names(cg)[[1L]]
    ids <- as.character(cg[[id_col]])
    m <- as.matrix(cg[, setdiff(names(cg), id_col), drop = FALSE])
    storage.mode(m) <- "numeric"
    rownames(m) <- ids
    args_in$check_geno <- m
  }
  args_in$check_basis <- NULL
  args_in$exclude_threshold_violators <- NULL
```

In `R/app.R`, add `check_geno = rv$data$check_geno`, `check_pheno = rv$data$check_pheno` and
`check_progeny_size = input$check_progeny_size` to the backend args next to `trait_checks`,
and delete the `check_basis` / `exclude_threshold_violators` entries.

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-run-backend-edge.R")'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/app.R inst/app/tools/run_cross_prediction_json.R tests/testthat/test-run-backend-edge.R
git commit -m "feat(runner): forward check_geno; stop sending the removed check parameters"
```

---

### Task 5: Run notes report a reference, not an exclusion

**Files:**
- Modify: `R/diagnostics.R:398-417`
- Test: `tests/testthat/test-diagnostics.R`

**Interfaces:**
- Consumes: `res$trait_check_reference` (replaces `res$trait_check_diagnostics`).
- Produces: `ngcd_diag_trait_check(res)` → list of `ngcd_diag_item()`s, severity `note` only —
  nothing is excluded any more, so nothing warrants a `warn`.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-diagnostics.R`:

```r
test_that("check run notes report counts and never mention exclusion", {
  res <- list(trait_check_reference = list(
    active = data.frame(trait = c("yield", "matur"), check = c("CHK_A", "CHK_B"),
                        reject_if = c("below", "above"), stringsAsFactors = FALSE),
    diagnostics = list(n_wrong_side = list(yield = 3L, matur = 0L),
                       n_not_evaluable = 0L, n_candidates = 20L)))
  items <- ngcd_diag_trait_check(res)
  expect_gte(length(items), 1L)
  txt <- paste(vapply(items, function(x) paste(unlist(x), collapse = " "), character(1)),
               collapse = " ")
  expect_true(grepl("3", txt, fixed = TRUE))
  expect_false(grepl("exclude", tolower(txt), fixed = TRUE))
  expect_true(all(vapply(items, function(x) x$severity != "warn", logical(1))))
})

test_that("no checks configured produces no notes", {
  expect_equal(length(ngcd_diag_trait_check(list())), 0L)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-diagnostics.R")'`
Expected: FAIL — the function still reads `trait_check_diagnostics` and returns `list()`.

- [ ] **Step 3: Write the implementation**

Replace `R/diagnostics.R:398-417`:

```r
ngcd_diag_trait_check <- function(res) {
  ref <- res$trait_check_reference
  if (is.null(ref)) return(list())
  d <- ref$diagnostics %||% list()
  spec <- if (is.data.frame(ref$active)) ref$active else data.frame()
  n_tot <- suppressWarnings(as.integer(d$n_candidates %||% NA_integer_))
  out <- list()
  for (k in seq_len(nrow(spec))) {
    tr <- spec$trait[[k]]
    nw <- suppressWarnings(as.integer(d$n_wrong_side[[tr]] %||% 0L))
    if (!is.finite(nw) || nw <= 0L) next
    side <- if (identical(spec$reject_if[[k]], "below")) "below" else "above"
    out <- c(out, list(ngcd_diag_item("trait_check", "note",
      sprintf("%d of %d cross(es) fall %s the %s check for %s", nw, n_tot, side,
              spec$check[[k]], tr),
      "Their mid-parent is on the worse side of your check line for this trait.",
      "Reference only — these crosses are still ranked and can still be selected. Check the P(beat check) column before discarding one.")))
  }
  ne <- suppressWarnings(as.integer(d$n_not_evaluable %||% 0L))
  if (is.finite(ne) && ne > 0L) {
    out <- c(out, list(ngcd_diag_item("trait_check", "note",
      sprintf("%d check comparison(s) were not evaluable", ne),
      "The check line had no value on the source this run used for that trait.",
      "Give the check a record on that source, or pick a check line that is measured.")))
  }
  out
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-diagnostics.R")'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/diagnostics.R tests/testthat/test-diagnostics.R
git commit -m "feat(diagnostics): check run notes report the reference instead of an exclusion"
```

---

### Task 6: Reference line and P(beat check) opacity on the charts

**Files:**
- Modify: `R/ui_charts.R`
- Test: `tests/testthat/test-ui-charts.R`

**Interfaces:**
- Consumes: `res$trait_check_reference`.
- Produces: `ngcd_check_line(res, trait = NULL)` → single numeric or `NA_real_`;
  `ngcd_chart_mean_vs_diversity(df, check_line = NULL, check_label = NULL, mean_axis = NULL)`
  → plotly widget. **`mean_axis` has no guessing default**: `"y"` draws a horizontal line,
  `"x"` a vertical one, `NULL` draws none.
  carrying a horizontal reference shape and marker opacity from `p_beat_check`.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-ui-charts.R`:

```r
test_that("check line resolves for single trait and declines for a rank index", {
  res <- list(trait_check_reference = list(
    active = data.frame(trait = "yield", check = "CHK_A", reject_if = "below",
                        stringsAsFactors = FALSE),
    values = list(yield = c(CHK_A = 6))))
  expect_equal(ngcd_check_line(res, trait = "yield"), 6)

  res_rank <- res
  res_rank$trait_check_reference$active <- data.frame(
    trait = c("yield", "protein"), check = "CHK_A", reject_if = "below",
    stringsAsFactors = FALSE)
  res_rank$trait_check_reference$values <- list(yield = c(CHK_A = 6), protein = c(CHK_A = 10))
  res_rank$multi_trait <- list(method = "rank_threshold")
  expect_true(is.na(ngcd_check_line(res_rank)))
})

test_that("the scatter carries a reference shape when a check line is given", {
  df <- data.frame(pair_kinship = c(0.1, 0.2), multi_trait_score = c(9, 4),
                   p_beat_check = c(0.98, 0.71), stringsAsFactors = FALSE)
  # mean on y -> HORIZONTAL line spanning x
  p <- ngcd_chart_mean_vs_diversity(df, check_line = 6, check_label = "CHK_A", mean_axis = "y")
  expect_s3_class(p, "plotly")
  sh <- p$x$layout$shapes
  expect_true(length(sh) >= 1L)
  expect_equal(sh[[1]]$y0, 6); expect_equal(sh[[1]]$y1, 6)
  expect_equal(sh[[1]]$xref, "paper")

  # mean on x (diversity-vs-mean scatter) -> VERTICAL line spanning y
  pv <- ngcd_chart_mean_vs_diversity(df, check_line = 6, check_label = "CHK_A", mean_axis = "x")
  shv <- pv$x$layout$shapes
  expect_equal(shv[[1]]$x0, 6); expect_equal(shv[[1]]$x1, 6)
  expect_equal(shv[[1]]$yref, "paper")

  # no mean-bearing axis -> NO line at all
  pn <- ngcd_chart_mean_vs_diversity(df, check_line = 6, check_label = "CHK_A", mean_axis = NULL)
  expect_true(is.null(pn$x$layout$shapes) || length(pn$x$layout$shapes) == 0L)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-ui-charts.R")'`
Expected: FAIL — `could not find function "ngcd_check_line"`.

- [ ] **Step 3: Write the implementation**

Add to `R/ui_charts.R`:

```r
# The y value for the check reference line, or NA when no honest line exists. A single-trait
# run plots the trait mean itself. A linear index (weighted / economic) is a fixed combination
# of trait values, so the check's index value is exact. A rank-based index is a function of the
# candidate distribution and a check has no rank -- there is no line, and the caller says so.
ngcd_check_line <- function(res, trait = NULL) {
  ref <- res$trait_check_reference
  if (is.null(ref)) return(NA_real_)
  spec <- as.data.frame(ref$active, stringsAsFactors = FALSE)
  if (!nrow(spec)) return(NA_real_)
  val <- function(tr) {
    ck <- spec$check[[match(tr, spec$trait)]]
    suppressWarnings(as.numeric(ref$values[[tr]][[ck]]))
  }
  if (!is.null(trait) || nrow(spec) == 1L) {
    tr <- trait %||% spec$trait[[1L]]
    if (!(tr %in% spec$trait)) return(NA_real_)
    v <- val(tr)
    return(if (length(v) && is.finite(v)) v else NA_real_)
  }
  meta <- res$multi_trait
  if (!(as.character(meta$method %||% "") %in%
        c("weighted", "economic_index", "desired_gain"))) return(NA_real_)
  w <- meta$weights
  tr <- intersect(spec$trait, names(w %||% character(0)))
  if (!length(tr)) return(NA_real_)
  v <- vapply(tr, val, numeric(1))
  if (any(!is.finite(v))) return(NA_real_)
  sum(v * as.numeric(w[tr]))
}

# Mean-by-diversity scatter with an optional check reference line. The check has no diversity
# coordinate, so it is a line spanning the full x range, never a marker. Marker opacity carries
# P(beat check): without it every point below the line looks equally dead, when in fact a
# high-variance cross below the check can still throw a superior progeny.
ngcd_chart_mean_vs_diversity <- function(df, check_line = NULL, check_label = NULL,
                                         x_col = "pair_kinship", y_col = "multi_trait_score") {
  if (!ngcd_has_cols(df, c(x_col, y_col))) return(ngcd_chart_empty())
  op <- if ("p_beat_check" %in% names(df)) {
    o <- suppressWarnings(as.numeric(df$p_beat_check))
    ifelse(is.finite(o), pmax(0.25, pmin(1, o)), 0.6)
  } else rep(0.8, nrow(df))
  p <- plotly::plot_ly(x = df[[x_col]], y = df[[y_col]], type = "scatter", mode = "markers",
                       marker = list(size = 9, opacity = op,
                                     color = ngcd_chart_palette()[[1L]]),
                       hoverinfo = "text",
                       text = sprintf("%s: %.4g<br>%s: %.4g%s", x_col, df[[x_col]],
                                      y_col, df[[y_col]],
                                      if ("p_beat_check" %in% names(df))
                                        sprintf("<br>P(beat check): %.2f", df$p_beat_check) else ""))
  # The check has a value on the MEAN axis and none on the other, so the line is perpendicular
  # to whichever axis carries the mean. A plot with no mean-bearing axis passes mean_axis = NULL
  # and gets no line -- a reference on a rank-vs-rank or kinship-vs-kinship plot is meaningless.
  if (!is.null(check_line) && is.finite(check_line) && !is.null(mean_axis)) {
    lab <- check_label %||% "reference"
    stroke <- list(color = "#B00020", width = 2, dash = "dash")
    shp <- if (identical(mean_axis, "y")) {
      list(type = "line", xref = "paper", x0 = 0, x1 = 1,
           y0 = check_line, y1 = check_line, line = stroke)
    } else {
      list(type = "line", yref = "paper", y0 = 0, y1 = 1,
           x0 = check_line, x1 = check_line, line = stroke)
    }
    ann <- if (identical(mean_axis, "y")) {
      list(xref = "paper", x = 1, y = check_line, xanchor = "right", yanchor = "bottom",
           text = sprintf("check: %s", lab), showarrow = FALSE,
           font = list(color = "#B00020", size = 11))
    } else {
      list(yref = "paper", y = 1, x = check_line, xanchor = "left", yanchor = "top",
           text = sprintf("check: %s", lab), showarrow = FALSE,
           font = list(color = "#B00020", size = 11))
    }
    p <- plotly::layout(p, shapes = list(shp), annotations = list(ann))
  }
  plotly::layout(p, xaxis = list(title = "Diversity (pair kinship)"),
                 yaxis = list(title = "Predicted mean"))
}
```

Wire it into the results tab: call `ngcd_check_line(res)` and pass the result plus the check id
into `ngcd_chart_mean_vs_diversity()`, passing `mean_axis = "y"` for the existing mean-on-y
scatter. A future diversity-on-y / mean-on-x view passes `mean_axis = "x"` and gets a vertical
line from the same helper with no further change. When the line is `NA` and checks are
configured, render
`ngcd_callout(kind = "note", "No single reference line applies to a rank-based index — see the per-trait panel below.")`.

- [ ] **Step 4: Add the per-trait panel for multi-trait runs**

One y axis cannot carry several check lines on different scales, so multi-trait checks get one
facet per trait. Append to `tests/testthat/test-ui-charts.R`:

```r
test_that("per-trait check panels draw one subplot per checked trait", {
  df <- data.frame(pair_kinship = c(0.1, 0.2),
                   yield_mean = c(9, 4), yield_check_value = 6,
                   yield_check_ok = c(TRUE, FALSE),
                   protein_mean = c(11, 13), protein_check_value = 10,
                   protein_check_ok = c(TRUE, TRUE), stringsAsFactors = FALSE)
  ref <- list(active = data.frame(trait = c("yield", "protein"), check = "CHK_A",
                                  reject_if = "below", stringsAsFactors = FALSE))
  p <- ngcd_chart_check_panels(df, ref)
  expect_s3_class(p, "plotly")
  expect_null(ngcd_chart_check_panels(df, NULL))      # no checks -> nothing to draw
})
```

Add to `R/ui_charts.R`:

```r
# One facet per checked trait: y = that trait's mid-parent mean, x = diversity, each with its
# own check line on its own scale. This is where multi-trait checks live -- a single index axis
# cannot carry several check lines honestly.
ngcd_chart_check_panels <- function(df, trait_check_reference, x_col = "pair_kinship") {
  if (is.null(trait_check_reference)) return(NULL)
  spec <- as.data.frame(trait_check_reference$active, stringsAsFactors = FALSE)
  traits <- spec$trait[paste0(spec$trait, "_mean") %in% names(df)]
  if (!length(traits) || !ngcd_has_cols(df, x_col)) return(NULL)
  panels <- lapply(traits, function(tr) {
    ok <- df[[paste0(tr, "_check_ok")]]
    p <- plotly::plot_ly(x = df[[x_col]], y = df[[paste0(tr, "_mean")]],
                         type = "scatter", mode = "markers", name = tr,
                         marker = list(size = 8,
                                       color = ifelse(ok %in% FALSE, "#BBBBBB",
                                                      ngcd_chart_palette()[[1L]])))
    tau <- suppressWarnings(as.numeric(df[[paste0(tr, "_check_value")]][[1L]]))
    if (length(tau) && is.finite(tau)) {
      p <- plotly::layout(p, shapes = list(list(
        type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = tau, y1 = tau,
        line = list(color = "#B00020", width = 2, dash = "dash"))))
    }
    plotly::layout(p, yaxis = list(title = tr))
  })
  plotly::subplot(panels, nrows = 1L, titleY = TRUE, margin = 0.05)
}
```

Render it under the main scatter whenever `nrow(spec) > 1`.

- [ ] **Step 5: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-ui-charts.R")'`
Expected: PASS

- [ ] **Step 6: Commit**

```bash
git add R/ui_charts.R tests/testthat/test-ui-charts.R
git commit -m "feat(charts): check reference line, P(beat check) opacity, per-trait panels"
```

---

### Task 7: Report, version bump, input-ID baseline

**Files:**
- Modify: `R/report.R`, `DESCRIPTION`, `NEWS.md`, `.superpowers/sdd/baseline-input-ids.txt`

- [ ] **Step 1: Draw the line in the report**

In `R/report.R`, in the frontier/scatter section, add after the existing `plot()`:

```r
    cl <- ngcd_check_line(res)
    if (is.finite(cl)) {
      graphics::abline(h = cl, lty = 2, lwd = 2, col = "#B00020")
    }
```

- [ ] **Step 2: Bump the version and the backend floor**

`DESCRIPTION`: `Version: 0.27.0`, and raise the `nextgenCrossDesign` requirement to
`(>= 0.23.0)`.

- [ ] **Step 3: Write the NEWS entry**

```markdown
# nextgenCrossWorkbench 0.27.0

* **Check lines are references, not filters.** Requires backend 0.23.0. Check genotypes are
  uploaded in their own file (Data > Check lines) and are never crossed. Each trait's check
  appears as a reference line on the results scatter and as reference columns in the workbook.
* The exclude-violators toggle and the check basis control are gone: nothing is dropped, and
  the check always follows the run's own mean source so the reference line is on the same
  scale as the axis it is drawn on.
* Marker opacity on the scatter now carries P(beat check), so a below-check cross with a
  superior tail is visible instead of looking like a discard.
* Multi-trait runs get a per-trait check panel (one facet per checked trait, each with its own
  line on its own scale) and a `p_beat_all_checks` column — the probability that a progeny
  beats every check at once.
```

- [ ] **Step 4: Re-extract the input-ID baseline and review the diff**

Use the project's own extraction (it scans the **static** ids in `R/app.R` only):

```bash
grep -oE '(numericInput|selectInput|radioButtons|checkboxInput|textAreaInput|textInput|sliderInput|fileInput|actionButton|dateInput|selectizeInput)\("[a-zA-Z0-9_]+"' R/app.R \
  | sed -E 's/.*\("([a-zA-Z0-9_]+)"/\1/' | sort -u > /tmp/ids-now.txt
diff .superpowers/sdd/baseline-input-ids.txt /tmp/ids-now.txt
```

Expected diff and nothing else:

```
> check_id_col
> check_progeny_size
> f_check
> f_check_pheno
< check_basis
< exclude_threshold_violators
```

The per-trait `chk_<trait>` / `dir_<trait>` / `basis_<trait>` ids are built inside `renderUI()`
with `paste0()`, so this static scan never saw them and will not show their removal — that is
expected, not a miss. Confirm every line above is intended, then
`cp /tmp/ids-now.txt .superpowers/sdd/baseline-input-ids.txt`.

- [ ] **Step 5: Run the full suite**

```bash
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: 0 failures.

- [ ] **Step 6: Commit**

```bash
git add R/report.R DESCRIPTION NEWS.md .superpowers/sdd/baseline-input-ids.txt
git commit -m "release: nextgenCrossWorkbench 0.27.0 — check reference lines"
```

---

### Task 8: End-to-end verification against the real backend

**Files:**
- Test: `tests/testthat/test-e2e-surfacing.R`

- [ ] **Step 1: Extend the e2e test**

Append to `tests/testthat/test-e2e-surfacing.R`, following the file's existing skip-if-no-backend
pattern:

```r
test_that("check reference surfaces end to end", {
  skip_if_not_installed("nextgenCrossDesign")
  skip_if_not(utils::packageVersion("nextgenCrossDesign") >= "0.23.0")
  res <- ngcd_e2e_fixture_run(with_checks = TRUE)
  expect_false(is.null(res$trait_check_reference))
  ct <- res$candidate_crosses
  expect_true(any(grepl("_check_value$", names(ct))))
  expect_true(any(grepl("_p_beat_check$", names(ct))))
  expect_true("checks_all_ok" %in% names(ct))
  # the check is never a parent
  expect_false(any(c(ct$parent1, ct$parent2) %in% res$trait_check_reference$active$check))
})
```

Add the `with_checks` argument to the `ngcd_e2e_fixture_run()` helper in the same file:

```r
ngcd_e2e_fixture_run <- function(..., with_checks = FALSE) {
  args <- ngcd_e2e_fixture_args(...)          # existing fixture assembly
  if (isTRUE(with_checks)) {
    set.seed(7)
    mk <- colnames(args$geno)
    args$check_geno <- matrix(
      rbinom(2L * length(mk), 2, 0.4), nrow = 2L,
      dimnames = list(c("CHK_A", "CHK_B"), mk))
    args$trait_checks <- data.frame(
      trait = names(args$trait_direction)[[1L]], check = "CHK_A",
      stringsAsFactors = FALSE)
  }
  do.call(nextgenCrossDesign::ng_run_cross_prediction, args)
}
```

If the file's fixture assembly is not already factored into `ngcd_e2e_fixture_args()`, extract
it first — the existing tests must keep calling `ngcd_e2e_fixture_run()` with no arguments and
get exactly what they got before.

- [ ] **Step 2: Run it**

```bash
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-e2e-surfacing.R")'
```

Expected: PASS (or a clean skip if backend 0.23.0 is not installed yet).

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-e2e-surfacing.R
git commit -m "test(e2e): verify check reference columns and diagnostics end to end"
```
