# PR1 — Diminishing-returns chart (A1) + wire the crop control (A2)

> **For agentic workers:** Use superpowers:executing-plans. Steps use `- [ ]` tracking.

**Goal:** Add the missing diminishing-returns chart for the auto cross-number sweep, and make
the `crop` control drive a crop-aware method recommendation — both additive, on-pattern, no
regression to existing tabs.

**Repo:** frontend `~/NextGenCrossDesign` only (calls existing backend functions).
**Version:** DESCRIPTION 0.10.1 → 0.10.2.

## Global Constraints
- Additive only. No existing tab/control/default/inputId changes behavior or look.
- Reuse the plotly theme from `ngcd_frontier_plotly` (colors `#00583d` line, `#FFC425`
  highlight, `#003524` title; horizontal legend; `margin t=40`).
- New runner logic must fail gracefully (never a raw error) when a capability/crop is absent.
- Backend accessed only via the existing out-of-process runner; the Shiny app process does
  NOT call backend functions directly.
- Verify each task: app boots headless (HTTP 200) and existing tabs render unchanged.

---

### Task 1: A1 — `ngcd_diminishing_returns_plotly()` helper

**Files:** Modify `R/helpers.R` (add after `ngcd_frontier_plotly`, ~line 260).

**Interfaces produced:** `ngcd_diminishing_returns_plotly(curve, recommended_k = NULL, criterion = "elbow_relative")` → a plotly object. `curve` is `result$cross_number_sweep$curve`, a data frame with columns `K, total_gain, mean_gain, group_coancestry, unique_parents, marginal_gain, relative_marginal, Ne_estimate`.

- [ ] **Step 1: Add the helper** (mirror the frontier helper's style)

```r
# Diminishing-returns curve for the auto cross-number sweep. x = K (number of crosses),
# y = mean gain; the recommended K (elbow) is highlighted. For the Ne / coancestry criteria
# a secondary trace shows the constrained quantity vs K so the user sees where it binds.
ngcd_diminishing_returns_plotly <- function(curve, recommended_k = NULL,
                                            criterion = "elbow_relative") {
  curve <- as.data.frame(curve, stringsAsFactors = FALSE)
  curve <- curve[order(curve$K), , drop = FALSE]
  htext <- sprintf(paste0("<b>K = %s</b><br>Mean gain: %.4g<br>",
                          "Marginal gain: %.4g<br>Ne: %.3g<br>Group coancestry: %.4g"),
    curve$K, curve$mean_gain, curve$marginal_gain,
    curve$Ne_estimate, curve$group_coancestry)
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = curve$K, y = curve$mean_gain,
    type = "scatter", mode = "lines+markers", name = "mean gain",
    line = list(color = "#00583d", width = 2), marker = list(color = "#00583d", size = 7),
    text = htext, hoverinfo = "text")
  if (!is.null(recommended_k) && is.finite(recommended_k)) {
    yk <- curve$mean_gain[match(as.integer(recommended_k), curve$K)]
    if (length(yk) && is.finite(yk))
      p <- plotly::add_trace(p, x = as.integer(recommended_k), y = yk,
        type = "scatter", mode = "markers", name = "recommended K",
        marker = list(color = "#FFC425", size = 16, line = list(color = "#003524", width = 2)),
        text = sprintf("<b>Recommended K = %d</b><br>Mean gain: %.4g", as.integer(recommended_k), yk),
        hoverinfo = "text")
  }
  # secondary axis for the criterion's constrained quantity
  sec <- if (identical(criterion, "ne_target")) list(col = "Ne_estimate", lab = "Effective size (Ne)")
         else if (identical(criterion, "coancestry_budget")) list(col = "group_coancestry", lab = "Group coancestry")
         else NULL
  if (!is.null(sec) && sec$col %in% names(curve)) {
    p <- plotly::add_trace(p, x = curve$K, y = curve[[sec$col]], yaxis = "y2",
      type = "scatter", mode = "lines", name = sec$lab,
      line = list(color = "#8a6d00", width = 1.5, dash = "dot"), hoverinfo = "skip")
  }
  plotly::layout(p,
    title = list(text = "Diminishing returns: gain vs number of crosses", font = list(color = "#003524")),
    xaxis = list(title = "Number of crosses (K)", zeroline = FALSE),
    yaxis = list(title = "Mean gain", zeroline = FALSE),
    yaxis2 = if (!is.null(sec)) list(title = sec$lab, overlaying = "y", side = "right", zeroline = FALSE) else NULL,
    hovermode = "closest", legend = list(orientation = "h", x = 0, y = -0.22),
    margin = list(t = 40))
}
```

- [ ] **Step 2: Verify the helper builds a plotly object** (standalone)

Run: from `~/NextGenCrossDesign`,
```r
Rscript -e 'source("R/helpers.R"); cv <- data.frame(K=seq(10,40,5), total_gain=cumsum(runif(7)), mean_gain=sort(runif(7),TRUE), group_coancestry=sort(runif(7)), unique_parents=10:16, marginal_gain=runif(7), relative_marginal=runif(7), Ne_estimate=seq(20,32,2)); p<-ngcd_diminishing_returns_plotly(cv, 25, "ne_target"); cat("class:", class(p)[1], "\n")'
```
Expected: `class: plotly`

- [ ] **Step 3: Commit**
```bash
git add R/helpers.R && git commit -m "feat(results): add ngcd_diminishing_returns_plotly helper"
```

### Task 2: A1 — wire the chart into Results + fix the dangling text

**Files:** Modify `R/app.R` — the sweep callout (~1358) and the Results-tab render area (near `res_frontier_plotly`, ~1500/1533).

**Interfaces consumed:** `ngcd_diminishing_returns_plotly()` (Task 1); `res()$cross_number_sweep$curve`.

- [ ] **Step 1: Fix the dangling text** at `app.R:1360` — change "See the diminishing-returns chart on the Report tab." to "See the diminishing-returns chart below." (the chart will live on Results next to the KPIs).

- [ ] **Step 2: Add a `plotlyOutput` slot** in the Results UI, immediately after the sweep-callout `tagList` / KPI cards block (guard with `requireNamespace("plotly")` exactly like the frontier block at ~1500). Only show it when `!is.null(sw$curve)`:
```r
if (!is.null(sw) && is.data.frame(sw$curve) && nrow(sw$curve) > 1 &&
    requireNamespace("plotly", quietly = TRUE))
  bslib::card(bslib::card_header("Choosing the number of crosses"),
    plotly::plotlyOutput("res_diminishing", height = "360px")),
```

- [ ] **Step 3: Add the render** next to `output$res_frontier_plotly` (~1533):
```r
output$res_diminishing <- plotly::renderPlotly({
  r <- res(); shiny::req(r); sw <- r$cross_number_sweep
  shiny::req(is.data.frame(sw$curve) && nrow(sw$curve) > 1)
  ngcd_diminishing_returns_plotly(sw$curve, sw$recommended_k, sw$criterion %||% "elbow_relative")
})
```

- [ ] **Step 4: Verify** app boots and existing tabs unchanged (see Task 4 boot check). Confirm no new/removed inputIds break `ngcd_settings_registry()` (the new output has no inputId, so it does not).

- [ ] **Step 5: Commit**
```bash
git add R/app.R && git commit -m "feat(results): show the diminishing-returns curve for auto cross-number sweeps"
```

### Task 3: A2 — crop-aware recommendation (runner + display)

**Files:** Modify `inst/app/tools/run_cross_prediction_json.R` (compute recommendation), `R/app.R` (display + help text), `R/helpers.R` (crop-name → scenario map).

**Interfaces produced:** result field `crop_recommendation = list(method, family, reason, is_fallback, crop, scenario, validation_scope)` or `NULL`.

- [ ] **Step 1: Add the crop mapping helper** to `R/helpers.R`:
```r
# Map the workbench's display crop names to the backend crop-genome archetype scenario keys.
# Crops without a dedicated archetype fall back to a generic diploid selfing scenario; the
# recommendation flags is_fallback so the UI can say "generic approximation".
NGCD_CROP_SCENARIO <- c(
  "Wheat (spring)" = "bread_wheat_hexaploid_approx", "Wheat (winter)" = "bread_wheat_hexaploid_approx",
  "Durum wheat" = "bread_wheat_hexaploid_approx", "Barley" = "barley_like", "Maize" = "maize_like",
  "Field pea" = "field_pea_like", "Potato" = "potato_tetraploid_stress")
ngcd_crop_scenario_key <- function(crop) {
  if (is.null(crop) || !nzchar(crop)) return(NA_character_)
  k <- NGCD_CROP_SCENARIO[[crop]]
  if (is.null(k)) NA_character_ else k
}
```

- [ ] **Step 2: Compute the recommendation in the runner.** In `run_cross_prediction_json.R`, after the main result is assembled and `n_crosses`/parent count are known (parent count = number of genotype rows), add — guarded so a missing function/crop never errors:
```r
crop_reco <- NULL
crop_name <- raw$crop %||% ""
if (nzchar(crop_name) &&
    exists("ng_crop_aware_policy_select", where = asNamespace("nextgenCrossDesign"))) {
  key <- ngcd_crop_scenario_key(crop_name)              # helper sourced alongside the runner
  n_par <- tryCatch(nrow(read_ids_table(genotype_file)), error = function(e) NA_integer_)
  crop_reco <- tryCatch({
    if (!is.na(key)) {
      sel <- nextgenCrossDesign::ng_crop_genome_select(key)
      rec <- nextgenCrossDesign::ng_crop_aware_policy_select(
        n_parents = as.integer(n_par %||% 30L),
        crop_scenario = sel$scenario, crop = sel$crop,
        harness_model = sel$harness_model, validation_scope = sel$validation_scope)
      list(method = rec$method, family = rec$family, reason = rec$reason,
           is_fallback = isTRUE(rec$is_fallback), crop = sel$crop,
           scenario = sel$scenario, validation_scope = sel$validation_scope, mapped = TRUE)
    } else {
      list(method = NA, family = NA, reason = "no_archetype", is_fallback = TRUE,
           crop = crop_name, scenario = NA, validation_scope = NA, mapped = FALSE)
    }
  }, error = function(e) NULL)
}
result$crop_recommendation <- crop_reco
```
(Reuse the runner's existing genotype-reading helper for the parent count; if none exists,
read the id column count with a minimal `utils::read.csv(..., nrows=…)`-style count. Do NOT
add a heavy re-read — a row count is enough.)

- [ ] **Step 3: Display it** in `R/app.R` on the Results tab (a callout under the KPI block):
```r
if (!is.null(r$crop_recommendation)) {
  cr <- r$crop_recommendation
  if (isTRUE(cr$mapped) && !isTRUE(cr$is_fallback))
    ngcd_callout(kind = "info", shiny::tags$b(sprintf("Crop-aware routing (%s): ", cr$crop)),
      sprintf("recommended method family '%s' (%s).", cr$family %||% "-", cr$reason %||% ""))
  else
    ngcd_callout(kind = "info", shiny::tags$b("Crop-aware routing: "),
      "no validated archetype for this crop; using the general diploid path. Pick a listed crop for a crop-specific recommendation.")
}
```

- [ ] **Step 4: Update the crop input help text** (`app.R` near the `crop` selectizeInput, ~115) — remove any implication it is ignored; add a `help-hint` div: "Used to suggest a crop-aware method family in your results (validated archetypes: wheat, barley, maize, field pea, potato)."

- [ ] **Step 5: Verify** the runner emits `crop_recommendation` for a mapped crop and `NULL`/fallback otherwise — run the JSON runner on a demo config with `"crop":"Barley"` and confirm `result$crop_recommendation$mapped == TRUE`; with `"crop":"Oat"` confirm fallback; with no crop confirm `NULL`. App boots.

- [ ] **Step 6: Commit**
```bash
git add R/app.R R/helpers.R inst/app/tools/run_cross_prediction_json.R
git commit -m "feat(crop): surface a crop-aware method recommendation from the crop selector"
```

### Task 4: Release PR1

- [ ] **Step 1: Bump version** — `DESCRIPTION` Version 0.10.1 → 0.10.2; add a NEWS entry.
- [ ] **Step 2: Boot check** — `Rscript -e 'nextgenCrossWorkbench::run_workbench(...)'`-equivalent HTTP-200 smoke (or `shiny::runApp` load without error); confirm every existing tab renders and the settings registry inputIds are unchanged.
- [ ] **Step 3: Build check** — `R CMD build` (or `devtools::load_all`) with no new errors.
- [ ] **Step 4: Branch, push, PR** — branch `feat/pr1-diminishing-crop`, push, open PR to `main`, merge (frontend main unprotected; verify locally since no CI).

## Notes / risks
- `read_ids_table` in Step 3.2 is a placeholder for the runner's existing input reader — use the real one; if none, a lightweight header+row count. Keep it cheap.
- The crop map covers the 5 validated archetypes; everything else is an honest fallback callout (never a silent no-op or a raw error).
- If `plotly` is unavailable the chart block is simply omitted (matches the frontier fallback).
