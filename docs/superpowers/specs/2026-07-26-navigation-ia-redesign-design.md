# Navigation IA redesign: workflow stages + intuitive names — design

**Date:** 2026-07-26
**Status:** Approved for planning (brainstorm complete)
**Scope:** Frontend `NextGenCrossDesign` (`nextgenCrossWorkbench`) — `R/app.R` UI + `R/helpers.R`
(`ngcd_guide`) + user-facing help text. Pure UI reorganization; no server logic / input-ID changes.

---

## 1. Goal & problem

The app is a `bslib::page_navbar` with **~10 flat top-level tabs** (Setup, Data, Objective, Scoring,
Trait checks, QC, Allocation, Advanced, Output, Run, Results) — a long, undifferentiated bar with no
sense of workflow, and jargon/overlapping names (Objective vs Scoring; Allocation; Advanced; Output
vs Results). Redesign to a **4-stage workflow** with breeder-intuitive names, reusing the app's
existing pattern (Data and Results already are top-level tabs holding an internal secondary tab-row).

## 2. Target structure

Top nav (left→right), Setup kept **dev-only** (`if (isTRUE(dev))`, unchanged):

- **Data** — top-level `nav_panel` with an internal `navset_tab`:
  `Genotype · Phenotype · Marker map · Trait direction · Data quality`
  (the old top-level **QC** panel body moves in here as **Data quality**).
- **Configure** — NEW top-level `nav_panel` with an internal `navset_tab` holding the 6 config
  sections (each = the existing panel body, moved verbatim, retitled):
  `Breeding goal · Prediction & scoring · Mate selection · Trait checks · Fine-tuning · Export options`
- **Run** — unchanged top-level `nav_panel`.
- **Results** — unchanged top-level `nav_panel` (its 15 internal sub-tabs are OUT OF SCOPE this round).

### 2.1 Rename map (top-level panel titles → new)
| old title | new title | old body location | new location |
|---|---|---|---|
| Objective | **Breeding goal** | top-level | Configure sub-tab |
| Scoring | **Prediction & scoring** | top-level | Configure sub-tab |
| QC | **Data quality** | top-level | **Data** sub-tab |
| Allocation | **Mate selection** | top-level | Configure sub-tab |
| Trait checks | Trait checks (unchanged label) | top-level | Configure sub-tab |
| Advanced | **Fine-tuning** | top-level | Configure sub-tab |
| Output | **Export options** | top-level | Configure sub-tab |

Panel **bodies move verbatim** — the `shiny::*Input(...)` calls inside keep their **exact input IDs**,
so `build_params`, all reactives, and `conditionalPanel`s are unaffected.

## 3. Mechanism (bslib)

- `Data` and `Results` already wrap an internal `bslib::navset_tab(...)` inside a `nav_panel` — mirror
  that exactly for `Configure`: `bslib::nav_panel("Configure", bslib::navset_tab( <6 nav_panels> ))`.
- Add the moved **Data quality** `nav_panel` to Data's existing internal `navset_tab`
  (R/app.R:168-172).
- Internal navsets need no `id` unless referenced; none are referenced today, so omit ids (consistent
  with the existing Data/Results internal navsets).

## 4. Guided tour (`ngcd_guide`) — renumber + retitle + fill the gap

`ngcd_guide(step, total, title, body, next_hint, open)` (R/helpers.R:25) renders a "Step N of total —
<title>" collapsible help box per panel. Current calls are a linear 10-step model matching the flat
tabs. After the redesign:

- **Retitle** each guide to the new name (Objective→"Breeding goal", etc.).
- **Renumber linearly** to the new encountered order (Setup is dev-only, numbered 0 or left as its
  own; the breeder-visible sequence is 1..10):
  1 Data · 2 Data quality · 3 Breeding goal · 4 Prediction & scoring · 5 Mate selection ·
  6 Trait checks · 7 Fine-tuning · 8 Export options · 9 Run · 10 Results.
- **Add the missing Trait-checks guide box** (`ngcd_guide(6, 10, "Trait checks", ...)`) — the
  Trait-checks panel currently has none.
- **Fix the `next_hint` chaining** so each step's "Next →" points at the next section in the new order
  (e.g. Data → "Data quality", Export options → "Run").
- Keep the "Step N of total" wording (low-risk); do not redesign `ngcd_guide` itself.

## 5. Help-text cross-references

User-facing text references the OLD tab names and must be updated to the new ones, or it directs
breeders to a tab that no longer exists. Known sites (verify by grep, there may be more):
- R/app.R:1647, 1684, 1746 — Report/diagnostics guidance saying "on the **Allocation** screen…" →
  "**Mate selection** screen".
- Any "Output"/"Advanced"/"Objective"/"Scoring"/"QC" mentions in help/report/diagnostics text →
  new names.
- The backend `R/diagnostics.R` "What to change" suggestions may name screens (e.g. "on the
  Allocation screen") — those live in the BACKEND repo and are OUT OF SCOPE here; note as a
  follow-up if any breeder-facing suggestion names a renamed tab. (This spec covers the frontend
  `nextgenCrossWorkbench` only.)

## 6. Invariants (must hold — this is UI-only)

- **No input ID changes** — every `input$*` id in the moved panels stays identical; a pre/post grep
  of input IDs must be equal.
- `nav_select("nav", "Results")` (R/app.R:1476) unchanged — Results stays top-level.
- `conditionalPanel`s (keyed on `input.workflow` / `input.data_source` / `input.prediction_mode`,
  never on the nav/tab title) unchanged and moved with their panel bodies.
- `build_params` and all server reactives unchanged.
- Setup stays behind `if (isTRUE(dev))`.

## 7. Testing / verification

Pure-UI change; verify structurally:
- `Rscript -e 'invisible(parse("R/app.R")); cat("parses\n")'`.
- The app object builds: `workbench_app()` returns a `shiny.appobj` (no error).
- **Input-ID conservation:** capture the set of `input$<id>` / `inputId="..."` before and after; assert
  identical (no input dropped/renamed).
- Boot HTTP 200 (the existing boot check used for prior frontend pushes).
- Visual/manual: the top bar shows Data · Configure · Run · Results; Configure's 6 sub-tabs and Data's
  5 sub-tabs render; each guide box shows the new title/number; no help text names a removed tab
  (grep the old names in user-facing strings → none remain except historical/NEWS).

## 8. Rollout

Frontend **0.17.0** (notable user-visible UX change; no functional/behaviour change). NEWS entry.
Push to `main` (unprotected, no PR). **No `Co-Authored-By` trailer on any commit** (user is sole
contributor of record). Image rebuild (to make it live) is the separate parked step.

## 9. Out of scope
- Grouping the 15 Results sub-tabs (separate follow-on).
- Redesigning `ngcd_guide` itself (stepper/wizard) — kept as-is, only renumbered/retitled.
- Backend `R/diagnostics.R` screen-name suggestions (backend repo).
- Any behaviour/logic change.
