# Stage-centric Run pipeline — design

**Date:** 2026-08-08
**Repo:** `NextGenCrossDesign` (frontend `nextgenCrossWorkbench`), branch `feat/stage-centric-run`
**Builds on:** `.superpowers/sdd/phase2-plan.md` (staged orchestration + compute-once engine) and
`phase3-plan.md` (per-activity figure tags). Those shipped in 0.18.0.

## Problem

The staged pipeline works in the backend and the compute-once engine is correct, but the **presentation
is scattered**, so the promised professional UX is not realized:

- The per-stage Run buttons (`Run QC`, `Fit effects & score`, `Build selection index`) are buried as
  secondary `uiOutput`s inside the Data/Configure sub-tabs.
- The per-stage figures are **collapsed on-demand tags**, hidden by default and again buried in Configure.
- The prominent call-to-action is a single **"Run cross prediction"** button on the Run tab, which for
  `write_outputs`/`write_figures`/auto-cross-number silently runs the **fully monolithic** `do_run()` —
  QC and everything fire at once.
- Every figure + table lands lumped in the **Results** tab (12+ sub-tabs).

Net effect for the user: QC is not a visible first-class step, neither is the multi-trait index build,
and no figure appears next to the stage that produced it.

## Non-negotiable invariants (do NOT regress)

1. **Compute-once.** A step never re-runs work the user already completed. This is already enforced by
   `ngcd_pipeline_mark()` / `ngcd_next_stages()` / `run_stage_manual()` (single-stage). The redesign
   **reuses this engine untouched** — no changes to `R/helpers.R` pipeline functions or the staleness
   observer (`app.R:1320`).
2. **Diagnostics stay at the diagnostic (stage) level.** A stage's diagnostic figure lives in that
   stage's card. Only output genuinely relevant to the *final plan* is surfaced in Results.
3. **Compute path unchanged.** `ngcd_run_stage()`, the stage JSON shapes, `do_run()`, `do_run_pipeline()`,
   and the poly/subgenome single-shot path are not modified. This is a UI reorganization.
4. **Input-ID conservation.** Net Shiny input IDs must not change except for intentional additions
   (the new "Run all steps" button and any moved-but-same-id buttons keep their ids: `run_qc`,
   `run_predict`, `run_index`, `run`). Figures use `uiOutput`/`plotlyOutput` (not input ids). A
   baseline input-id snapshot is taken before work and diffed after.

## Design

### Run tab = the pipeline

Replace the Run tab body (`app.R:556-586`, the single-button card) with a **vertical sequence of
stage-cards**. Each card is self-contained:

```
┌ N · <Stage title> ──────────── [Run <stage>]   <status badge> ┐
│  <one-line result summary once run>                            │
│  <inline figure(s) once run; "run this step" note if not>      │
└───────────────────────────────────────────────────────────────┘
```

- **status badge** reuses `stage_status_badge()` (locked / ready / running / done / stale ↻ / blocked).
- **Run button** reuses the existing ids/observers: `run_qc`, `run_predict`, `run_index`, and the
  final `run` (Allocate & rank). Each observer already calls `run_stage_manual(<stage>)` /
  `do_run_pipeline()` which honor compute-once.
- **Result summary** is a new small `uiOutput` per card, derived from the stored stage JSON
  (`rv$pipeline$stages[[s]]$json`), e.g. QC → "0 blockers, 2 warnings"; predict → "3 traits scored ·
  45 candidate crosses"; index → "index over 3 traits (economic weights)"; allocate → "K crosses ·
  mean gain G · coancestry C".
- **Inline figure(s)** reuse the existing renderers (`fig_qc_plot`, `fig_predict_plot`,
  `fig_index_plot`, `fig_allocate_frontier`, `fig_allocate_parents`) but rendered **open in the card**
  (not inside a collapsed `<details>`). A stage not yet `done` shows a short "Run this step to see its
  figure" note instead.

### Adaptive card set (by objective / workflow)

| Mode | Cards shown |
|------|-------------|
| Multi-trait (`objective_mode = "multi"`) | QC · Fit & score · **Build selection index** · Allocate & rank |
| Single trait / supplied index column (`single`/`index`) | QC · Fit & score · Allocate & rank (index step hidden — it is trivial/auto) |
| Polyploid / disomic-subgenome | **single "Run" card** (unchanged `do_run()`); no step pipeline |

The index card's visibility keys off `objective_mode` (already an input). The poly/subgenome branch
reuses the existing `is_poly()`/`is_subgenome()` guards.

### "Run all remaining steps"

A secondary button at the top of the Run tab (id `run` is taken by Allocate; add `run_all`) that calls
`do_run_pipeline()` — which already walks only the non-done stages (compute-once) and hosts the
auto-cross-number sweep + artifact export fallbacks. This preserves one-click convenience without
losing any capability. For poly/subgenome the top button is just the normal single Run.

### Results tab — trimmed to the final plan

Keep in Results: KPI row, Report, Selected crosses, Candidate scores, Parent use, Family sizes, Robust
plan, Polyploid plan, Downloads, provenance (Input matching, Marker effects, Method & settings), and the
opt-in **Portfolio & risk** and **Pareto explorer** (final-plan analyses — genuinely results-relevant).

Move OUT of Results into their stage cards (diagnostics): the QC putative-duplicate figure, the
trait-model reliability figure, and the index-distribution figure. The **gain–diversity frontier** and
**parent-use** figures are the Allocate stage's output *and* final-plan relevant, so they appear in the
Allocate card; the standalone "Gain-diversity frontier" Results sub-tab is removed (the frontier remains
reachable in the Allocate card and inside the Report).

### Configure tab — options only

Remove the three run-button `uiOutput`s and the four `ngcd_figure_tag()` disclosures from the
Data/Configure sub-tabs. Keep the **live pre-run trait/index distribution preview** on the Selection-
objective screen (it is an input preview, not a stage output). Configure returns to being purely the
place you set options; Run is where you execute.

## Testing

- **Compute-once (the load-bearing test):** a `testthat` `testServer` test that runs QC then predict,
  changes an *allocation-only* input, and asserts `qc` and `predict` stay `done` (not re-run) while
  `allocate`/`rank` go `stale` — proving a later change does not restart earlier steps. (Exercises the
  existing engine through the new UI wiring.)
- **Adaptive cards:** unit-test a pure helper `ngcd_run_stage_cards(objective_mode, is_poly, is_sub)`
  that returns the ordered list of stage ids to show (multi → 4, single/index → 3, poly/sub → the single
  run). Assert each mapping.
- **Summary builders:** pure functions `ngcd_stage_summary(stage, json)` → one-line string; unit-test
  each stage's shape (using the documented stage-JSON shapes) incl. empty/NULL json → "not run yet".
- **Input-ID conservation:** snapshot input ids before, diff after; expect only `run_all` added.
- **Smoke:** the existing `test-server-logic` / `test-pipeline-state` continue to pass; the rendered UI
  contains the stage cards and no longer renders the removed Results figure tabs.

## Out of scope

Backend changes; the compute-once engine; the stage-JSON contract; poly/subgenome compute path; report
content. Purely the Run/Results/Configure *presentation* and small pure helpers for card selection +
summaries.
