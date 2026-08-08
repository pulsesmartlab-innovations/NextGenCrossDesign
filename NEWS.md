# nextgenCrossWorkbench 0.21.0

* **Stage-centric Run pipeline.** The Run tab is now the pipeline itself: a
  vertical sequence of step-cards — **1 · Quality control → 2 · Fit effects &
  score → 3 · Build selection index → 4 · Allocate & rank** — where each step has
  its **own Run button, status badge, one-line result summary, and its figure(s)
  shown inline** in the card. Quality control and the multi-trait selection-index
  build are now first-class steps you run on their own, each with its own figure,
  instead of being buried in Configure or lumped into one run whose output all
  landed in Results.
    - **Compute-once, preserved and tested.** Running a later step never re-runs a
      step you already completed; changing a setting marks only the affected step
      and the steps after it for a re-run, while earlier steps stay done. (Backed
      by a new test that changes an allocation-only setting and asserts QC / Fit /
      Index stay `done`.)
    - **Adaptive.** Single-trait and index-column modes show three steps (the
      index build is hidden); polyploid and disomic-subgenome designs keep their
      single one-shot Run card. A **Run all remaining steps** button walks whatever
      is left in one click (and still hosts the auto cross-number sweep + workbook/
      figure export).
    - **Configure is options only.** The per-stage run buttons and diagnostic
      figures moved out of the Data/Configure sub-tabs into their Run cards; the
      live pre-run trait/index distribution preview stays on the Selection
      objective screen. Results keeps the final plan (ranked crosses, report,
      downloads, KPIs, Portfolio/Pareto, gain–diversity frontier).

# nextgenCrossWorkbench 0.20.0

* **Parent type selector (Inbred / DH / RIL).** Prediction & scoring now has a
  **Parent type** dropdown that surfaces the backend's residual-heterozygosity
  governance directly, replacing the old *"Assume inbred parents"* checkbox (which
  drove the now-deprecated `assume_inbred` backend flag). `Inbred` and `Doubled
  haploid (DH)` expect fully fixed lines and block heterozygous parents as a data
  error; `Recombinant inbred line (RIL)` accepts the residual heterozygosity RILs
  retain after finite selfing. The choices are read from the backend capability
  registry, so the experimental-status gate applies automatically. Data-quality
  het warnings and run-blocking hints now point at the selector. A settings
  profile saved with the old checkbox migrates automatically (ticked → `Inbred`,
  unticked → `RIL`).
* Raised `required_backend_version` to **0.17.2**, which advertises `parent_type`
  in the capability registry and ships the exact residual-het-parent variance.

# nextgenCrossWorkbench 0.19.1

* **Experimental capabilities never surface in the UI.** `ngcd_control_choices()`
  now drops any dropdown choice the backend capability registry marks
  `status = "experimental"` or `"guarded"`, and that registry status overrides a
  hardcoded frontend fallback that still lists the value. The backend registry is
  the single point of retraction: marking a capability experimental removes it
  from the workbench with no frontend edit (VALIDATED_STATE frontend-surfacing
  governance).

# nextgenCrossWorkbench 0.19.0

* **Breeder-intuitive cross-scoring metric names.** The "Cross-scoring metric"
  dropdown now reads `Mid-parent mean`, `Family variance`,
  `Reliable family variance`, `Usefulness`, `Parent distance` instead of the
  raw backend tokens (`mean`/`vpm`/`pmv`/`var_complex`/`parent_distance`). The
  underlying merit is unchanged; the frontend just talks to you in the
  vocabulary you'd use with a colleague.
* **Family/reliable variance as a scoring objective in its own right.**
  Picking `Family variance` or `Reliable family variance` now scores and ranks
  crosses on segregating variation alone (a pure-variance objective), not only
  as an input to `Usefulness`.
* **Conditional variance-source and variance-accuracy controls.** The
  "Usefulness variance source" dropdown only appears when the metric is
  `Usefulness`, and the "Variance accuracy" (fast vs. full-posterior) dropdown
  only appears when a reliable/family-variance calculation is actually in
  play — the previous version showed both unconditionally, which read as
  relevant even when the chosen metric ignored them.
* **Legacy settings-profile migration.** A `.json` settings profile saved
  before this rework (raw `var_complex`/`pmv`/`vpm`/`mean` tokens) now
  restores correctly: `var_complex`/`pmv` → `Usefulness` +
  `Reliable family variance`, `vpm` → `Usefulness` + `Family variance`,
  `mean` → `Mid-parent mean`. Restoring an old profile no longer leaves the
  metric dropdown blank.
* Raised `required_backend_version` to **0.16.0**, which ships the friendly
  metric vocabulary in the backend capability registry that drives these
  dropdowns.

# nextgenCrossWorkbench 0.18.0

* **Staged, gated, activity-connected pipeline.** The standard cross-prediction
  workflow now runs as four explicit, manual steps instead of one monolithic
  run: **Run QC → Fit effects & score → Build selection index → Allocate &
  rank**. Each step's button is enabled only once its upstream step is done, so
  you walk the pipeline deliberately; nothing runs automatically.
    - **QC is a real gate.** Quality control runs as its own certified step and
      **blocks the design only on blockers** — warnings pass through. A
      duplicate-genotype or other blocker disables the downstream steps (and the
      backend refuses them) until you resolve it.
    - **Compute-once.** Each stage's result is cached; a downstream stage never
      recomputes upstream work. Changing a setting marks only that stage and the
      steps after it as needing a re-run — untouched upstream stages stay done.
      Staged execution is **byte-identical** to the old one-shot run.
    - **On-demand "Figure" tags.** Each activity screen carries a small
      collapsible *Figure* tag that reveals that step's chart in place — putative
      duplicates after QC, trait-model reliability after Fit & score, the
      computed index distribution after Build index, and the gain–diversity
      frontier plus parent-use after Allocate & rank. A tag whose step has not
      run yet shows a short "Run this step" note; none of them trigger a run.
    - **Live trait/index distribution.** The Selection objective screen shows the
      spread of your chosen trait or index in the loaded phenotype **before** any
      backend run, updating as you change the objective.
* Auto cross-number selection and output/figure writing continue to use the
  full one-shot run, so the diminishing-returns sweep chart and the exported
  workbook/figures are produced exactly as before. Polyploid and
  disomic-subgenome designs are unchanged (they keep their single-run path).
* Raised `required_backend_version` to **0.15.0**. The staged pipeline calls the
  backend's new `ng_run_stage()` entry point, which does not exist before
  backend 0.15.0, so the runtime version gate must require it — otherwise an
  older separately-installed backend would pass the gate and then fail at call
  time.

# nextgenCrossWorkbench 0.17.4

* **Reverted the static PDF GEBV labels from 0.17.3.** Printing the mid-parent
  GEBV directly onto the trait-rank heatmap cells and scatter points crowded the
  static figures. The mid-parent GEBV is instead revealed on **hover** in the
  interactive report (as added in 0.17.2) — the intended, uncluttered UX — and the
  PDF figures return to their clean form.

# nextgenCrossWorkbench 0.17.3

* **Mid-parent GEBV labels in the PDF report too.** The static PDF versions of the
  trait-rank heatmap and the priority-tier scatter now print each cross's
  mid-parent GEBV directly on the figure — the value in every heatmap cell
  (white-on-dark for contrast) and beside every selected scatter point — matching
  the hover tooltips added to the interactive report in 0.17.2. Both figures note
  that the label is the mid-parent GEBV.

# nextgenCrossWorkbench 0.17.2

* **Report plots now tie back to real parent values on hover.** The
  direction-aware trait-rank heatmap (the z-score plot) and the "Selected vs all
  candidates" priority-tier scatter now show each cross's **mid-parent GEBV**
  (`<trait>_mean_gebv`) in the hover tooltip, so an abstract z-score or priority
  tier reads against the actual predicted trait value. A heatmap cell shows the
  cross, trait, z-score, and mid-parent GEBV; a scatter point shows the cross, its
  multi-trait score, and the mid-parent GEBV for every trait. Traits without a
  mid-parent GEBV column fall back to "--".

# nextgenCrossWorkbench 0.17.1

* **Disomic-subgenome results no longer show blank fields.** The subgenome design
  path returns a compact result (no group-coancestry scalar, priority tiers, or
  standard metric/method settings), which previously left the Results KPI row and
  the report's executive summary full of `--`/`?` placeholders. The workbench now
  backfills the derivable values (unique parents, maximum parent use, mean pairwise
  kinship) from the selected crosses, shows a subgenome-specific KPI row
  (Subgenomes and Mean pair kinship in place of the inapplicable Group coancestry
  and Mean progeny F), and renders a dedicated, placeholder-free executive summary
  describing the subgenome design (subgenomes, markers per subgenome, variance
  model, GRM, progeny target, and OCS allocation).

# nextgenCrossWorkbench 0.17.0

* **Navigation IA redesign.** The flat ~10-tab navbar is reorganized into four
  workflow stages — **Data · Configure · Run · Results** (plus a developer-only
  Setup). **Configure** now holds five sections in pipeline order: *Selection
  objective*, *Prediction & scoring*, *Cross filters & genetic constraints*,
  *Mate allocation*, *Export options*. The old "Advanced" junk-drawer is dissolved
  into those sections, the former top-level "QC" becomes a *Data quality* sub-tab
  under Data, and *Mate allocation* groups its controls into *Plan size &
  constraints* / *Gain-diversity & relatedness* / *Engine & advanced* panels.
* **Selection objective is now breeder-framed** with a three-way choice: a
  single trait, multiple traits (build a selection index), or your own
  pre-computed selection-index column. Single- and multiple-trait modes both run
  full trait-by-trait prediction (usefulness/UC, within-family variance,
  risk/portfolio, and the per-trait check veto); the index-column mode scores the
  supplied index directly.
* **Guided tour** boxes are collapsed by default with a single "Show guided tour"
  toggle, and their stale "Step N of 10" counters are reconciled to the new stage
  structure.
* Swept stale tab/mode references out of all help, diagnostic, and report text,
  and fixed the settings save/restore registry to capture the new objective
  controls.

# nextgenCrossWorkbench 0.16.0

* Surface the backend cross-priority **risk & portfolio** decision layer (backend
  0.13.0): a 'Portfolio & risk' tab plotting genetic level x within-family SD
  (colour = estimation risk) with iso-genetic-usefulness contours, and priority-risk /
  portfolio run notes in the Report. Single-trait runs only; hidden otherwise.
* Surface the per-trait **check-threshold veto** (backend 0.14.0): a 'Trait checks'
  panel with a per-trait check-line picker (GEBV or phenotype basis), optional
  hard-exclude, an Excel per-trait mid-parent GEBV toggle, and trait-check run notes.

# nextgenCrossWorkbench 0.15.1

* Documentation: the vignette now covers the diminishing-returns chart, crop
  suitability note, registry-driven menus, family-size allocation, multi-trait
  joint P(superior progeny), the Pareto frontier explorer, and disomic-subgenome
  design.

# nextgenCrossWorkbench 0.15.0

## New features

* **Pareto frontier explorer.** A new option on the Allocation screen sweeps an explicit
  diversity-penalty (lambda) grid via `ng_pareto_mate_allocation()` and shows the whole
  gain-vs-diversity trade-off on a new *Results > Pareto explorer* tab (frontier chart + a points
  table). Complements the single auto diversity dial by letting you see and pick from the full
  curve. Off by default; does not change the main plan.

# nextgenCrossWorkbench 0.14.0

## New features

* **Disomic-subgenome design (true allopolyploids).** A new *Disomic subgenome* analysis type for
  true allopolyploids where each subgenome is inherited diploidly (dosage 0..2 per subgenome). You
  declare each marker's subgenome via a marker-map column; the workbench splits the dosage and runs
  everything subgenome-aware -- per-subgenome QC, per-subgenome ridge effects, the subgenome GRM,
  subgenome cross scoring, and a coancestry-aware allocation. Not crop-specific (most crops, e.g.
  wheat and canola, are genotyped as diploid -- use Standard for those).

# nextgenCrossWorkbench 0.13.0

## New features

* **Multi-trait joint P(superior progeny).** A new option on the Objective screen adds a
  `p_superior_progeny_mt` column: the probability a cross throws progeny that clear the target on
  *every* selected trait at once, using the estimated cross-trait genetic covariance (a joint
  superiority a per-trait probability misses). Targets default to each trait's population mean in
  its selection direction and can be overridden per trait. Multi-trait runs only; off by default.

# nextgenCrossWorkbench 0.12.0

## New features

* **Family-size allocation.** A new *Family sizes* card on the Allocation screen lets you set a
  total progeny budget; the backend (`ng_allocate_family_sizes()`) distributes it across the
  selected crosses in proportion to their merit, with optional min/max per family. Results appear
  on a new *Results > Family sizes* tab as a per-cross progeny table and bar chart. Off by default.

# nextgenCrossWorkbench 0.11.0

## New features

* **Registry-driven controls.** The workbench now reads the backend capability registry
  (`ng_backend_capability_registry()`, schema v2) at startup and derives its method / metric
  dropdown options from it, so new or renamed backend methods appear without a UI edit. Existing
  labels are preserved; only genuinely-new options are appended. This immediately surfaces
  options the hardcoded lists were missing (e.g. the `threshold` multi-trait method, and the
  `DHs` / `RILs` progeny systems). Falls back to the built-in lists against an older backend.

# nextgenCrossWorkbench 0.10.2

## New features

* **Diminishing-returns chart.** The automatic cross-number sweep now renders the
  gain-vs-K curve on Results with the recommended K (elbow) highlighted, plus an Ne /
  coancestry overlay for those stopping rules. Previously only the recommended number was
  shown and the "see the chart" text pointed nowhere.
* **Crop-aware suitability note.** The `crop` selector (previously collected but ignored)
  now drives a crop-suitability callout in the results via `ng_crop_aware_policy_select()`:
  it states how well the diploid DH/RIL approach is validated for the crop (directly
  supported vs a diploidized/stress approximation) and switches to a polyploid-appropriate
  method for complex polyploids. Wheat is treated as diploid.

# nextgenCrossWorkbench 0.10.1

Lockstep release for the backend API-naming program (nextgenCrossDesign 0.7.0).

## Changes

* Raised `required_backend_version` to **0.7.0**. The workbench now calls the
  renamed backend API (e.g. `parent_kinship`, the `pmv` / `vpm` /
  `parent_distance` score columns, and the `ng_polyploid_*` functions), none of
  which exist before 0.7.0, so the runtime version gate must require it. This
  fixes a too-loose pin that would let a separately-installed 0.4.0 backend pass
  the gate and then fail at call time.
* Deployment: the ShinyProxy container image now bundles backend 0.7.0
  (`ghcr.io/pulsesmartlab-innovations/ngcd-workbench:0.10.1`).

# nextgenCrossWorkbench 0.10.0

Release focused on robustness, documentation, and CRAN readiness.

## New features

* Added a **Diagnostics & tuning** tab (and a matching report section) that
  explains why each procedure produced its result and which parameter to change
  to steer it: automatic cross-number recommendations that hit the edge of the
  swept range, binding pairwise-kinship / parent-use / unique-parent
  constraints, low trait reliability, robust-vs-point-estimate disagreement, and
  QC blockers. Each item is graded CHECK / NOTE / OK.
* All runtime dependencies are now declared in `Imports` (including `plotly`
  and `parallel`), so the interactive report and figures work out of the box.
* Added a package vignette walking through the standard, robust, and polyploid
  cross-design workflows.
* Added a comprehensive `README` with annotated screenshots and animated
  workflow demonstrations of every screen.

## Improvements

* Authorship recorded in `DESCRIPTION`: Mario Morales (front-end / maintainer)
  and Sikiru Atanda (author of the `nextgenCrossDesign` backend engine).
* The in-app footer and the generated HTML report now credit both the backend
  and the front-end authors.
* Extended the automated test suite to 370+ assertions across 14 files,
  including edge cases for delimiter/BOM/encoding-robust CSV reading, the
  heterozygosity audit at diploid and tetraploid ploidy, report figure
  rendering on empty and sparse results, config serialization precision, and
  error-hint mapping.

## Bug fixes

* Replaced a fragile `plotly:::` internal call in the report serializer with a
  guarded lookup and a `jsonlite` fallback.
* Removed a non-ASCII character from the app source so the package is portable.
* The report executive summary no longer prints a raw `NA` when a run has no
  selected crosses.
* Byte-perfect inlining of `plotly.js` fixes blank charts under a C locale.

# nextgenCrossWorkbench 0.9.0

* Added the dosage-aware polyploid / clonal design workflow
  (`ng_design_crosses_poly`): ploidy-aware GRM and QC, dominance/heterosis
  modeling, and double reduction.

# nextgenCrossWorkbench 0.8.0

* Added the automatic cross-number optimizer (diminishing-returns, effective
  population size, and coancestry-budget selection rules) and robust
  (posterior-quantile / top-N-probability) mate allocation.

# nextgenCrossWorkbench 0.7.0

* Robust, locale-safe CSV reading (delimiter and byte-order-mark auto-detection,
  Latin-1 fallback); one-click residual-heterozygosity exclusion; editable
  input-data tables.
