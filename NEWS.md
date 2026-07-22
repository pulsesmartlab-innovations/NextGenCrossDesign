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
