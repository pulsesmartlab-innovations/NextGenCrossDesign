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
