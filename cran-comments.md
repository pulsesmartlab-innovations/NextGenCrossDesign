## Submission notes

This is the first CRAN submission of nextgenCrossWorkbench.

nextgenCrossWorkbench is a `shiny` front-end for genomic cross prediction and
mate allocation. The breeding-genetics computations are performed by a companion
engine, `nextgenCrossDesign`, which is invoked as a *separate, user-configured R
process* (an external subprocess), not loaded into the checking session. The
front-end installs, loads, and launches its user interface independently, so the
package checks and its non-interactive tests run without the backend present.
The backend requirement is documented in `SystemRequirements` rather than
declared as an R package dependency, because it is never attached in-process.

## Test environments

* Local: Ubuntu 24.04, R 4.3.3
* R CMD check --as-cran

## R CMD check results

0 errors | 0 warnings | 0 notes

Interactive functions (`run_workbench()`, `workbench_app()`) launch a Shiny app
and are wrapped in `\dontrun{}`. `init_workbench_dir()` writes only to a
user-supplied directory (its example uses `tempdir()`). Backend-dependent tests
are skipped on CRAN via `testthat::skip_on_cran()` and a runtime backend probe.
