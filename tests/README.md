# Tests

Two layers.

## 1. Fast unit tests (no backend)

Pure front-end logic — config loading, column detection, heterozygosity /
marker / ID alignment checks, `build_params()` assembly (including the
`traits_to_use` omission and gain-diversity-dial mapping), config JSON
round-trip, edited-cell handling. These run without the backend and cover the
behaviours behind every error we've fixed.

Run them:

```r
# from the package source directory
testthat::test_dir("tests/testthat")
# or a full check
# R CMD check
```

## 2. Backend combination sweep

`run_combination_tests()` (exported) executes many parameter combinations on
the bundled demo data through the real backend and reports which succeed or
fail. It covers one-at-a-time sweeps of every major option plus a factorial
core (metric x optimizer x method x progeny).

```r
library(nextgenCrossWorkbench)
res <- run_combination_tests("~/cross-workbench", level = "full")  # or "smoke"
subset(res, !ok)          # inspect any failures
```

Or standalone from the shell:

```sh
Rscript inst/combination_tests/run_combinations.R  "~/cross-workbench"  full
```

Results are written to `combination_results.csv`.

The sweep is also wired into testthat as `test-combinations.R`, but it is
skipped during ordinary `R CMD check`. Enable it with:

```sh
NGCD_RUN_COMBINATIONS=1 Rscript -e 'testthat::test_dir("tests/testthat")'
```

### Expected failures

A couple of combinations fail *by design* on the demo data and are not
regressions:

- `multi_trait_method = economic_index` / `desired_gain` require economic
  weights in the trait-direction file, which the demo does not provide.

The standalone script treats only *other* failures as a non-zero exit.
