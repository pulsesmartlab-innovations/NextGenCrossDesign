# nextgenCrossWorkbench

**NextGenCrossDesign** — a point-and-click Shiny front-end for the
[`nextgenCrossDesign`](#the-backend) genomic cross-prediction and mate-allocation
engine. It turns a genomic-selection cross-design pipeline into a guided,
ten-step web app over plain CSV inputs, with NDSU branding, editable input
tables, an interactive cross-linked HTML report, an automatic cross-number
optimizer, robust (posterior) allocation, and a full polyploid design workflow.

The backend runs in a *separate*, user-configured R process, so this front-end
installs and runs cleanly on its own.

**Authors**

- **Backend** (`nextgenCrossDesign`, the breeding-genetics engine): **Dr. Sikiru Atanda**
- **Front-end** (`nextgenCrossWorkbench`, this Shiny workbench): **Mario Morales**

Developed at North Dakota State University (PulseSmartLab).

---

## Contents

- [Movies: end-to-end workflows](#movies-end-to-end-workflows)
- [What it does](#what-it-does)
- [Install](#install)
- [Run](#run)
- [Guided walkthrough (with screenshots)](#guided-walkthrough)
- [The interactive report](#the-interactive-report)
- [Polyploid / clonal design workflow](#polyploid--clonal-design-workflow)
- [Feature coverage](#feature-coverage)
- [Testing & robustness](#testing--robustness)
- [Scientific references](#scientific-references)
- [The backend](#the-backend)
- [Citation](#citation)

---

## Movies: end-to-end workflows

**Standard genomic cross prediction** — load the bundled demo, set the objective,
score and allocate crosses, run, and read the recommended plan:

![Standard cross-prediction workflow](man/figures/demo-standard-workflow.gif)

**Polyploid design** — switch to the polyploid workflow, set the ploidy, and
produce a dosage-aware crossing plan for an autopolyploid / clonal crop:

![Polyploid design workflow](man/figures/demo-polyploid-workflow.gif)

---

## What it does

The workbench predicts the genetic merit of every possible cross between a set of
genotyped, phenotyped parents, then allocates a mating plan that trades off
**genetic gain** against **diversity** (group coancestry / inbreeding). It
covers the whole decision, from raw CSVs to a shareable report:

- **Genomic cross prediction** — marker-effect estimation, per-cross progeny
  mean and variance, and the *usefulness criterion* (mean + selection intensity ×
  progeny standard deviation) as the default merit metric.
- **Multi-trait objectives** — trait-by-trait or index-as-trait, with explicit,
  direction-aware selection (yield up, disease down) and several combination
  methods (auto, weighted, economic index, desired gain).
- **Optimal mate allocation** — optimum-contribution-style selection that
  balances mean gain against group coancestry, with greedy/evolutionary/MIP
  optimizers and constraints on parent use, kinship, and group quotas.
- **Automatic cross-number optimizer** — instead of fixing the number of crosses,
  the app can sweep K and recommend a number using a diminishing-returns
  (elbow / kneedle), effective-population-size floor, or coancestry-budget rule.
- **Robust (posterior) allocation** — re-optimize using posterior quantiles or
  top-N probabilities so the plan is stable under prediction uncertainty.
- **Polyploid / clonal design** — a dedicated dosage-aware workflow for
  autopolyploids and clonal crops, with ploidy-aware GRM, dominance/heterosis
  modeling, double reduction, and ploidy-aware QC.
- **A self-contained interactive report** — executive summary, KPIs, and eight
  interactive figures, cross-linked and saveable to PDF or standalone HTML.

Inputs are **plain CSVs** you can edit in-app (double-click a cell). Every screen
has a collapsible "how to use this step" guide, and a live status bar shows
whether the backend is connected.

---

## Install

Two packages, installed independently — the front-end does **not** need the
backend in the same R library.

```r
# 1. the backend (compiles native code — needs Rtools/Xcode/build-essential)
#    Requires nextgenCrossDesign >= 0.4.0 (see required_backend_version in config.yml).
remotes::install_github("pulsesmartlab-innovations/nextgenCrossDesignR@v0.4.0")

#    …or from a local source tarball:
R CMD INSTALL nextgenCrossDesign_0.4.0.tar.gz

# 2. this front-end — from CRAN once published:
install.packages("nextgenCrossWorkbench", dependencies = TRUE)

#    …or from a local source tarball:
install.packages("nextgenCrossWorkbench_0.10.0.tar.gz",
                 repos = NULL, type = "source", dependencies = TRUE)
```

All front-end dependencies are declared in `Imports` and pulled automatically:
`shiny`, `bslib`, `DT`, `jsonlite`, `yaml`, `base64enc`, `plotly`, `parallel`,
plus base `utils`/`grDevices`/`graphics`/`stats`. If your R can't reach CRAN,
install the imports first:

```r
install.packages(c("shiny","bslib","DT","jsonlite","yaml","base64enc","plotly"))
```

The interactive report and figures use `plotly` (now a hard dependency, so
charts are interactive out of the box). Excel workbook export and static PNG
figure export are optional and live in `Suggests` (`openxlsx`, `ggplot2`),
because they run in the backend process; the app degrades gracefully without
them.

---

## Run

```r
library(nextgenCrossWorkbench)

# First time on a machine: create a working folder with a config.yml to edit
init_workbench_dir("~/cross-workbench")
#   -> edit ~/cross-workbench/config.yml: set rscript_path and package_library

run_workbench("~/cross-workbench")   # opens the app in your browser
```

`config.yml` points the app at the R installation and library where
`nextgenCrossDesign` lives (it can be a different R than the one running the UI).
Any key can be overridden by an environment variable, e.g. `NGCD_RSCRIPT_PATH`.

### Developer mode

By default the app shows only the analysis workflow (Data → Results). The
**Setup** screen and the **Save / Load settings** profile tools are hidden so end
users aren't exposed to configuration plumbing. Turn them on while configuring a
machine by setting `developer_mode: true` in `config.yml`, appending `?dev=1` to
the app URL, or setting `NGCD_DEVELOPER_MODE=true`.

---

## Guided walkthrough

The workbench is organized as a ten-step navbar. Each screen below is shown with
the bundled demo (10 inbred parents, 12 markers, traits *yield* ↑ and *disease* ↓).

### 1. Setup — connect to your R & backend

Confirm the app can reach your R installation and the `nextgenCrossDesign`
engine before doing anything else. Green badges mean Rscript, the runner, the
backend package, and its version are all in order; an optional-capabilities panel
shows which extra features (lpSolve, AlphaSimR, openxlsx, …) are available.

![Setup screen](man/figures/screen-01-setup.png)

### 2. Data — load, map, and edit inputs

Load the bundled demo or upload your own CSVs (comma/semicolon/tab separated; a
UTF-8 byte-order mark from Excel is handled automatically). Column mapping is
auto-guessed and adjustable, data-alignment checks flag ID/marker mismatches, and
every input table is **editable in place** — double-click a cell to change it and
re-run.

![Data screen](man/figures/screen-02-data.png)

### 3. Objective — what you are selecting for

Choose trait-by-trait or index-as-trait prediction, tick the traits to include,
and pick a multi-trait combination method. Trait directions are explicit, so risk
traits can be selected *downward*.

![Objective screen](man/figures/screen-03-objective.png)

### 4. Scoring — merit metric & breeding system

Select the trait-value metric (default `var_complex`, the usefulness criterion),
the breeding system (DH or RIL), recombination and GRM methods, and the selection
proportion. "Assume inbred parents" can be toggled for non-inbred material.

![Scoring screen](man/figures/screen-04-scoring.png)

### 5. QC — clean the data before running

Duplicate-parent detection, marker-missingness and MAF filters, optional LD
pruning, and a residual-heterozygosity audit that mirrors the backend's inbred
model. Heterozygous parents that would break a DH/RIL model are flagged with a
one-click exclusion.

![QC screen](man/figures/screen-05-qc.png)

### 6. Allocation — build the mating plan

Set the **number of crosses** (fixed, or let the automatic optimizer sweep K and
recommend a value), the diversity balance (a gain-vs-coancestry dial), the
optimizer (greedy / evolutionary / MIP / AlphaMate-style), and constraints on
parent use, kinship, quotas, and progeny inbreeding. A robust-allocation card
re-optimizes on posterior quantiles for stability under uncertainty.

![Allocation screen](man/figures/screen-06-allocation.png)

### 7. Advanced — fine control

Marker-target steering, lethal-allele handling, cost/logistic penalties, budget
constraints, and posterior-prediction (MCMC) options for programs that need them.

![Advanced screen](man/figures/screen-07-advanced.png)

### 8. Output — what to write

Choose whether to write an Excel crossing-plan workbook (needs `openxlsx`) and
static PNG figures (needs `ggplot2`), set the random seed for reproducibility, and
name the output files.

![Output screen](man/figures/screen-08-output.png)

### 9. Run — execute

One click assembles a JSON config, materializes any edited tables, and drives the
backend in its configured R process. Errors are surfaced in a debug panel with
plain-language hints for the most common failures.

![Run screen](man/figures/screen-09-run.png)

### 10. Results — the plan and the evidence

A KPI row (crosses, mean gain, group coancestry, unique parents, max parent use,
mean progeny inbreeding) sits above sub-tabs for the ranked plan, candidate
scores, parent use, the gain-diversity frontier, QC audit, input matching, marker
effects, and method/settings provenance.

Selected crosses (ranked, priority-tiered):

![Selected crosses](man/figures/screen-11-selected-crosses.png)

The gain-diversity frontier, with your chosen plan marked:

![Gain-diversity frontier](man/figures/screen-14-frontier.png)

Parent-use distribution across the plan:

![Parent use](man/figures/screen-13-parent-use.png)

Marker-effect reliability per trait:

![Marker effects](man/figures/screen-17-marker-effects.png)

---

## The interactive report

The **Report** tab renders a self-contained, cross-linked HTML report: an
executive summary in plain language, the KPI row, and eight interactive `plotly`
figures (priority tiers, multi-trait score distribution, selected-vs-all scatter,
gain-diversity frontier, cross-number diminishing-returns curve when the
auto-optimizer ran, parent use, trait-model reliability, and a trait-rank
heatmap). Charts are hover-, zoom-, and pan-able; the whole thing can be
downloaded as standalone HTML (plotly.js inlined, works offline) or PDF.

![Interactive report](man/figures/screen-10-report.png)

---

## Polyploid / clonal design workflow

Switching the analysis type to **Polyploid** on the Data screen exposes a
dosage-aware design path (`ng_design_crosses_poly`) for autopolyploids and clonal
crops: upload a dosage matrix (0..ploidy) and a single-trait phenotype, set the
ploidy, and optionally model dominance/heterosis, double reduction, and a
ploidy-aware GRM. The bundled demo is a tetraploid clone panel.

Polyploid data setup (ploidy = 4):

![Polyploid data setup](man/figures/screen-19-polyploid-data.png)

The resulting polyploid crossing plan — per-cross mean, variance, dominance-aware
usefulness, parent GEBVs, and pairwise kinship:

![Polyploid plan](man/figures/screen-20-polyploid-plan.png)

---

## Feature coverage

The front-end exposes the full parameter surface of the backend's cross-prediction
entry point and its optimization, robustness, and polyploid routines:

| Area | What's exposed |
|------|----------------|
| Prediction | trait-by-trait / index-as-trait; marker-effect reliability floor; posterior (MCMC) prediction |
| Merit | `var_complex` (usefulness), `uc`, `pmv`, `vpm`, `mean`, `var_simple`; UC variance source; PMV method |
| Multi-trait | auto / weighted / economic-index / desired-gain; soft/strict thresholds with autoscaled penalties |
| Breeding system | DH / RIL (infinite or finite selfing); Haldane/Kosambi; VanRaden/Yang GRM |
| Allocation | OCS / greedy / evolutionary / MIP / AlphaMate-style; parent-use, kinship, quota, inbreeding constraints |
| Cross number | fixed, or automatic sweep with elbow / kneedle / Ne-floor / coancestry-budget selection |
| Robustness | posterior-quantile and top-N-probability re-optimization |
| Polyploid | dosage 0..ploidy design; dominance/heterosis; double reduction; ploidy-aware GRM & QC |
| QC | duplicate detection, missingness/MAF filters, LD pruning, residual-heterozygosity audit |
| Output | interactive HTML + PDF report; Excel workbook; PNG figures; reproducible seed |

---

## Testing & robustness

The package ships an extensive `testthat` suite (Config/testthat/edition 3) and
passes `R CMD check` with **status OK** (no errors, warnings, or notes).

- **370+ assertions across 14 test files.** Unit tests cover the IO/formatting
  helpers (delimiter and BOM auto-detection, quoted fields, Latin-1 fallback,
  number formatting, column guessing, the heterozygosity audit at diploid and
  tetraploid ploidy), the report layer (figure-registry integrity, applicability
  filtering, render-ready JSON serialization, self-contained interactive HTML,
  PDF, and graceful degradation on empty/sparse results), config and
  developer-mode gating, settings save/restore round-trips, run-directory and
  config-serialization edge cases, and error-hint mapping.
- **Server-logic tests** drive the reactive layer with `shiny::testServer`,
  including regression tests for column-mapping crashes and wide-genotype ID
  selection.
- **Backend-gated integration tests** (run when `nextgenCrossDesign` is present)
  exercise real runs: the combination smoke sweep, the automatic cross-number
  sweep, robust posterior allocation, the residual-heterozygosity fix, and the
  full polyploid design.

Run the fast suite with:

```r
devtools::test()                     # non-backend tests
# or, including backend integration tests:
Sys.setenv(NOT_CRAN = "true", NGCD_RUN_COMBINATIONS = "1")
devtools::test()
```

Debugging hardening in this release includes byte-safe CSV reading in any locale,
byte-perfect inlining of `plotly.js` (fixing blank charts under a C locale), a
guarded `plotly` JSON serializer with a `jsonlite` fallback (no fragile `:::`
call), ASCII-only UI source (portable-package clean), and display-safe report
summaries that never surface a raw `NA`.

---

## Scientific references

The methods surfaced by this workbench draw on the following literature.

**Genomic prediction and relationship matrices**

- Meuwissen, T.H.E., Hayes, B.J., & Goddard, M.E. (2001). Prediction of total
  genetic value using genome-wide dense marker maps. *Genetics* 157(4):1819–1829.
- VanRaden, P.M. (2008). Efficient methods to compute genomic predictions.
  *Journal of Dairy Science* 91(11):4414–4423.
  [doi:10.3168/jds.2007-0980](https://doi.org/10.3168/jds.2007-0980)

**Cross usefulness and progeny-variance prediction**

- Schnell, F.W. & Utz, H.F. (1975). The usefulness criterion (F1 progeny mean +
  selection response) for evaluating crosses — the foundational concept, later
  formalized for genomic prediction.
- Lehermeier, C., Teyssèdre, S., & Schön, C.-C. (2017). Genetic gain increases by
  applying the usefulness criterion with improved variance prediction in
  selection of crosses. *Genetics* 207(4):1651–1661.
  [doi:10.1534/genetics.117.300403](https://doi.org/10.1534/genetics.117.300403)
- Wolfe, M.D., Chan, A.W., Kulakow, P., Rabbi, I., & Jannink, J.-L. (2021).
  Genomic mating in outbred species: predicting cross usefulness with additive
  and total genetic covariance matrices. *Genetics* 219(3):iyab122.
  [doi:10.1093/genetics/iyab122](https://doi.org/10.1093/genetics/iyab122)

**Optimal contribution selection & genomic mating**

- Meuwissen, T.H.E. (1997). Maximizing the response of selection with a
  predefined rate of inbreeding. *Journal of Animal Science* 75(4):934–940.
- Akdemir, D. & Sánchez, J.I. (2016). Efficient breeding by genomic mating.
  *Frontiers in Genetics* 7:210.
  [doi:10.3389/fgene.2016.00210](https://doi.org/10.3389/fgene.2016.00210)

**Polyploid relationship matrices & design**

- Amadeu, R.R., Cellon, C., Olmstead, J.W., Garcia, A.A.F., Resende, M.F.R., &
  Muñoz, P.R. (2016). AGHmatrix: R package to construct relationship matrices for
  autotetraploid and diploid species — a blueberry example. *The Plant Genome*
  9(3). [doi:10.3835/plantgenome2016.01.0009](https://doi.org/10.3835/plantgenome2016.01.0009)

**Mapping functions**

- Haldane, J.B.S. (1919). The combination of linkage values, and the calculation
  of distances between the loci of linked factors. *Journal of Genetics* 8:299–309.
- Kosambi, D.D. (1944). The estimation of map distances from recombination values.
  *Annals of Eugenics* 12:172–175.

**Related software**

- Peixoto, M.A., Coelho, I.F., Leach, K.A., Lübberstedt, T., Bhering, L.L., &
  Resende, M.F.R. (2025). SimpleMating: R-package for prediction and optimization
  of breeding crosses using genomic selection. *The Plant Genome*.
  [doi:10.1002/tpg2.20533](https://doi.org/10.1002/tpg2.20533)

---

## The backend

`nextgenCrossDesign`, authored by **Dr. Sikiru Atanda**, is the breeding-genetics
engine that performs marker-effect estimation, per-cross prediction, mate
allocation, the cross-number sweep, robust posterior optimization, and polyploid
design. This workbench is a thin, well-tested UI over that engine: it assembles a
JSON configuration, invokes the backend in its own R process, and renders the
results. Because the two are decoupled, the backend can be upgraded independently
and can live in a different R installation.

---

## Citation

If you use this workbench in published work, please cite both components:

> Atanda, S. *nextgenCrossDesign: genomic cross prediction and mate allocation*
> (R package). North Dakota State University.
>
> Morales, M. *nextgenCrossWorkbench: NextGenCrossDesign — a Shiny
> front-end for nextgenCrossDesign* (R package, v0.10.0). North Dakota State
> University, PulseSmartLab - PI: Dr. Sikiru Atanda.

---

*NextGenCrossDesign. Backend (nextgenCrossDesign): Dr. Sikiru Atanda.
Front-end (workbench): Mario Morales.*
