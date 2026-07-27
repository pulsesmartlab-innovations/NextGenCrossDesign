# nextgenCrossWorkbench

**NextGenCrossDesign** — a point-and-click Shiny front-end for the
[`nextgenCrossDesign`](#the-backend) genomic cross-prediction and mate-allocation
engine. It turns a genomic-selection cross-design pipeline into a guided,
four-stage web app (Data · Configure · Run · Results) over plain CSV inputs, with
NDSU branding, editable input tables, an interactive cross-linked HTML report, an
automatic cross-number optimizer, robust (posterior) allocation, and a full
polyploid design workflow.

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
- [Diagnostics & tuning](#diagnostics--tuning)
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
- **Flexible selection objective** — pick a **single trait**, **multiple traits**
  combined into a selection index, or **your own pre-computed index column**, with
  explicit direction-aware selection (yield up, disease down) and several
  combination methods (auto, weighted, economic index, desired gain). Optionally
  score each cross by its **joint probability of superior progeny** — the chance a
  cross yields offspring that clear the target on *all* traits at once.
- **Breeder decision controls** — a per-trait **check-line veto** (screen crosses
  against a reference line before allocation), a unified **mate-relatedness** dial,
  and a single-trait **portfolio & risk** view (genetic level × within-family
  upside × estimation risk) layered on the priority tiers.
- **Optimal mate allocation** — optimum-contribution-style selection that
  balances mean gain against group coancestry, with greedy/evolutionary/MIP
  optimizers and constraints on parent use, kinship, and group quotas.
- **Automatic cross-number optimizer** — instead of fixing the number of crosses,
  the app can sweep K and recommend a number using a diminishing-returns
  (elbow / kneedle), effective-population-size floor, or coancestry-budget rule.
- **Pareto / breeder explorer** — walk the full gain–diversity frontier and pick
  the plan that matches your appetite for gain versus long-term diversity, rather
  than committing to a single trade-off up front.
- **Family-size allocation** — split a fixed total-progeny budget across the
  selected crosses by merit, with per-family minimum and maximum sizes, so the
  best crosses get proportionally more seed.
- **Robust (posterior) allocation** — re-optimize using posterior quantiles or
  top-N probabilities so the plan is stable under prediction uncertainty.
- **Polyploid / clonal design** — dosage-aware workflows for **autopolyploids**
  (potato-like, tetrasomic) with ploidy-aware GRM, dominance/heterosis, double
  reduction, and ploidy-aware QC, plus a **disomic-subgenome (allopolyploid)**
  path whose within-family variance is recombination-aware and GRM is
  VanRaden/Yang, computed per subgenome.
- **A self-contained interactive report** — executive summary, KPIs, and eight
  interactive figures, cross-linked and saveable to PDF or standalone HTML.

Inputs are **plain CSVs** you can edit in-app (double-click a cell). Every screen
has a collapsible "how to use this step" guide, and a live status bar shows
whether the backend is connected.

---

## Two ways to run it

- **Individual, on your own machine — no Docker.** Install the two R packages and
  call `run_workbench()`. That's it. The app runs in `local` mode by default:
  your runs are kept in an `ngcd-data` folder beside your working directory and
  persist across sessions (the most recent `keep_runs` are retained — default 20;
  set `keep_runs: 0` to keep every run). Docker is **not** involved and **not**
  needed. Follow **Install** and **Run** below.
- **Hosted for many users on a server — Docker.** Deploy the containerised app
  under ShinyProxy (one container per user). This is the only scenario that uses
  Docker. It runs in `server` mode: per-session, ephemeral run storage. See
  [`deploy/`](deploy/) for the Dockerfile, ShinyProxy config, and instructions.

The mode is controlled by `deployment_mode` (`local` / `server`, or the
`NGCD_DEPLOYMENT_MODE` env var). Individual users never set it — `local` is the
default; the container image sets `server`.

## Install

Two packages, installed independently — the front-end does **not** need the
backend in the same R library.

```r
# 1. the backend (compiles native code — needs Rtools/Xcode/build-essential)
#    Floor is nextgenCrossDesign >= 0.7.0 (required_backend_version in config.yml);
#    0.14.0 adds the per-trait check-line veto, portfolio/risk and constraint
#    diagnostics, and the unified mate-relatedness control surfaced by this app.
remotes::install_github("pulsesmartlab-innovations/nextgenCrossDesignR@v0.14.0")

#    …or from a local source tarball:
R CMD INSTALL nextgenCrossDesign_0.14.0.tar.gz

# 2. this front-end — from CRAN once published:
install.packages("nextgenCrossWorkbench", dependencies = TRUE)

#    …or from a local source tarball:
install.packages("nextgenCrossWorkbench_0.17.0.tar.gz",
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

### Updating the backend

The workbench runs the backend out-of-process (see `SystemRequirements`), so the
backend is a **separately installed R package**, not an automatic dependency —
reinstalling the workbench does not refresh it. When the backend publishes a new
tagged release, refresh it with:

```r
Rscript tools/update-backend.R          # installs the version this workbench requires
Rscript tools/update-backend.R 0.4.1    # or a specific version
```

The script reads `required_backend_version` from `config.template.yml`, installs
the matching `vX.Y.Z` tag from
`pulsesmartlab-innovations/nextgenCrossDesignR`, and verifies the result. The
backend repo is private, so `install_github` needs a GitHub token with repo read
scope (`Sys.setenv(GITHUB_PAT = "ghp_…")`); to skip the token, install from a
built tarball instead:

```r
NGCD_BACKEND_TARBALL=/path/nextgenCrossDesign_0.4.1.tar.gz Rscript tools/update-backend.R
```

You rarely need to touch the app for a backend update: the workbench calls the
backend by name and filters run parameters against the installed backend's live
formals, so new or changed backend parameters are picked up on the next run once
the package is reinstalled. Bump `required_backend_version` (in `config.yml` /
`config.template.yml`) only when the workbench needs to *require* a newer backend
— the app then warns at startup if an older one is installed.

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

The workbench is organized as **four workflow stages** in the top navbar —
**Data · Configure · Run · Results** (plus a developer-only Setup) — that follow
the order you actually work in. Each panel below is shown with the bundled demo
(10 inbred parents, 12 markers, traits *yield* ↑ and *disease* ↓).

> The screenshots show the individual configuration **panels**; version 0.17
> regrouped the navigation from a flat ten-tab bar into the four stages described
> here, so a panel's controls are unchanged but now live under the stage noted in
> each heading.

**Setup** *(developer-only, hidden in production)* confirms the app can reach your
R installation and the `nextgenCrossDesign` engine — green badges for Rscript, the
runner, the backend package and its version, plus an optional-capabilities panel
(lpSolve, AlphaSimR, openxlsx, …).

![Setup screen](man/figures/screen-01-setup.png)

### Data — load, check, and edit inputs

Load the bundled demo or upload your own CSVs (comma/semicolon/tab separated; a
UTF-8 byte-order mark from Excel is handled automatically). Column mapping is
auto-guessed and adjustable, data-alignment checks flag ID/marker mismatches, and
every input table is **editable in place** — double-click a cell to change it and
re-run.

![Data screen](man/figures/screen-02-data.png)

A **Data quality** sub-tab holds duplicate-parent detection, marker-missingness and
MAF filters, optional LD pruning, and a residual-heterozygosity audit that mirrors
the backend's inbred model — heterozygous parents that would break a DH/RIL model
are flagged with a one-click exclusion.

![Data quality (QC) sub-tab](man/figures/screen-05-qc.png)

### Configure — every design choice, in five sections

**1. Selection objective.** The breeder question "what does *good* mean?" as a
three-way choice: a **single trait**, **multiple traits (build a selection index)**,
or **use my selection-index column** (a pre-computed index already in your
phenotype file). Single- and multiple-trait modes both run full trait-by-trait
prediction; for multiple traits you pick the combination method (automatic,
relative weights, economic weights, desired gains) and can add a **joint
P(superior progeny)** column. Trait directions are explicit, so risk traits are
selected *downward*.

![Selection objective screen](man/figures/screen-03-objective.png)

**2. Prediction & scoring.** The effect & variance model (recombination and GRM
methods, marker-effect reliability floor, optional training-set augmentation and
posterior engine), the **cross-value metric** (default `var_complex`, the
usefulness criterion), and the breeding system (DH or RIL) with the selection
proportion. "Assume inbred parents" can be toggled for non-inbred material.

![Prediction & scoring screen](man/figures/screen-04-scoring.png)

**3. Cross filters & genetic constraints.** Candidate-level screens applied
*before* allocation: the **per-trait check-line veto** (flag or remove crosses
whose mid-parent for a trait is worse than a reference line, on GEBV or phenotype
basis), lethal-allele guarding, and marker-target steering.

![Cross filters & genetic constraints screen](man/figures/screen-07-advanced.png)

**4. Mate allocation.** The **number of crosses** (fixed, or let the automatic
optimizer sweep K and recommend a value), the gain-vs-coancestry dial, the unified
**mate-relatedness** control (avoid inbreeding / favor complementarity), the
optimizer (greedy / evolutionary / MIP / AlphaMate-style), constraints on parent
use, kinship and quotas, a family-size budget, the Pareto explorer, and a
robust-allocation card that re-optimizes on posterior quantiles for stability
under uncertainty.

![Mate allocation screen](man/figures/screen-06-allocation.png)

**5. Export options.** Whether to write an Excel crossing-plan workbook (needs
`openxlsx`) and static PNG figures (needs `ggplot2`), and the random seed for
reproducibility.

![Export options screen](man/figures/screen-08-output.png)

### Run — execute

One click assembles a JSON config, materializes any edited tables, and drives the
backend in its configured R process. Errors are surfaced in a debug panel with
plain-language hints for the most common failures.

![Run screen](man/figures/screen-09-run.png)

### Results — the plan and the evidence

A KPI row (crosses, mean gain, group coancestry, unique parents, max parent use,
mean progeny inbreeding) sits above sub-tabs for the ranked plan, candidate
scores, parent use, the **Portfolio & risk** view, the gain-diversity frontier,
QC audit, input matching, marker effects, and method/settings provenance.

Selected crosses (ranked, priority-tiered):

![Selected crosses](man/figures/screen-11-selected-crosses.png)

Per-cross candidate scores (mean, variance, usefulness, kinship) for every
predicted cross, not just the selected ones:

![Candidate scores](man/figures/screen-12-candidate-scores.png)

The gain-diversity frontier, with your chosen plan marked:

![Gain-diversity frontier](man/figures/screen-14-frontier.png)

**Pareto / breeder explorer.** The Pareto explorer sweeps the diversity-penalty
(lambda) grid and lays out every optimal plan along the gain-vs-diversity frontier,
so you can step along it and adopt the plan that matches your appetite for gain
versus long-term diversity — instead of being locked into one trade-off. Each
frontier point is a fully-specified plan (gain, coancestry, unique parents):

![Pareto explorer](man/figures/screen-24-pareto-explorer.png)

**Cross-number optimizer.** When you let the app choose the number of crosses
(Objective screen → *Number of crosses: automatic*), it sweeps K and reports the
diminishing-returns curve below, marking the recommended K under your chosen rule
(relative-marginal / kneedle elbow, effective-population-size floor, or
coancestry budget). Total gain rises steeply, then flattens as added crosses buy
less merit — the recommendation is where that trade-off turns. The same curve
appears in the interactive report. In R this is `ng_optimize_mating_plan_curve()`
+ `ng_plot_diminishing_returns()`.

![Cross-number optimizer — diminishing returns](man/figures/screen-22-cross-number-optimizer.png)

Parent-use distribution across the plan:

![Parent use](man/figures/screen-13-parent-use.png)

**Portfolio & risk** *(single-trait runs).* Two crosses with the same score can be
very different bets. This tab decomposes each cross into its genetic **level**
(mid-parent breeding value) and **upside** (within-family genetic SD, √VPM),
coloured by an **estimation-risk** bin, so you can read each cross's *profile* —
a high-level **workhorse**, a high-upside **breakthrough**, a **long shot**, or a
cross to **deprioritize** — instead of a single opaque ranking. It is a
decision-support view on top of the priority tiers and never changes how crosses
are scored or allocated.

![Portfolio & risk](man/figures/screen-27-portfolio-risk.png)

**Family-size allocation.** Turn a fixed total-progeny budget into a per-cross
seed plan: the best crosses get proportionally more progeny, bounded by per-family
minimum and maximum sizes. In R this is `ng_allocate_family_sizes()`.

![Family-size allocation](man/figures/screen-23-family-size.png)

**Multi-trait joint P(superior progeny).** For a multi-trait run you can also score
each cross by the probability it throws progeny clearing the target on *every* trait
at once (using the estimated cross-trait covariance) — a joint superiority that a
per-trait probability misses. It surfaces as a `p_superior_progeny_mt` column and a
results callout:

![Multi-trait joint P(superior progeny)](man/figures/screen-25-multitrait-joint.png)

Marker-effect reliability per trait:

![Marker effects](man/figures/screen-17-marker-effects.png)

QC audit — duplicate parents, missingness/MAF, and residual heterozygosity:

![QC audit](man/figures/screen-15-qc-audit.png)

Input matching — the exact parent/marker/column matching used for the run:

![Input matching](man/figures/screen-16-input-matching.png)

Method & settings provenance — the full configuration that produced the plan:

![Method and settings](man/figures/screen-18-method-settings.png)

---

## Diagnostics & tuning

Every procedure — the automatic cross-number optimizer, mate allocation, robust
posterior re-optimization, trait reliability, and QC — can leave a plan looking
"off". A **Diagnostics & tuning** section in the interactive report — together
with inline run notes on the **Results** screen (e.g. the crop-suitability,
joint-superiority, and "number of crosses chosen automatically" callouts above) —
explains *why* each procedure produced its result and *which parameter to change*
to steer it. Each item is graded **CHECK** (act on it), **NOTE** (worth knowing),
or **OK** (stable).

For example, the single most common reason an automatic cross-number
recommendation looks wrong is that it hit the edge of the swept range — the elbow
was never actually reached, so the number is capped by your range, not by the
data. The workbench detects this and tells you exactly what to do. Typical
diagnostics include: the cross-number recommendation sitting at the top
or bottom of the swept range (widen it), a binding pairwise-kinship or
parent-use cap (loosen or tighten it), a trait with very low marker-effect
reliability dominating the score (down-weight or drop it), how many candidate
crosses a **trait-check veto** flagged or removed, the mix of **portfolio
profiles** and estimation-risk bins in the plan, robust and point-estimate plans
disagreeing (move the robustness quantile or gather more training data), and
blocking QC issues (resolve them and re-run). These are backed by the backend's
`constraint_diagnostics` and (single-trait) `priority_risk_diagnostics`.

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
dosage-aware design path (`ng_polyploid_design_crosses`) for autopolyploids and
clonal crops: upload a dosage matrix (0..ploidy) and a single-trait phenotype, set
the ploidy, and optionally model dominance/heterosis, double reduction, and a
ploidy-aware GRM. The bundled demo is a tetraploid clone panel.

For **true allopolyploids** — species whose subgenomes are inherited *diploidly*
(each coded 0..2) — the **Disomic subgenome** analysis type runs everything
subgenome-aware: per-subgenome QC, marker effects, and VanRaden/Yang GRM, plus a
**recombination-aware** within-family usefulness variance (exact per-subgenome,
summed) when the marker map carries chromosome + cM positions. Autopolyploids such
as potato (dosages 0..4) use the *Polyploid* path instead — the subgenome path is
only for species whose subgenomes each segregate as a diploid. A bundled
disomic-subgenome demo (two subgenomes, with a cM map so the variance is
recombination-aware) lets you try it immediately:

![Disomic-subgenome results](man/figures/screen-26-subgenome-results.png)

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
| Selection objective | single trait / multiple traits (built into an index) / your own pre-computed index column; marker-effect reliability floor; posterior (MCMC) prediction |
| Merit | `var_complex` (usefulness), `uc`, `pmv`, `vpm`, `mean`, `var_simple`; UC variance source; PMV method |
| Multi-trait | auto / weighted / economic-index / desired-gain; soft/strict thresholds with autoscaled penalties; joint probability of superior progeny across all traits |
| Cross filters | per-trait check-line veto (GEBV/phenotype basis, flag or exclude); lethal-allele guarding; marker-target steering |
| Decision support | single-trait portfolio & risk profile (level × upside × estimation risk); `constraint_diagnostics` / `priority_risk_diagnostics` run notes |
| Breeding system | DH / RIL (infinite or finite selfing); Haldane/Kosambi; VanRaden/Yang GRM |
| Allocation | OCS / greedy / evolutionary / MIP / AlphaMate-style; parent-use, kinship, quota constraints; unified mate-relatedness control |
| Cross number | fixed, or automatic sweep with elbow / kneedle / Ne-floor / coancestry-budget selection |
| Pareto explorer | walk the gain–diversity frontier and adopt any optimal plan along it |
| Family size | split a total-progeny budget across selected crosses by merit, with per-family min/max |
| Robustness | posterior-quantile and top-N-probability re-optimization |
| Polyploid | autopolyploid dosage 0..ploidy design (dominance/heterosis, double reduction, ploidy-aware GRM & QC); disomic-subgenome allopolyploid path with recombination-aware per-subgenome variance & VanRaden/Yang GRM |
| QC | duplicate detection, missingness/MAF filters, LD pruning, residual-heterozygosity audit |
| Output | interactive HTML + PDF report; Excel workbook; PNG figures; reproducible seed |

---

## Testing & robustness

The package ships an extensive `testthat` suite (Config/testthat/edition 3) and
passes `R CMD check` with **status OK** (no errors, warnings, or notes).

- **430+ assertions across 20 test files.** Unit tests cover the IO/formatting
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
> front-end for nextgenCrossDesign* (R package, v0.17.0). North Dakota State
> University, PulseSmartLab - PI: Dr. Sikiru Atanda.

---

*NextGenCrossDesign. Backend (nextgenCrossDesign): Dr. Sikiru Atanda.
Front-end (workbench): Mario Morales.*
