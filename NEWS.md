# nextgenCrossWorkbench 0.31.0

* Requires backend `nextgenCrossDesign` >= 0.29.0, which blocks invalid user-supplied
  covariance matrices rather than solving an index from them. A supplied P/G pair is now
  refused when it implies a heritability above 1 (`P - G` not positive semidefinite), when
  an implied genetic correlation exceeds 1, when the matrix an index must invert is
  numerically singular, or when asymmetry exceeds a scale-relative tolerance. Each refusal
  names the traits and the offending number.

# nextgenCrossWorkbench 0.30.0

Requires backend nextgenCrossDesign >= 0.27.0 (`inst/BACKEND_VERSION`, enforced at run time).

**The two real selection indices are back: Economic index (Smith-Hazel) and Desired gains
(Pesek-Baker).** Until now the only multi-trait method the app offered was a rank sum -- it
respects your ordering of the traits, but it knows nothing about their variances,
heritabilities or genetic correlations. Both index methods were removed from the dropdown
earlier because the app had no way to supply the phenotypic (P) and additive-genetic (G)
covariance matrices they solve from, so every run with either was a guaranteed hard error.
Backend 0.27.0 accepts those matrices, and the app now collects them.

* **Import P and G.** A new optional card on the Data screen (`6 · Trait covariance matrices`)
  takes one CSV each: a square traits x traits table with the trait names in the first column
  AND as the column headers. Order does not matter -- the labels do. Both matrices are
  optional; neither is used by Automatic or Relative weights.
* **Validated before the run, never during it.** Each file is checked for: readable and
  square; row labels and column headers naming the same traits; every entry finite; symmetric
  within the backend's own 1e-8 tolerance (naming the two cells that disagree and by how
  much); positive variances on the diagonal; positive semidefinite; and -- for the matrix the
  chosen index actually has to invert -- non-singular and well enough conditioned to invert
  meaningfully, reporting the condition number when it is not. That last check has no
  equivalent in the backend, which ridges and pseudo-inverts: a near-singular P or G there
  produces plausible-looking coefficients made of rounding error, with nothing to notice.
* **Trait labels survive the JSON bridge -- provably.** `jsonlite` drops `dimnames` on a matrix
  round trip, and the app drives the backend by writing config JSON. A bare matrix would
  therefore arrive unlabelled and be read POSITIONALLY, which the backend cannot tell apart
  from a reordering; on a 3-trait permutation the backend measured Smith-Hazel coefficients
  moving by max |db| = 0.1708 and the emitted index re-ranking crosses at Spearman 0.9168,
  silently. So P and G are serialised in long form -- `{traits, cells:[{trait_row, trait_col,
  value}]}` -- with every value carrying its own row AND column label, and the runner rebuilds
  each matrix by a tiling assert: the p^2 cells must exactly cover traits x traits, with no
  unknown label, no duplicate and no gap, or the run stops. A permuted-but-labelled matrix
  now gives a bit-identical index to the correctly-ordered one, asserted end to end against a
  real backend run.
* **A program-wide matrix is subset for you.** Backend 0.27.0 errors on a label the run does
  not use, so a matrix covering more traits than the current index is narrowed here (to the
  traits the TRAIT-DIRECTION file declares, filtered by your trait selection -- exactly the
  set the backend builds the index over), and the card says which traits it ignored.
* **Desired gains needs G alone.** Its coefficients `b = G^-1 d` never touch P; P only scales
  the reported predicted response by the index SD `sqrt(b' P b)`. A G-only run is therefore
  allowed and is fully valid -- and when the reported predicted response and index standard
  deviation come back blank, the Results screen now says why and that the index and the cross
  ranking are unaffected, instead of showing empty cells.
* **Nothing is hidden when a matrix is missing.** Both methods stay in the dropdown whatever is
  loaded. What changes is that the note under the dropdown, and the run gate, NAME the missing
  matrix (P, G, or both) and point at the card -- and also name the `economic_weight` /
  `desired_change` column the chosen index needs in your trait-direction file.
* **The interim refusal of a self-promoting direction file is gone.** A positive
  `economic_weight` / `desired_change` column makes `Automatic` promote itself to the matching
  index inside the backend. That used to be refused outright, because the app could not supply
  the matrices the promotion implies. It now runs when the matrices are there, and is refused
  with the specific missing matrix named when they are not.
* Swapping in a different P or G invalidates the compute-once **index** stage (and everything
  downstream), so a changed matrix can never leave a stale index on screen.

# nextgenCrossWorkbench 0.29.0

Requires backend nextgenCrossDesign >= 0.26.0 (`inst/BACKEND_VERSION`, enforced at run time).

Three multi-trait defects. The first two changed numbers a breeder was shown, so **any
multi-trait result produced by 0.28.0 or earlier with "Robust posterior allocation" or the
multi-trait joint probability turned on should be re-run.**

* **The "Robust plan" on a multi-trait run was a SINGLE-TRAIT plan, chosen by the row order of
  your trait-direction file.** Your multi-trait plan is ranked on the selection index over every
  trait. The robust re-optimisation was run on `posterior_predictions[[1]]` -- the per-trait
  posterior of whichever trait happened to be listed FIRST in the direction CSV -- with that
  trait's usefulness as the gain column and that trait's orientation. Every other trait was
  discarded without a word, and the Results screen still called it "your robust plan". On the
  demo data the "robust plan" shared **2 of 6** crosses with the index plan; swapping the two
  rows of the direction file changed the plan AND flipped its orientation (maximize to
  minimize, because the newly-first trait is a decrease trait). Reordering two spreadsheet
  rows changed the robust answer.
  A multi-trait run now robust-allocates on the **index posterior itself**
  (`multi_trait_score`), which backend 0.26.0 returns as `posterior_multitrait` with the exact
  cached robustness quantile and its own orientation metadata -- so the robust plan and the
  standard plan are now two views of the same merit, and the plan does not move when the
  direction file is reordered. Single-trait runs are unchanged. If the index posterior is not
  available (posterior prediction off, or a backend older than 0.26.0), robust allocation is
  **refused with the reason** instead of silently falling back to trait 1. The Results screen
  now names which basis was used, and badges the index posterior's per-draw re-standardisation.
* **The multi-trait joint "probability of superior progeny" was wrong three ways at once.**
  Measured on the demo data (10 parents, 45 crosses, yield increase / disease decrease):
  the mean joint probability moves from **0.936 to 0.826**, with a **maximum per-cross change
  of 0.92** and a Spearman correlation of only 0.88 between the old and new rankings -- so
  individual crosses, not just the average, were misreported.
  - *Wrong variance.* It passed each trait's `_pmv` as the per-progeny variance. The joint
    probability is an order statistic over k progeny, `1 - (1 - p_one)^k`, which needs a
    variance that is independent across progeny: VPM. PMV also carries the shared posterior
    marker-effect uncertainty, which every progeny of the cross has in common and which cannot
    be exponentiated away, so the per-progeny spread -- and with it every probability -- was
    inflated. It now passes `_vpm`, the same column the backend's own `p_beat_all_checks` uses.
  - *Silent independence.* When it could not build a covariance it fell back to `diag()` --
    exact independence -- with no warning and nothing in the result. Within-family trait
    correlations of -0.91 to +0.96 were measured in the audit's data, so that is a different
    answer, not a mild approximation. It now uses the **exact** recombination-aware within-family
    cross-trait covariance the run already computed and which was sitting unused on the same
    table (`wf_var_*` / `wf_cov_*`). If that is genuinely unavailable it falls back to a
    population genetic correlation and says so in the result (`covariance_model`,
    `covariance_note`); if even that is unavailable the statistic is **not computed**.
    Independence is never assumed silently, and never assumed at all.
  - *Inverted directions.* `increase` and `maximize` were the only tokens treated as an increase
    trait; `max`, `higher`, `high`, `positive` and `+` -- all accepted by the backend -- were
    silently treated as DECREASE, swapping the bound and answering the opposite question. The
    backend's own direction normaliser is now used, so the vocabularies cannot drift apart, and
    an unknown token errors instead of defaulting to "minimize".
  The result now also records which variance columns and which covariance model produced the
  number, and that it is conditional on the point-estimated marker effects.
* **A trait-direction file with an `economic_weight` or `desired_change` column is refused
  before the run, with an explanation.** Those columns make `multi_trait_method = "auto"`
  promote itself to the `economic_index` / `desired_gain` selection-index methods, which need
  phenotypic (P) and genetic (G) covariance matrices this app does not collect -- so the run
  died part-way through with an error naming two arguments the UI does not expose. The on-screen
  help actively invited that configuration ("Economic weights / Desired gains if your direction
  file carries economic weights"); it no longer does, and now says to leave those columns out.
  The columns are **not** stripped and the method is **not** silently forced: a breeder who put
  those numbers there deliberately is told why they are not being honoured.

# nextgenCrossWorkbench 0.28.0

Requires backend nextgenCrossDesign >= 0.25.0 (`inst/BACKEND_VERSION`, enforced at run time).

* **Robust posterior allocation actually produces a plan now -- at any quantile the slider can
  set.** Turning on "Robust posterior allocation" asked the backend for a pessimistic quantile
  of gain that the posterior had never cached: the draws only ever kept the two 95%
  credible-interval tails (0.025 / 0.975), and the allocator correctly refuses to fabricate a
  quantile it does not have. **No value of the 0.05-0.50 Robustness quantile slider -- including
  its 0.25 default -- could yield a robust plan**, and the run reported the refusal quietly
  enough that a breeder saw an ordinary plan and was told nothing. The run now sends the
  breeder's quantile into the prediction itself, so the posterior caches that exact empirical
  tail from the same draws and the allocation is served exactly, with no normal approximation.
  The reported credible interval is untouched: the quantile and the interval are separate
  controls, so a 25% robustness setting no longer implies (and never silently produces) a 50%
  "95%" interval.
* **The robust plan now takes the conservative tail on the correct side.** The ranked value is
  not normalised to higher-is-better, so for a minimize trait (disease, lodging) scored on mean
  or usefulness the *lower* tail is the optimistic one. The app sent no direction at all, so the
  allocator would have taken the lower tail unconditionally -- ranking crosses by their BEST case
  and labelling the result robust. The orientation is now read back from the prediction's own
  posterior metadata, which is the orientation of the *ranked value*, not of the trait:
  pure-variance metrics (`pmv`, `vpm`, parent distance) stay "maximize" even for a minimize
  trait, because more within-family variance is more opportunity whichever way the trait points.
  The orientation used is written into the run JSON (`robust_plan$direction`).
* **Numbers change for minimize traits: `prob_top_tier` and `<trait>_post_topn`.** The backend's
  posterior top-N probability counted the N *largest* ranked values regardless of direction, so
  for a minimize trait scored on mean or usefulness it reported the fraction of draws in which a
  cross was among the WORST N and presented that as stability. Fixed in nextgenCrossDesign
  0.25.0 and surfaced here: a re-run of an existing minimize-trait project will show different
  (correct) top-tier probabilities and may reshuffle the cross-priority tiers that depend on
  them. Maximize traits and pure-variance-scored traits are unaffected.
* Changing the Robustness quantile now invalidates the *predict* stage of the staged run, not
  just *rank* -- it steers what the posterior caches, so the posterior has to be recomputed.
* `config.template.yml` no longer pins `required_backend_version: "0.7.0"`. A literal there
  overrides the floor bundled with the release, which silently lowered both the "Version OK"
  chip and the hard check-lines version gate for anyone whose config was seeded from the
  template. The entry ships commented out, so a fresh config inherits `inst/BACKEND_VERSION`.
  `tools/update-backend.R` reads that same file instead of the template.

# nextgenCrossWorkbench 0.27.0

* **Check lines are references, not filters.** Requires backend nextgenCrossDesign >= 0.24.0
  (enforced at run time; a version below that reports a mismatch instead of running). Check
  genotypes are uploaded in their own file (Data > Check lines) and are never crossed, never
  mated, and never scored themselves -- a check contributes exactly one benchmark value per
  trait. **A check never excludes a cross.** Crosses on the worse side of a check's line are
  still ranked, still shown, and still selectable; the point of this release is to stop implying
  otherwise. The exclude-violators toggle and the per-trait check-basis control are gone: nothing
  is dropped by a check, and the reference always follows the run's own mean source (GEBV or
  phenotype) so it sits on the same scale as the axis it is drawn on.
* **New report figure: per-trait check reference panels.** Each checked trait gets its own facet
  (mid-parent mean vs. diversity, with that trait's check as a dashed line on its own scale) in
  Results > Modelling graphics and in the downloadable run report (HTML and PDF). This is
  deliberately a NEW panel, not a line added to the existing "Selected vs all candidates"
  scatter or the gain-diversity frontier: both of those plot an aggregate multi-trait score, and
  a check's value has no honest place on an aggregate axis. A run with no check configured shows
  no panel and no empty placeholder.
* Marker opacity on the check panels now carries P(beat check), so a below-check cross with a
  superior tail is visible instead of looking like a discard.
* Multi-trait runs additionally get a `p_beat_all_checks` column when more than one trait is
  checked -- the Monte Carlo-approximated probability that a progeny beats every check at once
  (the per-trait `p_beat_check` columns remain exact/closed-form). The run report's Diagnostics
  section explains its cost and its approximation caveat.
* Known limitation: the check line can, internally, be drawn on either axis (mean on y or on x).
  Only the horizontal case (mean on y) has a consuming view today -- every check line a breeder
  sees is horizontal, on these per-trait panels. The vertical orientation is implemented and
  tested but unused; it is not part of this release's user-facing surface.
* Fixed: the "Which column is the check ID?" picker in Data > Check lines is now honoured by
  the run itself. Picking any column other than the first left the check genotypes keyed by
  the wrong column and the run then failed with the backend's "trait_checks names check
  line(s) absent from check_geno" -- an error pointing nowhere near the cause. Picking the
  first column, or leaving the guess alone, behaved correctly and is unchanged.
* Fixed: when checks are configured but the run carries no per-trait mean column for any
  checked trait, the modelling-graphics panel now explains why no reference panel could be
  drawn instead of leaving an empty slot (and no longer errors while trying to say so).
* Fixed: **the Mate-relatedness control now genuinely supersedes the raw `lambda_mating`.**
  Choosing any per-cross relatedness behaviour while a non-zero `lambda_mating` sat in the
  advanced OCS card sent both to the backend, which refuses them together ("Set per-cross
  relatedness via EITHER mate_relatedness OR the raw lambda_mating ... not both") -- so the
  unified control built to prevent exactly that stacking was leaking it. `lambda_mating` is now
  omitted whenever a Mate-relatedness behaviour is selected, and the advanced card replaces the
  raw input with a note saying so rather than showing a number that is not in effect.
* Fixed: **"Economic weights" and "Desired gains" are no longer offered as multi-trait
  methods.** Both were guaranteed hard errors: the backend needs per-trait economic weights or
  desired changes *and* explicit phenotypic (P) and genetic (G) covariance matrices, and it
  refuses to substitute candidate-score covariance for them -- while the app has no way to
  collect P and G. The help hint claiming those values were read from your trait-direction file
  was false and is gone. (A registry-declared "Threshold" method, which the backend rejects
  outright, is dropped for the same reason.) Supplying real P and G is a separate feature; use
  Automatic or Relative weights meanwhile.
* Fixed: **a budget cap with no cost column is refused before the run, not after it.** Typing a
  budget without uploading a per-cross cost table and picking its Cost column failed inside the
  backend with "a finite budget requires cost_col". The run now stops at the gate with a message
  naming your budget and telling you which two things to supply (or to clear the cap). The cost
  and logistic emphasis sliders are unaffected -- they are simply ignored without a cost table.
* **Polyploid dominance is now an explicit experimental opt-in.** Ticking "Model dominance
  (heterosis)" used to fail every polyploid run outright, because the backend treats
  additive+dominance fitting as research-only and refuses it unless told otherwise. The checkbox
  stays; ticking it reveals an acknowledgement you must also tick, carrying the backend's reason
  in plain terms -- the additive and dominance variance components currently share a single
  ridge penalty, so how much of the genetic variance is called additive versus dominance is not
  reliable. Suitable for exploring heterosis, not for selection decisions. Requesting dominance
  without the acknowledgement refuses the run and says why; it is never silently downgraded to
  additive-only scoring.
* `run_combination_tests()`'s sweep no longer generates two configurations the backend
  refuses and the app cannot build: `uc_variance_source = "parent_distance"` (genomic
  distance is not a trait variance -- `parent_distance` remains a valid *cross-scoring
  metric*, and is still swept as one) and `lambda_mating` set together with
  `lambda_progeny_inbreeding` (two knobs on one axis, whose effects add). Each relatedness
  lambda is now explored on its own. Sweep results are otherwise unchanged in kind, though
  the random draw composition shifts.

# nextgenCrossWorkbench 0.26.0

* **Opt-in guided workbench view.** Set `options(ngcd.wizard = TRUE)` before
  `run_workbench()` to turn the existing tabs into a guided, one-screen-at-a-time
  flow: a clickable progress stepper (Data → Objective → Scoring → Filters →
  Allocation → Outputs → Run → Results, driving both the top tabs and the
  Configure sub-tabs), Back/Next navigation, a live one-line run summary
  (workflow, data source, traits, crosses, run status), and a minimalist look
  with the raw navbar hidden. No inputs are moved or renamed and every parameter
  keeps working, so the classic UI is byte-for-byte unchanged when the option is
  off. The navbar-hiding has an escape hatch:
  `options(ngcd.wizard.hidenav = FALSE)` keeps the tabs visible. New modules:
  `R/wizard.R`, `R/ui_guided.R`.

* **New modelling graphics in Results.** A "Modelling graphics" panel renders
  five interactive plotly views from the run's candidate/selected crosses:
  predicted cross-score distribution (per trait), per-trait score ridgeline,
  score x confidence coloured by risk bin, score vs diversity (kinship) with
  the selected plan highlighted over all candidates, and per-trait
  cross-validation (marker-effect) reliability. The panel presents them one
  figure at a time in a guided sequence -- each with a short, report-style
  explanation and Previous/Next navigation plus clickable step dots -- rather
  than on a single board. Empty-safe before a run. New module: `R/ui_charts.R`.

# nextgenCrossWorkbench 0.25.1

* **Docs: the user vignette now covers staged polyploid and disomic-subgenome
  design.** The disomic-subgenome section explains that the design runs as the
  same stepped, compute-once pipeline as the Standard workflow (Quality control →
  Fit effects & score → Allocate & rank; single-trait, so no selection-index
  step), with per-step gating, caching, inline figures, and byte-identical
  results to a one-shot run — plus which setting change re-runs which step. A
  parallel note was added to the Polyploid (autotetraploid) section.

# nextgenCrossWorkbench 0.25.0

* **Disomic-subgenome design now runs the stepped pipeline (staging Phase 2).**
  The true-allopolyploid workflow joins the diploid and autotetraploid paths as a
  stepped, compute-once run: **Quality control → Fit effects & score → Allocate &
  rank**, each stage with its own Run button, status badge, one-line summary and
  inline figure — instead of the previous single one-shot Run card. Per-subgenome
  QC, per-subgenome ridge effects + recombination-aware scoring, and the native
  allocation are decomposed into ctx stages that persist across the shared run
  directory, so upstream work is never recomputed. Changing the subgenome map
  column or the DH/RIL progeny target correctly invalidates only the affected
  stages onward. The one-shot and staged paths share the same stage functions, so
  they are byte-identical by construction (verified end-to-end through the runner
  by `test-subgenome-staging-parity.R`). This completes the staged-pipeline effort
  across all three ploidy families. Requires nextgenCrossDesign >= 0.18.0.

# nextgenCrossWorkbench 0.24.0

* **Design-system pass — a calmer, more professional look.** The UI moves from a
  university-brand theme to a precise "scientific software" system while keeping
  NDSU green as the identity: a neutral grotesque (Inter) for all text, neutral
  greys carrying the interface with green as the single restrained accent, and
  yellow reserved for warnings/attention only (the Run button is now solid green).
  The **form controls users touch most are now styled** — dropdowns, checkboxes,
  numeric fields, file inputs and selectize menus get consistent heights, softer
  borders and a branded focus ring (they were raw Bootstrap before). Adds an
  elevation/shadow scale and subtle motion (cards, buttons, disclosures, nav),
  a consistent 8pt spacing rhythm, keyboard focus states throughout, and lighter,
  less shouty guidance/figure panels. No functional change; CSS + theme only.

# nextgenCrossWorkbench 0.23.0

* **Professional per-file guided data import.** The Data screen is rebuilt around
  one card per file: uploading a file reveals ITS OWN preview + column mapping
  (populated from that file) + inline cross-file validation, instead of a wall of
  upload boxes plus a separate lumped column-mapping block. A status strip
  (`✓ Genotype  ✓ Phenotype  ○ Map …`) shows progress at a glance; the genotype
  card flags duplicate IDs; the phenotype card lists its trait columns and shows
  how many of its IDs match the genotype. Cards adapt to the workflow (polyploid =
  genotype + phenotype only; disomic-subgenome adds the subgenome column to the
  Marker-map card) and are fully revisitable.
* **Traits come from the phenotype file.** The traits you select on the Selection
  objective screen are now the uploaded phenotype's own columns (via
  `ngcd_trait_columns`); the trait-direction file only annotates increase/decrease
  for those traits — it no longer defines the trait set.

# nextgenCrossWorkbench 0.22.0

* **Polyploid designs run as the stepped pipeline too (autotetraploid).** The
  autotetraploid workflow now runs as the same stage-by-stage cards as the
  standard workflow — **Quality control → Fit effects & score → Allocate & rank**
  (single-trait, so no selection-index step) — each with its own run button,
  status, summary, and figure, and the same compute-once guarantee (a later step
  never re-runs a completed one). Backed by the backend's new staged polyploid
  runner (`ng_poly_run_stage`), which is byte-identical to the one-shot
  `ng_polyploid_design_crosses`. Disomic-subgenome keeps its single-shot Run card
  for now (Phase 2).
* **Fix:** the stepped Run view now unlocks **Allocate** after *Fit effects &
  score* in single-trait mode (previously it waited on the hidden selection-index
  step, which single-trait runs skip).
* Raised `required_backend_version` to **0.18.0** (ships `ng_poly_run_stage`).

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
