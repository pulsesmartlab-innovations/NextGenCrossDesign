# Navigation IA redesign — implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: use superpowers:subagent-driven-development. Steps
> use checkbox (`- [ ]`) syntax.

**Goal:** Reorganize the `nextgenCrossWorkbench` Shiny UI from a flat ~10-tab navbar into a 4-stage
workflow (`Data → Configure → Run → Results`) whose Configure sections group controls by the QG
decision they serve, per the approved spec.

**Authoritative design:** `docs/superpowers/specs/2026-07-26-navigation-ia-redesign-design.md` (v3).
Each task IMPLEMENTS a part of that spec; when in doubt, the spec's §2.2 redistribution map governs.

**Architecture:** Sequential edits to `R/app.R` (UI) + `R/helpers.R` (`ngcd_guide`). Cards move as
self-contained `bslib::card` / `accordion_panel` blocks. **UI-only except ONE deliberate exception**
(the 3-way `objective_mode` + its `build_params` derivation, Task 5).

**Tech stack:** R, Shiny, bslib; testthat (`tests/testthat`).

## Global Constraints

- **THE regression guard — input-ID conservation.** The static input-ID set must not change except
  the documented `prediction_mode`→`objective_mode` swap (Task 5). Baseline (151 IDs) is at
  `.superpowers/sdd/baseline-input-ids.txt`. **After every task**, re-extract and diff:
  ```
  grep -oE '(numericInput|selectInput|radioButtons|checkboxInput|textAreaInput|textInput|sliderInput|fileInput|actionButton|dateInput|selectizeInput)\("[a-zA-Z0-9_]+"' R/app.R \
    | sed -E 's/.*\("([a-zA-Z0-9_]+)"/\1/' | sort -u > /tmp/ids-now.txt
  diff .superpowers/sdd/baseline-input-ids.txt /tmp/ids-now.txt
  ```
  Tasks 1–4, 6–7: diff MUST be empty. Task 5: diff MUST be exactly `< prediction_mode` / `> objective_mode`
  (plus any documented single-trait picker id).
- **No server logic changes except Task 5** (`build_params` deriving `prediction_mode`/`traits_to_use`/
  `multi_trait_method` from `objective_mode`, and re-pointing `input.prediction_mode` conditionalPanels
  to `input.objective_mode`). Everything else moves UI blocks only.
- **`conditionalPanel` conditions move intact with their block.** `nav_select("nav","Results")` and
  the dev-only Setup gate are untouched.
- After **every** task: `Rscript -e 'invisible(parse("R/app.R")); cat("parses\n")'` OK, and
  `Rscript -e 'library(nextgenCrossWorkbench); stopifnot(inherits(workbench_app(),"shiny.appobj")); cat("app OK\n")'`
  (install the package first: testthat/`workbench_app` resolve from the INSTALLED package —
  `R CMD INSTALL . --no-multiarch --no-docs` before checks that load it).
- **PLAIN commit messages — NO `Co-Authored-By`/Claude/AI trailer** (user is sole contributor).
- Backend `nextgenCrossDesign` 0.14.0 must be installed for the `workbench_app()`/boot checks.

## File Structure
- **Modify** `R/app.R` — the whole reorg (Tasks 1–5, 7).
- **Modify** `R/helpers.R` — `ngcd_guide` (Task 6).
- **Modify** `R/app.R` server — `build_params` + conditionalPanel repoint (Task 5).
- **Modify** `DESCRIPTION`, `NEWS.md` (Task 8).

---

### Task 1: 4-stage shell — Data(+QC) · Configure(6 whole-body sections) · Run · Results

Establish the new top-level structure with **whole-panel moves only** (no card redistribution yet;
Advanced kept as a temporary 6th Configure section so nothing is lost). Spec §2.1, §2.4, §3.

- [ ] **Step 1: Verify baseline** — run the input-ID extraction; confirm it equals
  `.superpowers/sdd/baseline-input-ids.txt` (151). This is the reference for every later diff.
- [ ] **Step 2:** In the `page_navbar` (R/app.R:48), add the QC panel BODY as a `nav_panel("Data quality", …)`
  at the end of Data's existing internal `navset_tab` (R/app.R:168-172); remove the old top-level
  `nav_panel("QC", …)`.
- [ ] **Step 3:** Create `bslib::nav_panel("Configure", bslib::navset_tab( … ))` containing, in
  pipeline order, six `nav_panel`s whose bodies are the (renamed) existing panel bodies moved verbatim:
  "Selection objective" (old Objective), "Prediction & scoring" (old Scoring),
  "Cross filters & genetic constraints" (old Trait checks), "Mate allocation" (old Allocation),
  "Export options" (old Output), and — TEMPORARY — "Advanced" (old Advanced body, unchanged; dissolved
  in Task 2). Remove those six old top-level panels.
- [ ] **Step 4:** Leave Run, Results, and the dev-only Setup panel exactly as-is.
- [ ] **Step 5: Verify** — parse OK; input-ID diff EMPTY; `workbench_app()` builds; top bar is
  Data · Configure · Run · Results.
- [ ] **Step 6: Commit** — `git commit -am "refactor(ui): 4-stage navbar shell; QC under Data; Configure holds sections"`

---

### Task 2: Dissolve "Advanced" — redistribute its 6 accordion panels

Spec §2.2 (dissolve map). Move each `accordion_panel` block from the temp Advanced section to its
conceptual home, then delete the empty Advanced section.

- [ ] **Step 1:** Move to **② Prediction & scoring**: the "Posterior prediction" panel
  (`run_posterior_prediction`, `posterior_method`, `n_iter`, `burn_in`, `use_parallel`, `n_threads`)
  AND the "Training-set augmentation" panel (`f_train_geno`, `f_train_pheno`,
  `training_genotype_id_col`, `training_phenotype_id_col`).
- [ ] **Step 2:** Move to **③ Cross filters & genetic constraints**: the "Marker steering & lethal
  guarding" panel (`marker_target_spec`, `lambda_marker`, `lethal_spec`, `drop_lethal_carrier_crosses`).
- [ ] **Step 3:** Move to **④ Mate allocation**: "Mate relatedness" (`mate_relatedness`,
  `mate_relatedness_weight`), "Breeder mating constraints" (`min_crosses_per_parent`, `parent_group`,
  `group_quota`, `group_disallow`), and "Cost & logistics" (`budget`, `lambda_cost`, `lambda_logistic`,
  `cost_col_ui`). (Exact card placement within ④ is Task 3.)
- [ ] **Step 4:** Delete the now-empty temp Advanced `nav_panel`.
- [ ] **Step 5: Verify** — parse OK; **input-ID diff EMPTY** (this is the key check — all 6 panels'
  inputs must survive the move; a dropped panel shows here); `workbench_app()` builds; Configure now
  has 5 sections, no Advanced.
- [ ] **Step 6: Commit** — `git commit -am "refactor(ui): dissolve Advanced into Prediction/Cross-filters/Mate-allocation"`

---

### Task 3: ④ Mate allocation — internal 3-panel accordion

Spec §2.2④. Wrap ④'s cards in a `bslib::accordion` of 3 panels so it isn't an ~11-card wall.

- [ ] **Step 1:** Group ④'s cards into three `bslib::accordion_panel`s:
  - "Plan size & constraints": Plan size & parent use (n_crosses/sweep, max/min parents,
    max_pair_kinship), Family sizes, heterotic pools (parent_group/group_quota/group_disallow),
    min_crosses_per_parent, Cost & logistics.
  - "Gain–diversity & relatedness": Gain-diversity dial + `mate_relatedness`(+weight) adjacent, +
    Pareto explorer.
  - "Engine & advanced": Optimizer & method (optimizer/allocation_method/use_ocs/evolution internals),
    OCS penalties, AlphaMate-style controls, robust posterior allocation — with a `help-hint`
    cross-link "uses the posterior engine set in Prediction & scoring → Effect & variance model".
- [ ] **Step 2: Verify** — parse OK; input-ID diff EMPTY; `workbench_app()` builds; ④ shows a 3-panel
  accordion.
- [ ] **Step 3: Commit** — `git commit -am "feat(ui): sub-group Mate allocation into Plan/Diversity/Engine panels"`

---

### Task 4: ② Prediction & scoring — 3 cards + move joint-superiority from ①

Spec §2.2②. Organize ② into 3 titled cards and pull the joint-P card out of Selection objective.

- [ ] **Step 1:** Structure ② as three `bslib::card`s: "Effect & variance model" (grm_method,
  recomb_model, method_varPMV, min_effect_reliability, + the Posterior-prediction & Training-set
  panels moved in Task 2); "Cross-value metric" (trait_value_metric, uc_variance_source,
  selection_prop); "Breeding system" (progeny, assume_inbred).
- [ ] **Step 2:** MOVE the "Multi-trait joint P(superior progeny)" card (`multitrait_joint_prob`,
  `multitrait_targets`) from ① Selection objective INTO ②'s "Cross-value metric" card.
- [ ] **Step 3: Verify** — parse OK; input-ID diff EMPTY; `workbench_app()` builds; ② shows the 3
  cards; joint-P is under ②, not ①.
- [ ] **Step 4: Commit** — `git commit -am "feat(ui): Prediction & scoring 3-card layout; move joint-superiority in"`

---

### Task 5: ① Selection objective — 3-way objective_mode (the one logic change)

Spec §2.2①, §6. This is the only task that touches `build_params`/server. TDD the pure derivation.

- [ ] **Step 1: Write a failing test** for a pure helper `ngcd_objective_backend(objective_mode, single_trait, traits, index_col)`
  → `list(prediction_mode, traits_to_use, multi_trait_method_applies)` (append to
  `tests/testthat/test-server-logic.R` or a new `test-objective-mode.R`):
  ```r
  test_that("objective_mode maps to backend params", {
    s <- ng("ngcd_objective_backend")
    expect_equal(s("single", "yield", NULL, NULL)$prediction_mode, "trait_by_trait")
    expect_equal(s("single", "yield", NULL, NULL)$traits_to_use, "yield")
    expect_equal(s("multi", NULL, c("yield","oil"), NULL)$prediction_mode, "trait_by_trait")
    expect_equal(s("index", NULL, NULL, "idx")$prediction_mode, "index_as_trait")
  })
  ```
- [ ] **Step 2:** Implement `ngcd_objective_backend()` in `R/helpers.R` (pure) per the mapping in
  spec §2.2① (single → trait_by_trait + one trait + multi_trait_method="auto"; multi → trait_by_trait;
  index → index_as_trait). Run the test → passes.
- [ ] **Step 3: UI:** replace the `prediction_mode` radio with an `objective_mode` radio (3 choices:
  "Single trait" / "Multiple traits (build a selection index)" / "Use my selection-index column");
  add a single-trait selectInput (single-select) shown for single mode; keep `traits_to_use_ui`
  (multi) for multi mode; keep `index_col`/`index_direction` for index mode. Rename the weighting
  method labels (auto→Automatic, weighted→Relative weights, economic_index→Economic weights,
  desired_gain→Desired gains). Rename the threshold card to "Threshold handling". Add the **persistent
  `help-hint`** that economic/desired-gain values come from the trait-direction file. Disambiguate
  "index" wording.
- [ ] **Step 4:** Re-point `conditionalPanel("input.prediction_mode == …")` conditions to
  `input.objective_mode` values; in `build_params`, derive `prediction_mode`/`traits_to_use`/
  `multi_trait_method` via `ngcd_objective_backend(...)`.
- [ ] **Step 5: Verify** — parse OK; input-ID diff is EXACTLY `< prediction_mode` / `> objective_mode`
  (plus the single-trait picker id if added); `workbench_app()` builds; a single-trait selection
  hides the weighting apparatus; testthat green.
- [ ] **Step 6: Commit** — `git commit -am "feat(ui): 3-way Selection objective (single/multiple/index) + build_params derivation"`

---

### Task 6: Guided tour — collapsed default, counter reconciled, retitle, persistent hints

Spec §4. Edit `R/helpers.R` `ngcd_guide` + the `ngcd_guide(...)` calls.

- [ ] **Step 1:** Default the guide **collapsed** (`open = FALSE`); add one session "Show guided tour"
  toggle that flips it.
- [ ] **Step 2:** Replace the flat "Step N of 10" with a stage-scoped label (e.g. "Configure — 3 of 5")
  or drop the numeric counter; it must not claim 10 when the bar shows 4 stages.
- [ ] **Step 3:** Retitle each guide to the new section name; add the missing guide box for section ③;
  fix `next_hint` chaining to: Data → Data quality → Selection objective → Prediction & scoring →
  Cross filters & genetic constraints → Mate allocation → Export options → Run → Results.
- [ ] **Step 4:** Confirm the persistent economic/desired-gain hint (Task 5) is independent of the
  now-collapsed guide.
- [ ] **Step 5: Verify** — parse OK; input-ID diff EMPTY; `workbench_app()` builds; guide boxes default
  collapsed with a working toggle.
- [ ] **Step 6: Commit** — `git commit -am "feat(ui): guided tour collapsed-by-default, counter reconciled to 4 stages"`

---

### Task 7: Stale-reference sweep (user-facing text)

Spec §5. Grep ALL user-facing strings and repoint/reword.

- [ ] **Step 1:** Replace old tab names in help/report/diagnostics text: "Allocation"→"Mate allocation",
  "Advanced"/"Objective"/"Scoring"/"Output"/"QC" → new names (incl. app.R ~1647/1684/1746).
- [ ] **Step 2:** Reword the trait-check "index-mode" annotation (app.R ~245-246) to
  "Active for Single-trait and Multiple-trait modes; ignored when you use your own selection-index column."
- [ ] **Step 3:** Replace "Setup" pointers (app.R ~340, ~480: lpSolve/requirements) with inline
  requirement notes (Setup is dev-only/invisible in prod); reword any "above" references to
  mate_relatedness; cross-link the polyploid dosage-cleaning callout to Data quality.
- [ ] **Step 4:** Grep to confirm no user-facing string names a removed/renamed tab or the dead
  `prediction_mode`/"trait_by_trait"/"index_as_trait" wording (outside NEWS/history).
- [ ] **Step 5: Verify** — parse OK; input-ID diff EMPTY; `workbench_app()` builds.
- [ ] **Step 6: Commit** — `git commit -am "docs(ui): sweep stale tab/mode/Setup references in help text"`

---

### Task 8: Final verify + version bump + push

- [ ] **Step 1: Full input-ID conservation** — final diff = exactly the Task-5 delta
  (`prediction_mode`→`objective_mode` + single-trait picker id); nothing else lost.
- [ ] **Step 2:** `R CMD INSTALL . --no-multiarch --no-docs`; `Rscript -e 'testthat::test_dir("tests/testthat")'`
  — assert only the 2 KNOWN pre-existing failures (backend-version string; lambda_progeny_inbreeding),
  nothing new; boot HTTP 200.
- [ ] **Step 3:** Manual smoke: top bar Data · Configure · Run · Results; Configure = 5 sections in
  order; ④ 3-panel accordion; single-trait mode hides weighting; guide collapsed.
- [ ] **Step 4:** `DESCRIPTION` `0.16.0`→`0.17.0`; NEWS 0.17.0 entry.
- [ ] **Step 5: Commit + push** — `git commit -am "chore: bump to 0.17.0 (navigation IA redesign)"` then
  `git push origin main` (unprotected, no PR).

---

## Deferred / out of scope
- Grouping the 15 Results sub-tabs (follow-on).
- Plain-language relabel of the `uc_variance_source`/`pmv`/`vpm` dropdown VALUES (backlog; like the
  `le→parent_distance` pass).
- Redesigning `ngcd_guide` into a full wizard (only collapsed-default + counter reconcile here).
- Image rebuild (separate parked step).

## Self-Review
- **Spec coverage:** §2.1 (Task 1), §2.2 dissolve+②③④ (Tasks 2–4), §2.2① objective_mode (Task 5),
  §4 guide (Task 6), §5 sweep (Task 7), §6 invariants (input-ID guard every task), §7 verify (Task 8).
- **Placeholder scan:** none — each task names concrete blocks/inputs and gives the exact verify commands.
- **Type consistency:** `ngcd_objective_backend(objective_mode, single_trait, traits, index_col) ->
  list(prediction_mode, traits_to_use, ...)` used identically in the Task 5 test and build_params.
  Input-ID conservation is the cross-task invariant enforced after every task.
