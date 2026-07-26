# Navigation IA redesign: concept-based workflow — design (v2)

**Date:** 2026-07-26
**Status:** Approved for planning (brainstorm + breeder/QG-lens review complete)
**Scope:** Frontend `NextGenCrossDesign` (`nextgenCrossWorkbench`) — `R/app.R` UI, `R/helpers.R`
(`ngcd_guide`), user-facing help text. **UI reorganization only** — no server logic, no input-ID
changes, no behaviour change.

> **v2 note:** a breeder/quantitative-geneticist adoption review found the v1 "move panel bodies
> verbatim + rename" was insufficient — it relabels friction instead of removing it, because
> first-class QG constructs are scattered (relatedness across 2 tabs, effect-uncertainty across 3,
> a "Fine-tuning" junk drawer). v2 **redistributes controls by concept** so each control sits next
> to the decision it belongs to.

---

## 1. Goal

Turn a flat ~10-tab navbar into a **4-stage workflow** (`Data → Configure → Run → Results`) whose
`Configure` sections **group controls by the quantitative-genetics decision they serve**, with
breeder-intuitive names and an expert-friendly (default-collapsed) guide. Target user: a
QG-heavy breeder who must get from data to a mating plan without hunting for scattered controls.

## 2. Target structure

Top nav (Setup stays **dev-only**, `if (isTRUE(dev))`, unchanged):
`Data ▾  │  Configure ▾  │  Run  │  Results ▾`

### 2.1 Data ▾ (internal `navset_tab`)
`Genotype · Phenotype · Marker map · Trait direction · Data quality`
(the old top-level **QC** panel body moves in here as **Data quality**.)

### 2.2 Configure ▾ (internal `navset_tab`, pipeline order — 5 sections; Fine-tuning dissolved)

Each control keeps its **exact input ID**; cards move as self-contained `bslib::card` /
`accordion_panel` blocks. Redistribution map (source → destination):

**① Selection objective** (renamed from *Objective* / "Breeding goal") — restructured around the
breeder's question "what am I selecting for?", NOT a bag of mode+method+threshold. Three parts:
- **Mode toggle — THREE breeder-natural choices** (new UI radio `objective_mode`; the app derives
  the backend `prediction_mode` from it in `build_params`):
  1. **"Single trait"** — pick ONE trait (a single-select); **no index, no weighting apparatus
     shown**. → `prediction_mode = "trait_by_trait"`, `traits_to_use = [that trait]`,
     `multi_trait_method = "auto"`. **This is FULL single-trait genomic prediction**, so it keeps
     usefulness/UC, within-family variance, the Portfolio & risk view, and the trait-check veto (all
     of which need `trait_by_trait`). **It is NOT routed to `index_as_trait`** (that's a separate
     mode-3 choice for a pre-computed index column), and it must **not be confused with multi-trait**
     (mode 2, which combines several traits with weights). Single-trait is the common case for a
     breeder deciding crossing blocks on one trait and must not force them through index/weighting
     machinery.
  2. **"Multiple traits (build a selection index)"** — the `traits_to_use` multi-select + the
     "Traits & their importance" weighting card below. → `prediction_mode = "trait_by_trait"`.
  3. **"Use my selection-index column"** — `index_col` + `index_direction`. →
     `prediction_mode = "index_as_trait"`.
  Existing `conditionalPanel`s keyed on `input.prediction_mode` are re-pointed to `input.objective_mode`
  (or a derived flag); the backend param names/values are unchanged.
- **Card "Traits & their importance"** (shown for mode 2 only) — `traits_to_use` + the weighting
  method (`multi_trait_method`) **relabeled in breeder terms** (auto→"Automatic",
  weighted→"Relative weights", economic_index→"Economic weights", desired_gain→"Desired gains") +
  the weight/target inputs. **Input ids + values unchanged**; only display labels.
- **Card "Minimum levels (culling thresholds)"** — `threshold_policy` (soft/strict) +
  `threshold_penalty_weight` + `threshold_penalty_autoscale` (+ per-trait floor inputs), framed as
  independent culling levels, distinct from index weighting.
- **Disambiguate "index":** the word currently means two opposite things here — the *mode*
  ("index-as-trait" = you already have an index) vs a *weighting method* ("economic_index" = build
  one). Reword so "index" is never overloaded (mode → "my selection-index column"; method →
  "Economic weights").
- **MOVE OUT:** the joint-P(superior-progeny) card (`multitrait_joint_prob` / `multitrait_targets`,
  old Objective ~app.R:194-201) → **② Prediction & scoring** (it is a cross-scoring metric, not an
  objective).

**② Prediction & scoring** — everything about predicting/valuing a cross, in **3 titled cards**:
- *Effect & variance model:* `grm_method`, `recomb_model`, `method_varPMV`, `min_effect_reliability`
  (from old *Scoring*) **+ the Posterior-prediction accordion** (`run_posterior_prediction`,
  `posterior_method`, `n_iter`, `burn_in`, `use_parallel`, `n_threads`) **moved from Advanced**
  (app.R ~464-472) — so all effect-uncertainty lives in one place.
- *Cross-value metric:* `trait_value_metric`, `uc_variance_source`, `selection_prop` (from *Scoring*)
  **+ the joint-superiority card moved from Selection objective**.
- *Breeding system:* `progeny` (DH/RIL), `assume_inbred`.

**③ Cross vetoes** (renamed from *Trait checks*) — per-cross genetic vetoes/steering together:
- the trait check-line pickers + `check_basis` + `exclude_threshold_violators` + `include_trait_gebv`
  (existing Trait checks), **+ the "Marker steering & lethal guarding" accordion moved from
  Advanced** (`marker_target_spec`, `lambda_marker`, `lethal_spec`, `drop_lethal_carrier_crosses`,
  app.R ~439-443).
- Keep the existing "not applicable in index mode" annotation for the trait-check veto.

**④ Mate allocation (OCS)** (renamed from *Allocation*) — who mates whom + all relatedness:
- keep the existing Allocation cards (Plan size & parent use incl. cross-number sweep; Family sizes;
  Pareto explorer; Optimizer & method incl. `use_ocs`/evolution internals; **Gain-diversity dial**;
  robust posterior allocation).
- **MOVE IN from Advanced** the "Breeder mating constraints" accordion — critically the **per-cross
  `mate_relatedness` (+ weight)** control (app.R ~432-439), placed **adjacent to the Gain-diversity
  dial** as the per-cross half of one "Relatedness & diversity" grouping; plus `min_crosses_per_parent`
  and the **heterotic-pool** inputs (`parent_group`, `group_quota`, `group_disallow`).
- **MOVE IN from Advanced** the "Cost & logistics" accordion (`budget`, `lambda_cost`,
  `lambda_logistic`, `cost_col`) — it is an allocation constraint.

**⑤ Export options** (renamed from *Output*) — output file / workbook / figure settings (unchanged
body).

**Fine-tuning (old *Advanced*) is DISSOLVED** — its 4 accordion panels redistribute: Breeder mating
constraints + Cost & logistics → ④; Marker steering & lethal → ③; Posterior prediction → ②. The
remaining solver internals (`evol_*`, `local_iter`, `ocs_iter`) already live in ④'s "Optimizer &
method" card — no orphan bucket remains.

### 2.3 Run · Results
Unchanged. Results' 15 internal sub-tabs are OUT OF SCOPE this round.

### 2.4 Rename summary
Objective→**Selection objective**; Scoring→**Prediction & scoring**; QC→**Data quality** (under Data);
Trait checks→**Cross vetoes**; Allocation→**Mate allocation (OCS)**; Advanced→**dissolved**;
Output→**Export options**. Top-level "Configure" — acceptable; the 6→5 sub-labels carry the meaning.

## 3. Mechanism (bslib)
- `Configure` = a top-level `nav_panel` wrapping a `bslib::navset_tab` of the 5 section `nav_panel`s
  (mirrors the existing Data/Results pattern).
- Redistribution = cut/paste of self-contained `bslib::card(...)` / `bslib::accordion_panel(...)`
  blocks between section bodies. **Input IDs and `conditionalPanel` conditions inside each block are
  unchanged** and move with the block.
- Internal navsets need no `id` (none referenced today).

## 4. Guided tour (`ngcd_guide`) — expert-friendly + reconciled
- **Default collapsed:** call `ngcd_guide(..., open = FALSE)`; add one session-level "Show guided
  tour" toggle that flips `open`. Experts aren't walked through 10 boxes; first-run users can opt in.
- **Reconcile the counter with the 4-stage shell:** replace the flat "Step N of 10" with a
  stage-scoped label — e.g. "Configure — 3 of 5" (or drop the numeric counter). The counter must not
  claim "10" when the bar shows 4 stages.
- **Retitle** each guide to the new section name; **add the missing Cross-vetoes guide box**; fix the
  "Next →" `next_hint` chaining to the new pipeline order:
  Data → Data quality → Selection objective → Prediction & scoring → Cross vetoes → Mate allocation →
  Export options → Run → Results.

## 5. Stale-reference sweep (user-facing text)
Grep ALL user-facing strings (not just the 3 lines v1 flagged) and repoint/reword:
- old tab names: "Allocation"→"Mate allocation", "Advanced"/"Objective"/"Scoring"/"Output"/"QC" →
  new names (e.g. Report/diagnostics help at app.R ~1647/1684/1746).
- **"above" spatial references** to the Mate-relatedness control (app.R ~383) — now valid IF
  mate_relatedness is placed above the dial in ④; otherwise reword.
- **"Setup" references** (app.R ~340, ~480: lpSolve / requirements) — Setup is dev-only/invisible in
  production; replace with an inline requirement note, not a pointer to a hidden tab.
- polyploid dosage-cleaning callout (app.R ~261-265) — cross-link now that QC lives under Data.

## 6. Invariants (UI-only, with ONE deliberate exception)
- **One intentional exception (the objective mode):** the 3-way `objective_mode` selector is a NEW UI
  input replacing the direct `prediction_mode` radio; `build_params` derives `prediction_mode` +
  `traits_to_use` + `multi_trait_method` from it (§2.2 ①). This is additive UX — it produces the
  SAME backend values the app produces today (`trait_by_trait` for single/multiple, `index_as_trait`
  for the index column), just reachable more intuitively. It is the only place the redesign adds
  build_params logic; everything else is pure card relocation.
- **No OTHER input ID changes** — apart from `prediction_mode`→`objective_mode` above, pre/post grep
  of every `input$<id>` / `inputId=` must be identical (no other input lost/renamed in the card moves).
- `nav_select("nav","Results")` (app.R:1476) unchanged (Results stays top-level).
- `build_params` and all server reactives unchanged; `conditionalPanel`s (keyed on
  `input.workflow`/`data_source`/`prediction_mode`/`diversity_mode`/`mate_relatedness`/
  `run_posterior_prediction`) move intact with their blocks.
- Setup stays dev-gated. No behaviour/logic change.

## 7. Testing / verification
- `parse("R/app.R")` OK; `workbench_app()` returns a `shiny.appobj`.
- **Input-ID conservation:** the set of input IDs before and after is IDENTICAL (no input lost in
  the card moves) — this is the key regression guard for a redistribution.
- Boot HTTP 200.
- Manual/visual: top bar = Data · Configure · Run · Results; Configure shows the 5 sections in
  order; each moved card renders under its new section; `mate_relatedness` sits by the dial; no
  user-facing string names a removed/hidden tab (grep old names → none outside NEWS/history); guide
  boxes default collapsed with a working toggle.

## 8. Rollout
Frontend **0.17.0** (notable UX change; no behaviour change). NEWS entry. Push to `main`
(unprotected, no PR). **No `Co-Authored-By` trailer on any commit** (user is sole contributor).
Image rebuild (to make it live) is the separate parked step.

## 9. Out of scope
- Grouping the 15 Results sub-tabs (follow-on).
- Redesigning `ngcd_guide` into a full stepper/wizard (only collapsed-default + counter reconcile).
- Backend `R/diagnostics.R` screen-name suggestions (backend repo — note if any names a renamed tab).
- Any behaviour/logic/algorithm change.
