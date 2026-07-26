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
> **v3 note (2nd QG review):** confirmed the v2 fixes are good; folded in 8 more: ④ Mate allocation
> gains internal 3-panel sub-structure (it was an ~11-card wall); the "Minimum levels" card renamed
> "Threshold handling" (the app has NO per-trait floor inputs — don't promise them); the orphaned
> **Training-set augmentation** panel (Advanced has **6** panels, not 4) assigned to ②; ③ renamed
> **Cross filters & genetic constraints** (marker steering is a reward, not a veto); "(OCS)" dropped
> from ④ (it runs non-OCS allocators too); persistent inline hints so the collapsed guide can't hide
> load-bearing instructions (economic/desired-gain weights come from the direction file); + specific
> stale-string sweeps (trait-check annotation, prediction-mode wording).

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
  the weight inputs. **Input ids + values unchanged**; only display labels.
  - **Persistent hint (do NOT rely on the now-collapsed guide, §4):** "Relative weights" reveals a
    `trait_weights` box, but **"Economic weights" and "Desired gains" reveal no input** — their
    values are read from the **trait-direction file**. Add an always-visible `help-hint` in this card
    for those two cases ("economic values / desired gains are read from your trait-direction file"),
    or a breeder picks them, sees no field, and thinks the app is broken.
- **Card "Threshold handling"** (renamed — do NOT call it "per-trait floors"): `threshold_policy`
  (soft/strict) + `threshold_penalty_weight` + `threshold_penalty_autoscale`. **There are no
  per-trait floor inputs in the app** — the actual per-trait target/threshold *values* come from the
  trait-direction file (and the joint-superiority `multitrait_targets`, which §2.2② owns). So this
  card is only the *policy* that acts on those levels; label it honestly and add a hint that the
  levels themselves come from the direction file. (Drop the earlier "(+ per-trait floor inputs)"
  claim.)
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
  (app.R ~464-472) — so all effect-uncertainty lives in one place **+ the "Training-set
  augmentation" accordion** (`f_train_geno`, `f_train_pheno`, `training_genotype_id_col`,
  `training_phenotype_id_col`) **moved from Advanced** — it sharpens marker-effect estimation, so it
  belongs with the effect model (this panel was the ORPHAN the v2 dissolve map missed).
- *Cross-value metric:* `trait_value_metric`, `uc_variance_source`, `selection_prop` (from *Scoring*)
  **+ the joint-superiority card moved from Selection objective**.
- *Breeding system:* `progeny` (DH/RIL), `assume_inbred`.

**③ Cross filters & genetic constraints** (renamed from *Trait checks* — NOT "Cross vetoes": the
section holds both *exclusions* AND a *positive* directional reward, so "vetoes" alone is wrong):
- **Exclusions/vetoes:** the trait check-line pickers + `check_basis` + `exclude_threshold_violators`
  + `include_trait_gebv` (existing Trait checks) + **lethal-recessive guarding** (`lethal_spec`,
  `drop_lethal_carrier_crosses`) moved from Advanced.
- **Directional constraint (NOT a veto):** **marker/allele steering** (`marker_target_spec`,
  `lambda_marker`) moved from Advanced — this is a *reward* that nudges an allele frequency toward a
  target via an optimizer weight, not an exclusion. Label it distinctly within the section (or, if
  preferred at build time, it may instead live in ④'s "Engine & advanced" since it is a lambda — but
  the spec's default is here, clearly separated from the exclusions).
- Keep the trait-check "index-mode" annotation but **reword to the new mode vocabulary** (§5): the
  veto is active for **Single-trait and Multiple-trait** modes, ignored for **"Use my
  selection-index column"** — not the dead phrase "Trait-by-trait prediction mode (Scoring tab)".

**④ Mate allocation** (renamed from *Allocation*; **NOT "(OCS)"** — the section also runs
non-OCS allocators via `allocation_method` ∈ {ocs, alphamate_style, alphamate_executable}). This
becomes the largest section (~11 cards after the moves), so it MUST have **internal sub-structure**
— a `bslib::accordion` with **3 panels** so a breeder who just wants "20 crosses, cap parent use"
isn't facing a wall of expert cards:
- **"Plan size & constraints"** — `n_crosses` / cross-number sweep, `max_crosses_per_parent`,
  `min_unique_parents`, `max_pair_kinship`, Family sizes; **+ moved-in** heterotic pools
  (`parent_group`, `group_quota`, `group_disallow`) + `min_crosses_per_parent`; **+ moved-in** Cost
  & logistics (`budget`, `lambda_cost`, `lambda_logistic`, `cost_col`).
- **"Gain–diversity & relatedness"** — the **Gain-diversity dial** + the **per-cross
  `mate_relatedness` (+ weight)** moved in from Advanced (app.R ~432-439), placed adjacent so the
  two relatedness levers sit together; + the Pareto-frontier explorer.
- **"Engine & advanced"** — `optimizer`, `allocation_method`, `use_ocs`, evolution internals,
  OCS penalties, AlphaMate-style controls, **robust posterior allocation** (add a cross-link note:
  "uses the posterior engine set in Prediction & scoring → Effect & variance model", since that
  control now lives two tabs away, §2.2②).

**⑤ Export options** (renamed from *Output*) — output file / workbook / figure settings (unchanged
body).

**Fine-tuning (old *Advanced*) is DISSOLVED** — its **6** accordion panels redistribute (the app has
six, not four): Mate relatedness + Breeder mating constraints (pools) + Cost & logistics → ④;
Marker steering & lethal guarding → ③; Posterior prediction + **Training-set augmentation** → ②. The
remaining solver internals (`evol_*`, `local_iter`, `ocs_iter`) already live in ④'s Engine card — no
orphan bucket remains, and **every one of the six panels is assigned** (verify against §7
input-ID conservation).

### 2.3 Run · Results
Unchanged. Results' 15 internal sub-tabs are OUT OF SCOPE this round.

### 2.4 Rename summary
Objective→**Selection objective**; Scoring→**Prediction & scoring**; QC→**Data quality** (under Data);
Trait checks→**Cross filters & genetic constraints**; Allocation→**Mate allocation** (no "(OCS)");
Advanced→**dissolved**; Output→**Export options**. Top-level "Configure" — acceptable; the sub-labels
carry the meaning.

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
- **Retitle** each guide to the new section name; **add the missing guide box for section ③**; fix the
  "Next →" `next_hint` chaining to the new pipeline order:
  Data → Data quality → Selection objective → Prediction & scoring → Cross filters & genetic
  constraints → Mate allocation → Export options → Run → Results.
- **Do NOT let the collapsed guide hide load-bearing explanations.** Any control whose ONLY rationale
  currently lives in a guide box must get a **persistent inline `help-hint`** — notably the
  economic-weights/desired-gains "values come from your trait-direction file" note (§2.2①). The guide
  is for orientation, not for the only copy of a required instruction.

## 5. Stale-reference sweep (user-facing text)
Grep ALL user-facing strings (not just the 3 lines v1 flagged) and repoint/reword:
- old tab names: "Allocation"→"Mate allocation", "Advanced"/"Objective"/"Scoring"/"Output"/"QC" →
  new names (e.g. Report/diagnostics help at app.R ~1647/1684/1746).
- **"above" spatial references** to the Mate-relatedness control (app.R ~383) — now valid IF
  mate_relatedness is placed above the dial in ④; otherwise reword.
- **"Setup" references** (app.R ~340, ~480: lpSolve / requirements) — Setup is dev-only/invisible in
  production; replace with an inline requirement note, not a pointer to a hidden tab.
- polyploid dosage-cleaning callout (app.R ~261-265) — cross-link now that QC lives under Data.
- **trait-check "index-mode" annotation** (app.R ~245-246): "Applies only in Trait-by-trait
  prediction mode (Scoring tab)" — BOTH names die (mode → `objective_mode` labels; "Scoring" →
  "Prediction & scoring"). Reword to: "Active for Single-trait and Multiple-trait modes; ignored when
  you use your own selection-index column."
- **any "prediction mode" / "trait_by_trait" / "index_as_trait" wording** in help text → the new
  `objective_mode` labels (Single trait / Multiple traits / Use my selection-index column).

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
