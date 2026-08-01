# Frontend Capability Integration — Design

**Goal:** Close the gaps between the `nextgenCrossDesign` backend's full capability
surface and what the `nextgenCrossWorkbench` frontend exposes, so the workbench
surfaces every user-relevant backend capability — without regressing the existing,
already-polished UI.

**Prime directive (non-negotiable):** The frontend already looks good. Every change is
**additive and on-pattern**. Match the existing design language exactly: `bslib`
`nav_panel`/`card`/`accordion` structure, the `ngcd_callout()` helper, the plotly
theme used by `res_frontier_plotly`/`repfig_*`, the settings/reset registry
(`ngcd_settings_registry()`), and the plain-language help-text tone. No existing tab,
control, default, or result view may change behavior or look worse. New tabs are allowed
where a capability warrants its own home (user-approved).

## Scope: 9 features in three groups

### Group A — quick, self-contained
- **A1. Diminishing-returns chart.** The auto cross-number sweep already runs and returns
  `result$cross_number_sweep$curve` (K vs gain, `recommended_k`, `criterion`), but the app
  only prints "K = N chosen" and points to a chart that does not exist. Render the curve as
  a plotly chart on Results (K on x, gain on y, elbow K marked; overlay achieved Ne /
  coancestry when `ne_target`/`coancestry_budget` are the criterion). Fix the dangling
  "see the chart" text. **Frontend only** — data contract already present.
- **A2. Wire the `crop` control.** `crop` is collected but flagged "ignored by the runner".
  Route it through `ng_crop_aware_policy_select()` (via the crop-genome catalog
  `ng_crop_genome_select()`) and surface the recommended method family / routing / validation
  scope as an informational callout on the Data or Scoring tab. **Frontend runner + UI**,
  calling existing backend functions.

### Group B — structural backbone
- **B1. Registry-driven UI.** Today every dropdown's choices are hardcoded in `R/app.R`, so
  each backend rename/addition needs a manual frontend edit (the root cause of the
  rename-lockstep pain). Make the frontend read the backend capability registry at startup
  and derive its enumerable choice lists **and** capability/panel visibility from it.
  **Two-repo change:**
  - *Backend* (`R/25_backend_capability_registry.R`): add a new `controls` section (schema
    version bump) enumerating each user-facing config parameter with: `id`, `label`,
    `group` (owning panel/tab), `type` (enum/number/bool/text), `choices` (value+label for
    enums), `default`, `min`/`max`/`step` (numbers), `depends_on` (gating), `capability`
    (the method-family/workflow it belongs to). This is additive and backward-compatible;
    existing sections and `ng_run_cross_prediction` are untouched.
  - *Frontend*: at startup the runner already writes `backend_capabilities.json`; add a
    lightweight probe so the app reads the registry (via a one-shot Rscript that calls
    `ng_backend_capability_registry()` and returns JSON, mirroring the existing `config.R`
    formals probe) and builds the enumerable `selectInput`/`radioButtons` choices and panel
    gating from `controls`. Hardcoded vectors become a fallback used only if the registry is
    unavailable (older backend) — so the app never breaks against an old engine.

### Group C — new user-facing capabilities (each: runner workflow handler + UI + result display)
The runner dispatches on `raw$workflow` (`inst/app/tools/run_cross_prediction_json.R:200`).
Each C-feature adds a `workflow` value + handler that calls the existing backend function,
plus a UI surface and a results view. AlphaSimR/benchmark workflows (`head_to_head`,
`alphasimr_benchmark`, `poly4x_benchmark`) are **developer tools and stay out of the UI**.

- **C1. Family-size allocation + variance calibration.** `ng_allocate_family_sizes()`,
  `ng_fit_family_variance_calibrators()`, `ng_apply_family_variance_calibrators()`,
  `ng_validate_metric_calibration()`, `ng_family_metric_summary()`. Needs user trial-data
  upload (realized families / historical crosses) with clearly-labelled expected schemas
  (reuse the vignette's "replace with your own" column contract). New **Calibration** tab.
- **C2. Multi-trait posterior prediction.** Extend the posterior panel to multi-trait via
  `ng_posterior_multitrait_cross_predict()` / `ng_p_superior_progeny_multitrait()` (today it
  is single-trait only). Surfaced in the existing Advanced/Posterior area + a results table.
- **C3. Pareto frontier + breeder-selection allocators.** `ng_pareto_mate_allocation()`,
  `ng_optimize_breeder_selection_plan()`, `ng_breeder_selection_objective()`. Add as
  alternative allocation views/outputs (the frontier plot infra `res_frontier_plotly` already
  exists and can be reused).
- **C4. Multi-trait validation grids.** `ng_run_multitrait_validation_grid()` /
  `ng_run_multitrait_crop_validation_grid()`. Evidence for method choice. New **Validation**
  tab (registry marks it a `validation` workflow, medium runtime → run as an explicit job).
- **C5. Allopolyploid / subgenome path.** `ng_polyploid_subgenome_as_dosage_list()`,
  `ng_polyploid_subgenome_grm()`, `ng_polyploid_subgenome_score_crosses()`. Add a
  subgenome sub-mode to the polyploid workflow (experimental — label as such, matching the
  registry `status=experimental`).
- **C6. Native baseline comparison as a job.** `ng_popvar_style_scores()`,
  `ng_add_simplemating_scores()`, `ng_add_gms_vpm_scores()`, `ng_alphamate_style_select()`.
  Let the user compare their plan against native baselines (today only a status badge). New
  **External Tools**-adjacent view or a section under Results.

## Delivery — phased, reviewable PRs (frontend repo unless noted)

Each PR: additive change → app still boots and all existing tabs unchanged → frontend
version bump → open PR → merge (frontend `main` is unprotected, no CI, so verify locally:
`R CMD build`/load, app boot, and a manual on-pattern/visual review). Backend changes for
B1 go to the backend repo as their own release.

1. **PR1 — A1 + A2** (chart + crop): smallest, highest visible value. FE 0.10.1 → 0.10.2.
2. **PR2 (backend) — B1 registry `controls` section**: backend registry extension, its own
   backend patch/minor release; then **PR3 (frontend)** consumes it. FE → 0.11.0 (new
   capability-discovery behavior).
3. **PR4..PR9 — C1..C6**, one capability per PR, each its own FE minor bump.

Ordering rationale: A first (quick wins, no dependencies); B second (backbone that later C
tabs can also use for their option lists); C last (each independent).

## Verification per change (the "never worse" gate)
- App boots headless (`nextgenCrossWorkbench::run_workbench(...)` returns HTTP 200, as in
  the container smoke test) with the change present.
- Every pre-existing tab/control/default renders and behaves identically (diff the settings
  registry; no removed inputIds).
- New UI matches existing components (nav_panel/card/accordion, ngcd_callout, plotly theme,
  help text). No inline restyling of existing elements.
- New runner workflows fail gracefully (clear message) when inputs/optional packages are
  missing — never a raw error, mirroring the `NG_ALPHAMATE_EXE` guard pattern.
- Backend B1 change: additive registry section only; `R CMD check` Status OK; existing
  registry consumers unaffected.

## Non-goals
- Developer/benchmark workflows (head-to-head, AlphaSimR, poly4x benchmarks) stay out of the
  user UI.
- No restyling or restructuring of existing tabs beyond what a feature strictly requires.
- No backend API/behavior change other than the additive registry `controls` section.
