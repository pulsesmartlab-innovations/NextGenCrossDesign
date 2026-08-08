# Data-import redesign — Implementation Plan

> REQUIRED SUB-SKILL: superpowers:executing-plans (inline). Steps use `- [ ]`.
> Branch off `origin/main` AFTER the polyploid-staging PR (#29) merges.

**Goal:** Turn the Data screen into a professional, guided, per-file import: each file's upload
reveals its own preview + column mapping (from that file) + inline validation, progressive and
revisitable; and the traits to analyze come from the phenotype file (not the direction file).

**Architecture:** Pure frontend (`R/app.R` Data `nav_panel` + server column/preview/validation
renders; `R/helpers.R` small pure helpers). No backend/runner change. The staged Run pipeline is
untouched.

## Global Constraints

- **Input-ID conservation:** the existing input ids (`f_geno`/`f_pheno`/`f_map`/`f_dir`,
  `genotype_id_col`/`phenotype_id_col`/`map_*_col`/`direction_*_col`, `pos_unit`, `bp_per_cm`,
  `ploidy`, `poly_*`, `subgenome_col`, `traits_to_use`, `single_trait`) KEEP their ids so config
  assembly, settings save/restore, and staleness keying are unchanged. New ids only where genuinely
  new (e.g. a per-step status is `uiOutput`, not an input). Snapshot ids before/after; diff is
  additions only.
- **No behavior change to config assembly / the backend contract:** `build_params()` /
  `build_poly_params()` still read the same input ids. Only WHERE the controls live and HOW they
  surface changes, plus the trait-source of `full_trait_set()`.
- **Guard existing tests:** `test-config`, `test-server-logic`, `test-settings`, `test-objective-mode`,
  `test-combinations` must stay green (they lock the config surface).

## Current state (what we're replacing)

- Data `nav_panel` (`app.R:94-190`): four `fileInput`s in one block + a single **Column mapping**
  card = `uiOutput("colmap_ui")` that renders EVERY column dropdown for ALL files at once
  (`app.R:944-990`), plus `uiOutput("data_checks")` and editable-table sub-tabs.
- `full_trait_set()` (`app.R:987`) derives traits from the **direction** file (fallback: phenotype
  minus ID). The Objective screen reads `full_trait_set()`.

---

### Task 1: Trait source = phenotype file (foundational, testable)

**Files:** `R/app.R` (`full_trait_set`, objective wiring), `R/helpers.R` (pure helper), tests.

- [ ] **Step 1 — failing test** `tests/testthat/test-trait-source.R`: a pure
  `ngcd_trait_columns(phenotype, id_col)` returns the phenotype's non-ID, numeric-ish columns;
  and given a direction table it does NOT drive the set. Assert order + exclusion of the ID column.
- [ ] **Step 2 — implement** `ngcd_trait_columns(pheno_df, id_col)` in `R/helpers.R` (mirror the
  numeric-column detection already inlined in `run_polyploid_design`/`full_trait_set`). Rewrite
  `full_trait_set()` to `ngcd_trait_columns(rv$data$phenotype, <detected pheno id>)`. The direction
  file is now consulted only to ANNOTATE directions for the chosen traits (default increase).
- [ ] **Step 3 — objective wiring:** the Objective screen's `traits_to_use` / `single_trait`
  choices come from `full_trait_set()` (now phenotype-based) — verify no other reader assumed the
  direction source (grep `full_trait_set`), adjust the direction-matching in `build_params()` so
  `trait_direction` is built for the chosen traits (missing → "increase").
- [ ] **Step 4 — run** `test-trait-source.R` + `test-objective-mode` + `test-config` → green.
- [ ] **Step 5 — commit.**

---

### Task 2: Per-file import-card scaffolding + status strip

**Files:** `R/app.R` (Data `nav_panel` body), `R/helpers.R` (card + chip helpers),
`inst/app/www/ndsu.css` (card/strip styles).

- [ ] **Step 1:** pure helpers in `R/helpers.R`:
  - `ngcd_import_status(df, kind)` → `list(state = "empty"|"ok"|"warn", chip = "…")` (rows×cols, or a
    warning string). Unit-test the states.
  - `ngcd_step_strip(states)` → a compact `①✓ ②✓ ③○ ④○` tag from a named states list.
- [ ] **Step 2:** replace the Data `nav_panel` upload block + `colmap_ui` card with a
  `uiOutput("import_steps")` (server-rendered so cards adapt to workflow + upload state) preceded by
  `uiOutput("import_strip")`. Keep the editable-table sub-tabs and `data_source` (demo vs upload)
  toggle as-is. Move `ploidy`/`poly_*` into the polyploid step-2 card region.
- [ ] **Step 3:** server `output$import_strip` from the four `ngcd_import_status(...)` states; smoke.
- [ ] **Step 4 — commit.**

---

### Task 3: Per-file preview + progressive column mapping

**Files:** `R/app.R` (`output$import_steps`), reuse `id_col_choices`/`ngcd_guess_col`.

- [ ] **Step 1:** `output$import_steps <- renderUI(...)` builds one card per file (workflow-aware).
  Each card: header + status chip; if the file is loaded → a small preview (`DT` first 5 rows, or a
  compact HTML table) + THAT file's mapping controls (populated from its columns); else → a compact
  drop zone. Reuse the exact `selectInput` ids from `colmap_ui`, now placed inside their file's card:
  - Genotype card: `genotype_id_col` (+ dosage-range note for poly).
  - Phenotype card: `phenotype_id_col` + `traits_to_use`/`single_trait` surfaced here (traits from
    THIS file, Task 1) — the "trait column(s) to analyze" the user asked for.
  - Marker-map card: `map_marker_col`/`map_chr_col`/`pos_unit`/`bp_per_cm`/`map_pos_*_col`
    (+ `subgenome_col` when subgenome).
  - Direction card: `direction_*_col`, pre-filtered to the chosen traits.
- [ ] **Step 2:** delete the old `colmap_ui` render (ids now live in the cards). Confirm each id is
  declared exactly once (grep count == 1) — no duplicate-output bug.
- [ ] **Step 3:** smoke-render UI; `testServer` renders the genotype card's `genotype_id_col` from an
  uploaded file's columns.
- [ ] **Step 4 — commit.**

---

### Task 4: Inline per-card validation

**Files:** `R/app.R` (fold `data_checks` into the cards).

- [ ] **Step 1:** per-card validation `uiOutput`s: genotype → dosage-range/duplicate-ID; phenotype →
  "N/M IDs match the genotype" (overlap of pheno IDs vs geno IDs); map → marker coverage
  (markers in geno present in map); direction → traits covered. Reuse the logic in `data_checks`.
- [ ] **Step 2:** remove the standalone `data_checks` block (or keep a compact roll-up only). Keep
  the Data-quality sub-tab (QC audit) as-is.
- [ ] **Step 3 — commit.**

---

### Task 5: Revisitability, workflow adaptivity, polish, tests

- [ ] Status strip items are clickable and scroll/focus their card (anchor + small JS or `bslib`
  accordion `open`); re-uploading one file never resets others (already true — inputs are
  independent; add a test).
- [ ] Polyploid: cards = genotype + phenotype + ploidy only (no map/direction). Subgenome: map card
  gains the subgenome column. Standard: all four.
- [ ] `testServer`: upload geno then pheno → strip shows ①✓ ②✓; phenotype card lists its trait
  columns; changing a mapping doesn't clear another file. Input-ID conservation diff = additions only.
- [ ] Full suite green; `R CMD check` via CI; version bump + NEWS; PR.

## Self-review

- Covers: per-file progressive cards (T2/T3), trait-from-phenotype (T1), inline validation (T4),
  revisitable + adaptive (T5). Input-ID conservation is a global constraint with a diff gate.
  Backend/runner untouched. The one real ripple (trait source) is isolated in T1 with its own tests.
