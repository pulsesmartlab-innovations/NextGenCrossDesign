# Multi-user run isolation — design

Status: proposed
Date: 2026-07-18
Component: nextgenCrossWorkbench (frontend)
Files in scope: `R/run_backend.R`, `R/config.R` (+ tests)

## Context

The workbench runs the backend out-of-process: it writes `config.json` and input
CSVs into a per-run directory, invokes the backend's headless Rscript runner via
`system2(..., timeout = ...)`, and reads back `result.json` plus figure
artifacts. This model gives process isolation, a hard timeout, cancel, and
reproducible run artifacts, and it lets one codebase serve both an individual on
a laptop (`run_workbench()`) and a server hosting many users.

Serving many users on a server exposes concrete problems in the current
implementation. None is inherent to the subprocess architecture; all are
isolation bugs in how the app's writable areas are located, named, and retained.
Line numbers below are from the code as of this spec.

1. **Run directories are not unique.** `ngcd_new_run_dir()`
   (`R/run_backend.R:5`) names a run `format(Sys.time(), "%Y%m%d-%H%M%S")` + a
   label slug — second resolution, no session id, no PID, no random component
   (`dir <- file.path(cfg$runs_dir, paste0(stamp, "_", slug))`). Two runs started
   in the same second with the same label resolve to the same directory and
   clobber each other's `config.json` / `result.json`.
2. **All writable areas share one base that may be read-only.** Three writable
   locations are rooted at `cfg$work_dir` (which defaults to `getwd()`):
   `runs/` (`R/config.R:58`), `_report/` (`R/run_workbench.R:42`,
   `R/app.R:1255`), and `presets/` (`R/app.R:1129`). In a container `getwd()` can
   be a read-only application directory, so **all three** fail to write, not just
   runs. The fix must relocate the writable *base*, not one directory.
3. **The config env-override does not reach `runs_dir` as written.**
   `ngcd_load_config()` applies `NGCD_*` overrides in a loop over `names(cfg)`
   (`R/config.R:42-51`), but `cfg$runs_dir` is assigned unconditionally *after*
   that loop (`R/config.R:58`). So a naive `NGCD_RUNS_DIR` would be set by the
   loop and then silently overwritten. Any new override must be either seeded
   into `defaults` (before the loop) or applied so the post-loop assignment
   respects an already-set value.
4. **The inline report path is process-global and fixed-named.**
   `addResourcePath("ngcd_report", report_dir)` registers one process-wide
   URL→dir mapping (`R/run_workbench.R:44`) and the preview is written to a fixed
   `report.html` / `report.pdf` (`R/app.R:1260-1261`). Within one container,
   multiple browser tabs (multiple Shiny sessions in one R process) share it, so
   the inline preview is last-writer-wins across tabs. This is bounded by two
   facts: execution is synchronous (blocking `system2`, `R/run_backend.R:75`;
   single-threaded Shiny) so there is no mid-write corruption, and the **download
   handlers write to per-download temp files** (`R/app.R:1364-1369`), so the
   artifact-egress path is already isolated. Only the shared *preview* is
   affected, and only across one user's own tabs.
5. **Run artifacts accumulate unbounded.** A long-lived session that performs
   many runs keeps every run directory (inputs, results, figures), so container
   disk grows without limit.

## Decisions (from brainstorming)

- **Isolation model: container-per-user (ShinyProxy).** Each user gets their own
  container — own filesystem, own R process. Cross-user collisions are therefore
  impossible at the infrastructure layer, and a blocking `system2()` run is fine
  because a container serves one user. The code only needs to be correct for one
  user in one container.
- **Artifact lifetime: within-session only.** Runs live in the container
  filesystem and are gone when the session/container ends; the user downloads
  their result and report during the session. No mounted volumes, no
  cross-session persistence.

These two decisions remove async execution, app-level concurrency caps, per-user
volumes, cross-session persistence, run-history UI, and any cross-user
partitioning from scope.

## Design

### 1. One writable data base for all mutable areas

Introduce a single writable base that `runs/`, `_report/`, and `presets/` all
derive from, so the read-only-container problem is fixed once at the root rather
than three times. In `ngcd_load_config()`:

```r
# Seed the base into defaults BEFORE the NGCD_* override loop, so NGCD_DATA_DIR
# and a config.yml `data_dir:` both take effect (fixes the ordering bug where
# runs_dir was assigned after the loop and clobbered any override).
defaults$data_dir <- file.path(tempdir(), "ngcd")
# ... existing defaults merge + NGCD_* override loop run here ...

cfg$data_dir <- cfg$data_dir %||% file.path(tempdir(), "ngcd")   # belt-and-braces
cfg$runs_dir <- file.path(cfg$data_dir, "runs")
# _report and presets likewise derive from cfg$data_dir (see below).
```

- **Default** `file.path(tempdir(), "ngcd")` — always writable, unique per R
  process, removed automatically when the session/container ends (exactly the
  within-session lifetime chosen). Works unchanged on a laptop and in a
  read-only-app container.
- **Override** `NGCD_DATA_DIR` (env) / `data_dir` (config.yml). A local user who
  wants persistent artifacts points it at a real folder; a container can point it
  at any writable ephemeral path or a mounted volume if persistence is ever added.
- **`_report/` and `presets/`** move from `cfg$work_dir` to `cfg$data_dir`.
  `cfg$work_dir` stays as the config/read location (where `config.yml` lives),
  which may legitimately be read-only; only `data_dir` must be writable.
- **Startup writability check**: verify `cfg$data_dir` is creatable/writable and
  surface a clear message through the existing backend-status messages
  (`ngcd_check_backend` populates `out$messages`, rendered as status chips in
  `app.R`) rather than failing at first run.

### 2. Unique run directories

Generate the run directory with `tempfile()`, which is guaranteed unique within
an R session (internal counter + PID), keeping the human-readable timestamp +
slug as the prefix for legibility:

```r
ngcd_new_run_dir <- function(cfg, label = NULL) {
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  slug  <- if (!is.null(label) && nzchar(label))
             gsub("[^A-Za-z0-9_-]+", "_", label) else "run"   # existing slugify
  dir   <- tempfile(pattern = paste0(stamp, "_", slug, "_"), tmpdir = cfg$runs_dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}
```

Yields e.g. `20260718-141530_trait_by_trait_a3f9c1/`. `tempfile()` is preferred
over a random suffix because it does not depend on the RNG state — the app sets
seeds for reproducibility, which could make a naive `sample()`-based suffix
repeat.

### 3. Inline report preview under one container

The download handlers already write to per-download temp files, so the important
artifact-egress path is isolated and needs no change. The only shared surface is
the inline preview (`addResourcePath("ngcd_report", …)` + fixed `report.html`),
and only across one user's own tabs in a single container.

For v1 this is left as last-writer-wins: it is bounded by synchronous execution
(no mid-write corruption), it affects only the transient preview (not downloads,
not saved artifacts), and container-per-user means the two tabs belong to the
same person. Moving `_report/` under `cfg$data_dir` (§1) keeps it writable; the
preview semantics are unchanged. A per-session report path (session-scoped
`addResourcePath` + session-unique filename) is noted as a *possible* later
refinement but is out of scope here — it adds per-session resource-path lifecycle
management for a preview-only, same-user edge case.

### 4. Within-session retention cap

After a new run directory is created, prune older run directories beyond a cap so
container disk stays bounded during a long session:

- Default: keep the most recent **20** run directories (by mtime); delete older
  ones. Never touch the run being created.
- Override: `NGCD_KEEP_RUNS` env var. `0` disables pruning (a local user who
  wants to keep everything).
- Pruning runs inside `ngcd_new_run_dir()` (or a small helper it calls), so it
  happens exactly when a new run starts and nowhere else.

## Configuration surface (new)

| Key | Default | Purpose |
| --- | --- | --- |
| `NGCD_DATA_DIR` (env) / `data_dir` (config) | `tempdir()/ngcd` | Writable base for `runs/`, `_report/`, `presets/` |
| `NGCD_KEEP_RUNS` (env) | `20` | Max run directories kept during a session; `0` = unlimited |

Both follow the existing `NGCD_*` override convention in `ngcd_load_config()`.
`data_dir` is seeded into `defaults` **before** the override loop so the override
actually takes effect (see problem 3 above); `runs_dir`, the report dir, and the
presets dir are then derived from it.

## UX note

Because artifacts are within-session (they vanish when the container ends), the
user must download results/report before leaving. The download affordances
already exist (`Download PDF` / `Download HTML`, and the report is also viewable
inline). Add a short, unobtrusive line near the report/output area — e.g.
"Downloads are saved to your computer; run artifacts on the server are cleared
when you close the app" — so a hosted user is not surprised by disappearing runs.
This is copy only; no new controls.

## Out of scope (YAGNI)

Async/`promises` execution, app-level concurrency limits, per-user mounted
volumes, cross-session persistence, a run-history browser, and cross-user
directory partitioning. Under container-per-user + within-session none is
required. Adding persistence later is additive — mount a volume, add a retention
policy, add a history view — and does not rework the run pipeline defined here.

## Testing

New targeted tests (testthat), plus the full suite for regression:

- Two runs created in the same wall-clock second resolve to distinct directories.
- `data_dir` resolves to a writable location; `NGCD_DATA_DIR` override is honored
  and is **not** clobbered by the post-loop assignment (the regression that
  problem 3 describes); `runs/`, `_report/`, `presets/` all sit under it.
- An unwritable `data_dir` produces a clear startup message (not a first-run
  crash).
- With `NGCD_KEEP_RUNS = 3`, creating a 4th run leaves exactly the 3 most recent
  directories and removes the oldest; the in-flight run is never pruned.
- `NGCD_KEEP_RUNS = 0` keeps all directories.
- Full frontend testthat suite (373 incl. backend-integration) passes unchanged.

## Compatibility

The individual/local experience is unchanged in feel: `run_workbench()` uses the
`tempdir()`-backed data dir and a keep-20 cap by default, needs no new
dependencies, and adds no new controls. A local user who wants persistent
artifacts sets `NGCD_DATA_DIR` to a real folder and `NGCD_KEEP_RUNS=0`. No changes
to the backend, the JSON contract, or the runner. Moving `_report/` and
`presets/` from `work_dir` to `data_dir` is transparent to users (same relative
layout under a writable base) and is the only behavioural change beyond the new
defaults.
