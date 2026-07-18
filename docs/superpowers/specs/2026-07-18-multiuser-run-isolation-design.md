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

Serving many users on a server exposes three concrete problems in the current
implementation. None is inherent to the subprocess architecture; all are
isolation bugs in how runs are located and retained.

1. **Run directories are not unique.** `ngcd_new_run_dir()` (`R/run_backend.R`)
   names a run `format(Sys.time(), "%Y%m%d-%H%M%S")` + a label slug — second
   resolution, no session id, no PID, no random component. Two runs started in
   the same second with the same label resolve to the same directory and clobber
   each other's `config.json` / `result.json`.
2. **The runs location may be read-only.** `runs_dir` defaults to
   `file.path(getwd(), "runs")` (`R/config.R`). In a container `getwd()` can be a
   read-only application directory, so runs fail to write.
3. **Run artifacts accumulate unbounded.** A long-lived session that performs
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

### 1. Unique run directories

Generate the run directory with `tempfile()`, which is guaranteed unique within
an R session (internal counter + PID), while keeping the human-readable
timestamp + slug as the prefix for legibility:

```r
ngcd_new_run_dir <- function(cfg, label = NULL) {
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  slug  <- ...                                   # existing label slugify
  dir   <- tempfile(pattern = paste0(stamp, "_", slug, "_"), tmpdir = cfg$runs_dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}
```

Yields e.g. `20260718-141530_trait_by_trait_a3f9c1/`. `tempfile()` is preferred
over a random suffix because it does not depend on the RNG state — the app may
set a seed elsewhere for reproducibility, which could make a naive
`sample()`-based suffix repeat. The same `tempfile()` treatment is applied to the
`_report/` output path so a fast second run cannot overwrite the first report
before it is downloaded.

### 2. Writable, configurable runs location

Resolve `runs_dir` to a location that is always writable, defaulting to a
per-session directory under `tempdir()`:

- Default: `file.path(tempdir(), "ngcd-runs")` — always writable, unique per R
  process, and removed automatically when the session/container ends (which is
  exactly the within-session lifetime chosen above).
- Override: `NGCD_RUNS_DIR` env var / `runs_dir` config key. A local user who
  wants runs under their working folder sets it; a container can point it at any
  ephemeral writable path.
- Startup check: verify the resolved `runs_dir` is writable and surface a clear
  message (in the existing backend-status messages) if it is not, rather than
  failing at first run.

`R/config.R` currently sets `cfg$runs_dir <- file.path(dir, "runs")`; this becomes
the override-aware resolution above, keeping `dir`/`work_dir` for config and the
report base.

### 3. Within-session retention cap

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
| `NGCD_RUNS_DIR` (env) / `runs_dir` (config) | `tempdir()/ngcd-runs` | Where run directories are written |
| `NGCD_KEEP_RUNS` (env) | `20` | Max run directories kept during a session; `0` = unlimited |

Both follow the existing `NGCD_*` override convention in `ngcd_load_config()`.

## Out of scope (YAGNI)

Async/`promises` execution, app-level concurrency limits, per-user mounted
volumes, cross-session persistence, a run-history browser, and cross-user
directory partitioning. Under container-per-user + within-session none is
required. Adding persistence later is additive — mount a volume, add a retention
policy, add a history view — and does not rework the run pipeline defined here.

## Testing

New targeted tests (testthat), plus the full suite for regression:

- Two runs created in the same wall-clock second resolve to distinct directories.
- `runs_dir` resolves to a writable location; `NGCD_RUNS_DIR` override is honored;
  an unwritable location produces a clear startup message.
- With `NGCD_KEEP_RUNS = 3`, creating a 4th run leaves exactly the 3 most recent
  directories and removes the oldest; the in-flight run is never pruned.
- `NGCD_KEEP_RUNS = 0` keeps all directories.
- Full frontend testthat suite (373 incl. backend-integration) passes unchanged.

## Compatibility

The individual/local experience is unchanged in feel: `run_workbench()` uses the
`tempdir()`-backed runs location and a keep-20 cap by default, needs no new
dependencies, and adds no new UI. A local user who wants persistent runs sets
`NGCD_RUNS_DIR` to a folder and `NGCD_KEEP_RUNS=0`. No changes to the backend, the
JSON contract, or the runner.
