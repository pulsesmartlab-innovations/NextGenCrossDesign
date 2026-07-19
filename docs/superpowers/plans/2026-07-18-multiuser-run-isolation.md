# Multi-user run isolation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the workbench safe to host for many users (one container per user, artifacts live only for the session) by giving all mutable areas a single writable base, unique run directories, and a within-session retention cap.

**Architecture:** All three writable areas (`runs/`, `_report/`, `presets/`) derive from one configurable base `cfg$data_dir` (default `tempdir()/ngcd`), resolved in `ngcd_load_config()` before the paths are built. Run directories become collision-proof via `tempfile()`. A retention cap prunes old run dirs when a new run starts. No async, no volumes, no cross-user code — infrastructure (ShinyProxy container-per-user) provides cross-user isolation.

**Tech Stack:** R, Shiny, testthat (edition 3), withr (test-only). No new dependencies.

## Global Constraints

- **Deployment model:** container-per-user (ShinyProxy); artifacts are within-session only. Do NOT add async/promises, concurrency caps, mounted volumes, cross-session persistence, run-history UI, or cross-user partitioning.
- **OS-agnostic:** must work identically on Windows, macOS, Linux. Test writability with a probe file, not `file.access(mode=2)` (unreliable on Windows).
- **No new dependencies.** `withr` is test-only (already in `Suggests`).
- **Do not change** the backend, the JSON run contract, the runner, or the package id `nextgenCrossWorkbench`.
- **Config override rule:** any new `NGCD_*`-overridable key MUST be seeded into the `defaults` list in `ngcd_load_config()` (before the override loop at `R/config.R:42-51`), and any post-loop derived path MUST respect an already-set value — otherwise the override is silently clobbered.
- **Commits authored as** `Sikiru Atanda <pulsesmartlab@gmail.com>`, no AI trailer.
- **Verification gate:** the full frontend testthat suite must stay green. Run non-integration as `Rscript tests/testthat.R`; run the backend-integration tier with `NOT_CRAN=true NGCD_RUN_COMBINATIONS=1 Rscript tests/testthat.R` (needs the backend installed).

---

### Task 1: Writable data base resolved in config

Give `ngcd_load_config()` a single writable base (`cfg$data_dir`) that overrides
correctly and falls back to a writable tempdir if the configured base is not
writable. Derive `runs_dir`, `report_dir`, `presets_dir` from it.

**Files:**
- Modify: `R/config.R` (the `defaults` list ~L28-38; the block ~L53-59)
- Test: `tests/testthat/test-config.R` (append)

**Interfaces:**
- Consumes: nothing new.
- Produces: `cfg$data_dir` (character, writable dir), `cfg$runs_dir`,
  `cfg$report_dir`, `cfg$presets_dir` (all `file.path(cfg$data_dir, ...)`),
  `cfg$data_dir_warning` (character or NULL). A package-internal helper
  `ngcd_dir_writable(d)` returning a single logical.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-config.R`:

```r
test_that("data_dir defaults under tempdir and derives the mutable areas", {
  cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
  expect_true(startsWith(normalizePath(cfg$data_dir, mustWork = FALSE),
                         normalizePath(tempdir(), mustWork = FALSE)))
  expect_equal(cfg$runs_dir,    file.path(cfg$data_dir, "runs"))
  expect_equal(cfg$report_dir,  file.path(cfg$data_dir, "_report"))
  expect_equal(cfg$presets_dir, file.path(cfg$data_dir, "presets"))
  expect_true(dir.exists(cfg$runs_dir))
  expect_true(dir.exists(cfg$report_dir))
  expect_true(dir.exists(cfg$presets_dir))
  expect_null(cfg$data_dir_warning)
})

test_that("NGCD_DATA_DIR override is honored (not clobbered after the loop)", {
  base <- tempfile("ngcd-data")
  withr::with_envvar(c(NGCD_DATA_DIR = base), {
    cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
    expect_equal(normalizePath(cfg$data_dir, mustWork = FALSE),
                 normalizePath(base, mustWork = FALSE))
    expect_equal(cfg$runs_dir, file.path(cfg$data_dir, "runs"))
  })
})

test_that("an unwritable data_dir falls back to tempdir with a warning", {
  # Point data_dir at a path under an existing *file* so creation must fail.
  blocker <- tempfile("blocker"); file.create(blocker)
  bad <- file.path(blocker, "cannot", "exist")   # parent is a file, not a dir
  withr::with_envvar(c(NGCD_DATA_DIR = bad), {
    cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
    expect_false(is.null(cfg$data_dir_warning))
    expect_true(dir.exists(cfg$data_dir))          # usable fallback
    expect_true(startsWith(normalizePath(cfg$data_dir, mustWork = FALSE),
                           normalizePath(tempdir(), mustWork = FALSE)))
  })
})

test_that("ngcd_dir_writable probes with a real file", {
  d <- tempfile("wtest"); dir.create(d)
  expect_true(nextgenCrossWorkbench:::ngcd_dir_writable(d))
  expect_false(nextgenCrossWorkbench:::ngcd_dir_writable(file.path(tempfile("f"), "x")))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd <repo>; Rscript -e 'library(testthat); library(nextgenCrossWorkbench); test_file("tests/testthat/test-config.R")'`
Expected: FAIL — `ngcd_dir_writable` not found; `cfg$data_dir` / `cfg$report_dir` / `cfg$presets_dir` NULL.
(The package must be installed first: `R CMD INSTALL .` from the repo root.)

- [ ] **Step 3: Add the writability helper**

In `R/config.R`, above `ngcd_load_config`, add:

```r
# TRUE if directory `d` exists (or can be created) and a probe file can be
# written there. Probe-file check is used instead of file.access(mode = 2)
# because file.access write-mode is unreliable on Windows.
ngcd_dir_writable <- function(d) {
  if (!dir.exists(d)) {
    ok <- tryCatch({ dir.create(d, recursive = TRUE, showWarnings = FALSE); dir.exists(d) },
                   error = function(e) FALSE)
    if (!isTRUE(ok)) return(FALSE)
  }
  probe <- file.path(d, paste0(".ngcd_write_test_", Sys.getpid()))
  ok <- tryCatch(isTRUE(file.create(probe)) && file.exists(probe), error = function(e) FALSE)
  if (ok) unlink(probe)
  isTRUE(ok)
}
```

- [ ] **Step 4: Seed `data_dir` into defaults**

In `R/config.R`, in the `defaults <- list(...)` block, add `data_dir` (keeps the
override loop able to see it):

```r
  defaults <- list(
    rscript_path             = "Rscript",
    package_library          = "",
    required_backend_version = "0.4.0",
    alphamate_executable     = "",
    run_timeout_seconds      = 1800,
    max_upload_mb            = 200,
    # Writable base for all mutable areas (runs, reports, presets). Seeded here,
    # before the NGCD_* override loop, so NGCD_DATA_DIR / a config.yml data_dir:
    # actually take effect instead of being overwritten later.
    data_dir                 = file.path(tempdir(), "ngcd"),
    developer_mode           = FALSE
  )
```

- [ ] **Step 5: Resolve the base and derive the paths**

In `R/config.R`, replace the block:

```r
  cfg$work_dir      <- dir
  cfg$config_path   <- cfg_path
  cfg$runner_script <- ngcd_res("tools", "run_cross_prediction_json.R")
  cfg$demo_data_dir <- ngcd_res("data", "demo")
  cfg$www_dir       <- ngcd_res("www")
  cfg$runs_dir      <- file.path(dir, "runs")
  if (!dir.exists(cfg$runs_dir)) dir.create(cfg$runs_dir, recursive = TRUE, showWarnings = FALSE)
  cfg
```

with:

```r
  cfg$work_dir      <- dir
  cfg$config_path   <- cfg_path
  cfg$runner_script <- ngcd_res("tools", "run_cross_prediction_json.R")
  cfg$demo_data_dir <- ngcd_res("data", "demo")
  cfg$www_dir       <- ngcd_res("www")

  # Resolve the writable data base (data_dir came from defaults / config / env).
  # If it is not writable, fall back to a tempdir base and record a warning that
  # the app surfaces in its status messages rather than failing at first run.
  cfg$data_dir <- cfg$data_dir %||% file.path(tempdir(), "ngcd")
  cfg$data_dir_warning <- NULL
  if (!ngcd_dir_writable(cfg$data_dir)) {
    fallback <- file.path(tempdir(), "ngcd")
    dir.create(fallback, recursive = TRUE, showWarnings = FALSE)
    cfg$data_dir_warning <- paste0("Configured data_dir '", cfg$data_dir,
                                   "' is not writable; using '", fallback, "' instead.")
    cfg$data_dir <- fallback
  }

  cfg$runs_dir    <- file.path(cfg$data_dir, "runs")
  cfg$report_dir  <- file.path(cfg$data_dir, "_report")
  cfg$presets_dir <- file.path(cfg$data_dir, "presets")
  for (d in c(cfg$runs_dir, cfg$report_dir, cfg$presets_dir))
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  cfg
```

(`%||%` is already defined and used across the package.)

- [ ] **Step 6: Reinstall and run the tests**

Run: `cd <repo>; R CMD INSTALL . >/dev/null 2>&1; Rscript -e 'library(testthat); library(nextgenCrossWorkbench); test_file("tests/testthat/test-config.R")'`
Expected: PASS (all config tests, including the pre-existing ones).

- [ ] **Step 7: Commit**

```bash
git add R/config.R tests/testthat/test-config.R
git commit -m "feat(config): single writable data_dir base with override + fallback"
```

---

### Task 2: Point `_report` and `presets` at the derived dirs

Replace the three `file.path(cfg$work_dir, ...)` writable-area call sites with the
`cfg$report_dir` / `cfg$presets_dir` resolved in Task 1, so a read-only
`work_dir` (container) no longer breaks report preview or preset saving.

**Files:**
- Modify: `R/run_workbench.R:42-43`, `R/app.R:1129-1130`, `R/app.R:1255-1256`
- Test: `tests/testthat/test-config.R` (append)

**Interfaces:**
- Consumes: `cfg$report_dir`, `cfg$presets_dir` (from Task 1).
- Produces: nothing new.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-config.R`:

```r
test_that("report_dir and presets_dir live under data_dir, not work_dir", {
  wd <- tempfile("wb"); dir.create(wd)
  cfg <- nextgenCrossWorkbench:::ngcd_load_config(wd)
  # The mutable areas must NOT be under the (possibly read-only) work_dir.
  expect_false(startsWith(normalizePath(cfg$report_dir, mustWork = FALSE),
                          normalizePath(wd, mustWork = FALSE)))
  expect_false(startsWith(normalizePath(cfg$presets_dir, mustWork = FALSE),
                          normalizePath(wd, mustWork = FALSE)))
  expect_true(startsWith(normalizePath(cfg$report_dir, mustWork = FALSE),
                         normalizePath(cfg$data_dir, mustWork = FALSE)))
})
```

- [ ] **Step 2: Run test to verify it passes already (Task 1 satisfies it), then wire the call sites**

Run: `Rscript -e 'library(testthat); library(nextgenCrossWorkbench); test_file("tests/testthat/test-config.R")'`
Expected: PASS (this test asserts Task 1's config). It guards the call-site wiring done next from regressing the derivation.

- [ ] **Step 3: Use `cfg$report_dir` in `run_workbench.R`**

In `R/run_workbench.R`, replace:

```r
  report_dir <- file.path(cfg$work_dir, "_report")
  if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  shiny::addResourcePath("ngcd_report", report_dir)
```

with:

```r
  report_dir <- cfg$report_dir
  if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  shiny::addResourcePath("ngcd_report", report_dir)
```

- [ ] **Step 4: Use `cfg$report_dir` in `app.R` (run report writer)**

In `R/app.R`, replace:

```r
        rdir <- file.path(cfg$work_dir, "_report")
        if (!dir.exists(rdir)) dir.create(rdir, recursive = TRUE, showWarnings = FALSE)
```

with:

```r
        rdir <- cfg$report_dir
        if (!dir.exists(rdir)) dir.create(rdir, recursive = TRUE, showWarnings = FALSE)
```

- [ ] **Step 5: Use `cfg$presets_dir` in `app.R` (settings profiles)**

In `R/app.R`, replace:

```r
    presets_dir <- file.path(cfg$work_dir, "presets")
    if (!dir.exists(presets_dir)) dir.create(presets_dir, recursive = TRUE, showWarnings = FALSE)
```

with:

```r
    presets_dir <- cfg$presets_dir
    if (!dir.exists(presets_dir)) dir.create(presets_dir, recursive = TRUE, showWarnings = FALSE)
```

- [ ] **Step 6: Reinstall and run the fast suite**

Run: `cd <repo>; R CMD INSTALL . >/dev/null 2>&1; Rscript tests/testthat.R 2>&1 | tail -3`
Expected: `[ FAIL 0 | ... ]` — no failures. (6 tests SKIP without the backend; that is expected here.)

- [ ] **Step 7: Commit**

```bash
git add R/run_workbench.R R/app.R tests/testthat/test-config.R
git commit -m "feat(paths): derive _report and presets from data_dir, not work_dir"
```

---

### Task 3: Collision-proof run directories

Make `ngcd_new_run_dir()` produce a unique directory even for two runs in the
same second, using `tempfile()` (RNG-independent — the app sets seeds for
reproducibility, so a `sample()` suffix could repeat).

**Files:**
- Modify: `R/run_backend.R:5-11`
- Test: `tests/testthat/test-run-isolation.R` (create)

**Interfaces:**
- Consumes: `cfg$runs_dir` (from Task 1).
- Produces: `ngcd_new_run_dir(cfg, label = NULL)` returns a unique existing dir
  path whose basename begins `YYYYMMDD-HHMMSS_<slug>_`.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-run-isolation.R`:

```r
test_that("two run dirs in the same second are distinct and both exist", {
  cfg <- list(runs_dir = tempfile("runs")); dir.create(cfg$runs_dir)
  a <- nextgenCrossWorkbench:::ngcd_new_run_dir(cfg, label = "same_label")
  b <- nextgenCrossWorkbench:::ngcd_new_run_dir(cfg, label = "same_label")
  expect_true(dir.exists(a))
  expect_true(dir.exists(b))
  expect_false(identical(a, b))
})

test_that("run dir basename keeps the readable timestamp+slug prefix", {
  cfg <- list(runs_dir = tempfile("runs")); dir.create(cfg$runs_dir)
  d <- nextgenCrossWorkbench:::ngcd_new_run_dir(cfg, label = "trait_by_trait")
  expect_match(basename(d), "^[0-9]{8}-[0-9]{6}_trait_by_trait_")
})

test_that("a NULL label falls back to 'run'", {
  cfg <- list(runs_dir = tempfile("runs")); dir.create(cfg$runs_dir)
  d <- nextgenCrossWorkbench:::ngcd_new_run_dir(cfg, label = NULL)
  expect_match(basename(d), "^[0-9]{8}-[0-9]{6}_run_")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'library(testthat); library(nextgenCrossWorkbench); test_file("tests/testthat/test-run-isolation.R")'`
Expected: FAIL — same-second dirs collide (second `dir.create` hits an existing path; `a` and `b` are identical).

- [ ] **Step 3: Rewrite `ngcd_new_run_dir` with `tempfile()`**

In `R/run_backend.R`, replace:

```r
ngcd_new_run_dir <- function(cfg, label = NULL) {
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  slug  <- if (!is.null(label) && nzchar(label)) gsub("[^A-Za-z0-9_-]+", "_", label) else "run"
  dir <- file.path(cfg$runs_dir, paste0(stamp, "_", slug))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}
```

with:

```r
ngcd_new_run_dir <- function(cfg, label = NULL) {
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  slug  <- if (!is.null(label) && nzchar(label)) gsub("[^A-Za-z0-9_-]+", "_", label) else "run"
  # tempfile() guarantees a unique name within the session (counter + PID), so
  # two runs in the same second do not collide. Keep the stamp+slug as a
  # human-readable prefix.
  dir <- tempfile(pattern = paste0(stamp, "_", slug, "_"), tmpdir = cfg$runs_dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd <repo>; R CMD INSTALL . >/dev/null 2>&1; Rscript -e 'library(testthat); library(nextgenCrossWorkbench); test_file("tests/testthat/test-run-isolation.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/run_backend.R tests/testthat/test-run-isolation.R
git commit -m "fix(runs): unique run directories via tempfile (no same-second collision)"
```

---

### Task 4: Within-session retention cap

Prune old run directories beyond `cfg$keep_runs` (default 20; `0` = unlimited)
when a new run is created, so a long-lived container's disk stays bounded.

**Files:**
- Modify: `R/config.R` (add `keep_runs` to `defaults`), `R/run_backend.R`
  (add `ngcd_prune_runs`, call it from `ngcd_new_run_dir`)
- Test: `tests/testthat/test-run-isolation.R` (append)

**Interfaces:**
- Consumes: `cfg$runs_dir`, `cfg$keep_runs`.
- Produces: `ngcd_prune_runs(cfg)` returns (invisibly) the count removed;
  `ngcd_new_run_dir` calls it after creating the new dir.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-run-isolation.R`:

```r
test_that("ngcd_prune_runs keeps the newest N and removes older ones", {
  runs <- tempfile("runs"); dir.create(runs)
  cfg <- list(runs_dir = runs, keep_runs = 3L)
  made <- character(0)
  for (i in 1:5) {
    d <- file.path(runs, sprintf("run_%02d", i)); dir.create(d)
    Sys.setFileTime(d, Sys.time() + i)   # ascending mtime: run_05 newest
    made <- c(made, d)
  }
  removed <- nextgenCrossWorkbench:::ngcd_prune_runs(cfg)
  expect_equal(removed, 2L)
  left <- sort(basename(list.dirs(runs, recursive = FALSE)))
  expect_equal(left, c("run_03", "run_04", "run_05"))
})

test_that("keep_runs = 0 disables pruning", {
  runs <- tempfile("runs"); dir.create(runs)
  cfg <- list(runs_dir = runs, keep_runs = 0L)
  for (i in 1:4) dir.create(file.path(runs, sprintf("run_%02d", i)))
  expect_equal(nextgenCrossWorkbench:::ngcd_prune_runs(cfg), 0L)
  expect_length(list.dirs(runs, recursive = FALSE), 4L)
})

test_that("new run dir triggers pruning and is itself never pruned", {
  runs <- tempfile("runs"); dir.create(runs)
  cfg <- list(runs_dir = runs, keep_runs = 2L)
  for (i in 1:3) {
    d <- file.path(runs, sprintf("old_%02d", i)); dir.create(d)
    Sys.setFileTime(d, Sys.time() - 100 + i)   # all older than the new one
  }
  fresh <- nextgenCrossWorkbench:::ngcd_new_run_dir(cfg, label = "x")
  expect_true(dir.exists(fresh))                        # new dir survives
  expect_length(list.dirs(runs, recursive = FALSE), 2L) # capped at keep_runs
})

test_that("NGCD_KEEP_RUNS is an integer config override", {
  withr::with_envvar(c(NGCD_KEEP_RUNS = "5"), {
    cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
    expect_equal(suppressWarnings(as.integer(cfg$keep_runs)), 5L)
  })
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'library(testthat); library(nextgenCrossWorkbench); test_file("tests/testthat/test-run-isolation.R")'`
Expected: FAIL — `ngcd_prune_runs` not found; `cfg$keep_runs` NULL.

- [ ] **Step 3: Add `keep_runs` to config defaults**

In `R/config.R`, in the `defaults <- list(...)` block, add after `data_dir`:

```r
    # Max run directories kept during a session (0 = unlimited). Container disk
    # is bounded because artifacts are within-session only.
    keep_runs                = 20,
```

- [ ] **Step 4: Add `ngcd_prune_runs` and call it**

In `R/run_backend.R`, add above `ngcd_new_run_dir`:

```r
# Keep only the newest cfg$keep_runs run directories under cfg$runs_dir; delete
# older ones (by mtime). keep_runs = 0 (or NA/negative) disables pruning. Env
# overrides arrive as strings, so coerce. Returns the number removed.
ngcd_prune_runs <- function(cfg) {
  keep <- suppressWarnings(as.integer(cfg$keep_runs %||% 20L))
  if (is.na(keep) || keep <= 0L) return(invisible(0L))
  dirs <- list.dirs(cfg$runs_dir, recursive = FALSE)
  if (length(dirs) <= keep) return(invisible(0L))
  mt <- file.info(dirs)$mtime
  ordered <- dirs[order(mt, decreasing = TRUE)]   # newest first
  to_remove <- ordered[-seq_len(keep)]
  unlink(to_remove, recursive = TRUE, force = TRUE)
  invisible(length(to_remove))
}
```

Then, in `ngcd_new_run_dir`, add a pruning call after `dir.create`:

```r
ngcd_new_run_dir <- function(cfg, label = NULL) {
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  slug  <- if (!is.null(label) && nzchar(label)) gsub("[^A-Za-z0-9_-]+", "_", label) else "run"
  dir <- tempfile(pattern = paste0(stamp, "_", slug, "_"), tmpdir = cfg$runs_dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ngcd_prune_runs(cfg)   # bound disk; the just-created dir is newest, never pruned
  dir
}
```

- [ ] **Step 5: Reinstall and run the isolation + config tests**

Run: `cd <repo>; R CMD INSTALL . >/dev/null 2>&1; Rscript -e 'library(testthat); library(nextgenCrossWorkbench); test_file("tests/testthat/test-run-isolation.R"); test_file("tests/testthat/test-config.R")'`
Expected: PASS for both files.

- [ ] **Step 6: Commit**

```bash
git add R/config.R R/run_backend.R tests/testthat/test-run-isolation.R
git commit -m "feat(runs): within-session retention cap (NGCD_KEEP_RUNS, default 20)"
```

---

### Task 5: Surface the data_dir warning + within-session UX note

Show the fallback warning (Task 1) in the app's status messages, and add a short
copy line telling hosted users to download before leaving (artifacts are cleared
when the container ends). Copy only — no new controls.

**Files:**
- Modify: `R/config.R` (`ngcd_check_backend` — append `cfg$data_dir_warning` to
  `out$messages`), `R/app.R` (report/output area — add the copy line)
- Test: `tests/testthat/test-config.R` (append)

**Interfaces:**
- Consumes: `cfg$data_dir_warning`, existing `ngcd_check_backend` `out$messages`.
- Produces: nothing new.

- [ ] **Step 1: Confirm where messages are assembled**

Run: `grep -n 'out\$messages' R/config.R`
Expected: the `ngcd_check_backend` function appends user-facing strings to
`out$messages` (rendered as status chips in `app.R`). Note the line just before
`out` is returned.

- [ ] **Step 2: Write the failing test**

Append to `tests/testthat/test-config.R`:

```r
test_that("check_backend surfaces the data_dir fallback warning", {
  blocker <- tempfile("blocker"); file.create(blocker)
  bad <- file.path(blocker, "nope")
  withr::with_envvar(c(NGCD_DATA_DIR = bad), {
    cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
    b <- nextgenCrossWorkbench:::ngcd_check_backend(cfg)
    expect_true(any(grepl("not writable", b$messages, fixed = TRUE)))
  })
})
```

- [ ] **Step 3: Run test to verify it fails**

Run: `Rscript -e 'library(testthat); library(nextgenCrossWorkbench); test_file("tests/testthat/test-config.R")'`
Expected: FAIL — the warning is not in `b$messages`.

- [ ] **Step 4: Append the warning in `ngcd_check_backend`**

In `R/config.R`, inside `ngcd_check_backend`, immediately before the function
returns `out`, add:

```r
  if (!is.null(cfg$data_dir_warning) && nzchar(cfg$data_dir_warning))
    out$messages <- c(out$messages, cfg$data_dir_warning)
```

- [ ] **Step 5: Add the within-session UX line in `app.R`**

In `R/app.R`, in the report/output UI (near the download buttons rendered in
`output$res_report`, around the `downloadButton("dl_report_pdf", ...)` block),
add a muted help line:

```r
        shiny::tags$p(class = "help-hint",
          "Downloads are saved to your computer. Run files kept on the server ",
          "are cleared when you close the app — download anything you want to keep."),
```

Place it adjacent to the existing download buttons so it reads as guidance for
them. Match the surrounding `shiny::tags$…` construction.

- [ ] **Step 6: Reinstall and run the full fast suite**

Run: `cd <repo>; R CMD INSTALL . >/dev/null 2>&1; Rscript tests/testthat.R 2>&1 | tail -3`
Expected: `[ FAIL 0 | ... ]`.

- [ ] **Step 7: Commit**

```bash
git add R/config.R R/app.R tests/testthat/test-config.R
git commit -m "feat(ux): surface data_dir warning + within-session download note"
```

---

### Task 6: Full-suite regression + spec/config docs

Run the complete test suite including the backend-integration tier, and update
the config template so the new knobs are discoverable.

**Files:**
- Modify: `inst/app/config.template.yml` (document `data_dir` / `keep_runs`)
- Test: full suite (no new test file)

**Interfaces:** none.

- [ ] **Step 1: Document the new config keys**

In `inst/app/config.template.yml`, under the existing `default:` block, add:

```yaml
  # Writable base for run files, reports, and saved settings. Defaults to a
  # per-session temp dir (cleared when the app closes). Set to a real folder to
  # keep artifacts between sessions. Overridable via the NGCD_DATA_DIR env var.
  data_dir: ""
  # Max run directories kept during a session (0 = keep all). Overridable via
  # the NGCD_KEEP_RUNS env var.
  keep_runs: 20
```

(An empty `data_dir: ""` means "use the default"; `ngcd_load_config` treats
empty/NULL as unset via `%||%` and the defaults seed.)

Note: confirm empty string is treated as unset. If `cfg$data_dir` can arrive as
`""`, change the resolve line in `R/config.R` to
`cfg$data_dir <- if (is.null(cfg$data_dir) || !nzchar(cfg$data_dir)) file.path(tempdir(), "ngcd") else cfg$data_dir`
and add a test that `data_dir: ""` resolves to the tempdir default.

- [ ] **Step 2: Run the full non-integration suite**

Run: `cd <repo>; R CMD INSTALL . >/dev/null 2>&1; Rscript tests/testthat.R 2>&1 | tail -4`
Expected: `[ FAIL 0 | WARN 0 | SKIP 6 | PASS <n> ]` (6 backend-integration tests skip without the flags).

- [ ] **Step 3: Run the backend-integration tier**

Prerequisite: backend installed (`R CMD INSTALL` the `nextgenCrossDesign` tarball, or `remotes::install_github(...@v0.4.0)`).
Run: `cd <repo>/tests; NOT_CRAN=true NGCD_RUN_COMBINATIONS=1 Rscript testthat.R 2>&1 | tail -3`
Expected: `[ FAIL 0 | WARN 1 | SKIP 0 | PASS 373+ ]` (WARN 1 is the pre-existing expected het-parent-rejection path; the new tests add to the total).

- [ ] **Step 4: Commit**

```bash
git add inst/app/config.template.yml
git commit -m "docs(config): document data_dir and keep_runs knobs"
```

---

## Self-Review

**Spec coverage:**
- Problem 1 (non-unique run dirs) → Task 3.
- Problem 2 (read-only shared base; `runs/`+`_report/`+`presets/`) → Task 1 (base) + Task 2 (relocate call sites).
- Problem 3 (override clobbered after loop) → Task 1 (seed into defaults + `%||%` resolve) with a dedicated regression test.
- Problem 4 (process-global inline report preview) → spec accepts last-writer-wins for v1; Task 2 keeps `_report` writable under `data_dir`; no behaviour change required. Covered by decision, not code.
- Problem 5 (unbounded artifacts) → Task 4.
- Config surface (`NGCD_DATA_DIR`, `NGCD_KEEP_RUNS`) → Tasks 1, 4; documented Task 6.
- UX note (download-before-you-leave) → Task 5.
- Startup writability message → Task 1 (detection) + Task 5 (surface).
- Testing section → Tasks 1-6 tests; full suite Task 6.
- Compatibility (local unchanged; no new deps; backend/contract/runner untouched) → honored by defaults and by touching only frontend `R/` + config.

**Placeholder scan:** No TBD/TODO; every code step shows the code; every run step shows the command and expected output.

**Type/name consistency:** `ngcd_dir_writable`, `ngcd_prune_runs`, `ngcd_new_run_dir`, `cfg$data_dir`, `cfg$runs_dir`, `cfg$report_dir`, `cfg$presets_dir`, `cfg$keep_runs`, `cfg$data_dir_warning` used identically across all tasks.
