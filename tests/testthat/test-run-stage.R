# Staged pipeline invoker (Phase 2, Task 1): ngcd_run_stage() drives ONE stage
# per subprocess call against a PERSISTENT run_dir (ngcd_new_pipeline_dir())
# so artifacts accumulate (compute-once). Backend-gated (runs the real
# staged-pipeline entry point, ng_run_stage(), stage by stage).

test_that("qc -> predict -> index -> allocate -> rank over one pipeline dir accumulates artifacts", {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the backend staged-pipeline check.")
  skip_if(!backend_available(), "Backend not available.")
  skip_if_not(exists("ng_run_stage", where = asNamespace("nextgenCrossDesign")),
              "Backend lacks ng_run_stage() (staged pipeline not installed).")

  cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wbstage"))

  dp <- demo_paths()
  demo_data <- list(
    genotype  = utils::read.csv(dp$genotype, check.names = FALSE),
    phenotype = utils::read.csv(dp$phenotype, check.names = FALSE),
    map       = utils::read.csv(dp$map, check.names = FALSE),
    direction = utils::read.csv(dp$direction, check.names = FALSE))

  params <- list(schema = "ng_run_config.v1",
    phenotype_file = dp$phenotype, genotype_file = dp$genotype,
    map_file = dp$map, direction_file = dp$direction,
    phenotype_id_col = "NAME", genotype_id_col = "NAME", direction_trait_col = "Trait",
    direction_column_col = "Trait", direction_direction_col = "Selection_direction",
    map_marker_col = "SNP_code", map_chr_col = "Chromosome",
    map_pos_bp_col = "Position_BP", map_position_unit = "bp", bp_per_cm = 1e6,
    prediction_mode = "trait_by_trait", multi_trait_method = "auto",
    trait_value_metric = "var_complex", progeny = "RIL", parent_type = "inbred",
    duplicate_action = "none", n_crosses = 8, max_crosses_per_parent = 4,
    optimizer = "greedy_local", allocation_method = "ocs", use_ocs = TRUE,
    seed = 20260719)

  run_dir <- nextgenCrossWorkbench:::ngcd_new_pipeline_dir(cfg)
  expect_true(dir.exists(run_dir))
  expect_match(basename(run_dir), "_pipe_")

  run_stage <- nextgenCrossWorkbench:::ngcd_run_stage
  results <- list()
  for (s in c("qc", "predict", "index", "allocate", "rank")) {
    out <- run_stage(cfg, s, run_dir, params, data = if (identical(s, "qc")) demo_data else NULL)
    expect_true(out$ok, info = paste0("stage '", s, "' failed: ", out$error_message, "\n", out$log))
    expect_false(is.null(out$status), info = paste0("stage '", s, "' returned no status"))
    results[[s]] <- out
  }

  expect_true(results$qc$status %in% c("pass", "warning"))
  expect_identical(results$predict$status, "done")
  expect_identical(results$index$status, "done")
  expect_identical(results$allocate$status, "done")
  expect_identical(results$rank$status, "done")

  # the run_dir is a single persistent dir shared across all five calls
  for (o in results) expect_identical(o$run_dir, run_dir)

  # backend-written figure/gate JSON accumulates in the shared run_dir
  for (s in c("qc", "predict", "index", "allocate")) {
    expect_true(file.exists(file.path(run_dir, "artifacts", paste0(s, ".json"))),
                info = paste0("missing artifacts/", s, ".json"))
    expect_false(is.null(results[[s]]$stage_json))
  }

  # rank carries the full ng_run_result.v1 payload, not a per-stage figure JSON
  expect_false(is.null(results$rank$result))
  expect_true(isTRUE(results$rank$result$ok))
  expect_true(is.data.frame(results$rank$result$selected_crosses))
  expect_true(nrow(results$rank$result$selected_crosses) > 0)

  # prune-protect: even with keep_runs exceeded, the protected pipeline dir survives
  cfg2 <- cfg
  cfg2$keep_runs <- 1L
  for (i in 1:3) {
    d <- file.path(cfg2$runs_dir, sprintf("decoy_%02d", i))
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    Sys.setFileTime(d, Sys.time() + 1000 + i)   # newer than run_dir -> would evict it
  }
  nextgenCrossWorkbench:::ngcd_prune_runs(cfg2, protect = run_dir)
  expect_true(dir.exists(run_dir))
})
