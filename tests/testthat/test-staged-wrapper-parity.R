# CLI parity: the wrapper's workflow="stage" path (driving qc -> predict -> index ->
# allocate -> rank one stage at a time over a shared run_dir, via ng_run_stage()) must
# produce the SAME ng_run_result.v1 payload as a single-shot full run (workflow default,
# ng_run_cross_prediction()) for the identical config. Task 3 of the staged-pipeline
# effort: proves the wrapper's shared coerce/emit helpers keep the two paths byte-identical.
# Backend-gated: requires nextgenCrossDesign with ng_run_stage() exported.

test_that("workflow=stage staged run matches a full run for the same config", {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the backend staged-wrapper parity check.")
  skip_if(!backend_available(), "Backend not available.")
  skip_if_not(exists("ng_run_stage", where = asNamespace("nextgenCrossDesign")),
              "Backend lacks ng_run_stage() (staged pipeline not installed).")

  # Prefer the in-repo source wrapper (what a developer iterating on it is actually
  # editing); fall back to the installed copy so this test still works from an
  # arbitrary working directory / a built-package check.
  runner <- testthat::test_path("..", "..", "inst", "app", "tools", "run_cross_prediction_json.R")
  if (!file.exists(runner)) {
    runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                          package = "nextgenCrossWorkbench")
  }
  expect_true(file.exists(runner))

  dp <- demo_paths()
  base_cfg <- list(schema = "ng_run_config.v1",
    phenotype_file = dp$phenotype, genotype_file = dp$genotype,
    map_file = dp$map, direction_file = dp$direction,
    phenotype_id_col = "NAME", genotype_id_col = "NAME", direction_trait_col = "Trait",
    direction_column_col = "Trait", direction_direction_col = "Selection_direction",
    map_marker_col = "SNP_code", map_chr_col = "Chromosome",
    map_pos_bp_col = "Position_BP", map_position_unit = "bp", bp_per_cm = 1e6,
    prediction_mode = "trait_by_trait", multi_trait_method = "auto",
    trait_value_metric = "var_complex", progeny = "RIL", assume_inbred = TRUE,
    duplicate_action = "none", n_crosses = 8, max_crosses_per_parent = 4,
    optimizer = "greedy_local", allocation_method = "ocs", use_ocs = TRUE,
    seed = 20260719)

  rd <- tempfile("wbstaged"); dir.create(rd, recursive = TRUE)

  # ---- full run (workflow default -> ng_run_cross_prediction) --------------
  cfgA <- file.path(rd, "configA.json"); resA <- file.path(rd, "resultA.json")
  jsonlite::write_json(base_cfg, cfgA, auto_unbox = TRUE, null = "null", pretty = TRUE)
  system2("Rscript", c(runner, cfgA, resA), stdout = FALSE, stderr = FALSE)
  expect_true(file.exists(resA))
  resultA <- jsonlite::fromJSON(resA, simplifyVector = TRUE)
  expect_true(isTRUE(resultA$ok))

  # ---- staged run: qc -> predict -> index -> allocate -> rank over one run_dir ----
  run_dir <- file.path(rd, "stage_run"); dir.create(run_dir, recursive = TRUE)
  resB <- file.path(rd, "resultB.json")
  for (s in c("qc", "predict", "index", "allocate", "rank")) {
    stage_cfg <- utils::modifyList(base_cfg,
      list(workflow = "stage", stage = s, run_dir = run_dir))
    cfgS <- file.path(rd, paste0("config_", s, ".json"))
    jsonlite::write_json(stage_cfg, cfgS, auto_unbox = TRUE, null = "null", pretty = TRUE)
    out_path <- if (identical(s, "rank")) resB else file.path(rd, paste0("stageout_", s, ".json"))
    msg <- system2("Rscript", c(runner, cfgS, out_path), stdout = TRUE, stderr = TRUE)
    expect_true(file.exists(out_path),
               info = paste0("stage '", s, "' did not write output:\n", paste(msg, collapse = "\n")))
  }
  expect_true(file.exists(resB))
  resultB <- jsonlite::fromJSON(resB, simplifyVector = TRUE)
  expect_true(isTRUE(resultB$ok))

  # Strip fields that legitimately differ run-to-run (timestamps) before comparing.
  stab <- function(x) {
    for (k in c("generated_at", "elapsed", "output_files")) x[[k]] <- NULL
    if (!is.null(x$qc)) x$qc$generated_at <- NULL
    x
  }
  expect_equal(stab(resultA), stab(resultB))

  # Load-bearing fields, checked explicitly in case a jsonlite round-trip ever
  # introduces trivial numeric/format noise elsewhere in the envelope.
  expect_equal(resultA$selected_crosses, resultB$selected_crosses)
  expect_equal(resultA$plan_summary, resultB$plan_summary)
  expect_equal(resultA$candidate_crosses, resultB$candidate_crosses)
  expect_equal(resultA$qc$status, resultB$qc$status)
})
