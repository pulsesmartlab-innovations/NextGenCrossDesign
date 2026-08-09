# CLI parity for the disomic-subgenome pipeline: the wrapper's workflow="stage" +
# prediction_family="subgenome" path (driving qc -> predict -> index -> allocate ->
# rank one stage at a time over a shared run_dir, via run_subgenome_stage() and the
# backend's generic stage store) must produce the SAME ng_run_result.v1 payload as a
# single-shot subgenome run (workflow="subgenome_design", run_subgenome_design()).
# Phase 2 of the staged-pipeline effort: the one-shot and staged paths share the
# subgenome_ctx_* stage functions, so they are byte-identical by construction --
# this proves it end-to-end through the real runner subprocess.
# Backend-gated: needs the disomic-subgenome scoring primitives + the R/45 stage store.

test_that("staged disomic-subgenome run matches a one-shot run for the same config", {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the backend subgenome-staging parity check.")
  skip_if(!backend_available(), "Backend not available.")
  ns <- asNamespace("nextgenCrossDesign")
  need <- c("ng_polyploid_subgenome_score_crosses", "ng_polyploid_subgenome_grm",
            "ng_polyploid_policy", "ng_polyploid_qc", "ng_fit_ridge_effects",
            "ng_make_pairs", "ng_stage_save", "ng_stage_load_ctx")
  skip_if_not(all(vapply(need, exists, logical(1), where = ns)),
              "Backend lacks the disomic-subgenome staging primitives.")

  runner <- testthat::test_path("..", "..", "inst", "app", "tools", "run_cross_prediction_json.R")
  if (!file.exists(runner))
    runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                          package = "nextgenCrossWorkbench")
  expect_true(file.exists(runner))

  # ---- self-contained synthetic allopolyploid: 2 disomic subgenomes ----------
  set.seed(20260809)
  rd <- tempfile("sgparity"); dir.create(rd, recursive = TRUE)
  ids <- sprintf("P%02d", 1:10)
  mkA <- 8L; mkB <- 8L
  gA <- matrix(sample(0:2, length(ids) * mkA, TRUE), length(ids), mkA,
               dimnames = list(ids, sprintf("A_m%02d", seq_len(mkA))))
  gB <- matrix(sample(0:2, length(ids) * mkB, TRUE), length(ids), mkB,
               dimnames = list(ids, sprintf("B_m%02d", seq_len(mkB))))
  dosage <- cbind(gA, gB)
  geno_file <- file.path(rd, "geno.csv")
  utils::write.csv(data.frame(NAME = ids, dosage, check.names = FALSE), geno_file, row.names = FALSE)

  mk <- colnames(dosage)
  map_file <- file.path(rd, "map.csv")
  utils::write.csv(data.frame(
    SNP_code = mk, Subgenome = ifelse(grepl("^A_", mk), "A", "B"),
    Chromosome = ifelse(grepl("^A_", mk), "1A", "1B"),
    pos_cm = c(seq(0, 70, length.out = mkA), seq(0, 70, length.out = mkB))),
    map_file, row.names = FALSE)

  y <- as.numeric(scale(dosage) %*% stats::rnorm(ncol(dosage))) + stats::rnorm(length(ids), 0, 0.5)
  pheno_file <- file.path(rd, "pheno.csv")
  utils::write.csv(data.frame(NAME = ids, yield = y), pheno_file, row.names = FALSE)

  data_cols <- list(
    genotype_file = geno_file, genotype_id_col = "NAME",
    map_file = map_file, map_marker_col = "SNP_code", subgenome_col = "Subgenome",
    map_chr_col = "Chromosome", map_pos_cm_col = "pos_cm", map_position_unit = "cm",
    phenotype_file = pheno_file, phenotype_id_col = "NAME", poly_trait_col = "yield",
    n_crosses = 6L, max_crosses_per_parent = 3L, seed = 7L, grm_method = "vanraden",
    poly_gain = "gain", selection_prop = 0.1, subgenome_progeny = "DH", run_qc = TRUE)

  runit <- function(cfg, out) {
    cf <- file.path(rd, paste0("cfg_", basename(out)))
    jsonlite::write_json(cfg, cf, auto_unbox = TRUE, null = "null", pretty = TRUE)
    msg <- system2("Rscript", c(runner, cf, out), stdout = TRUE, stderr = TRUE)
    list(msg = msg)
  }

  # ---- one-shot -------------------------------------------------------------
  resA <- file.path(rd, "oneshot.json")
  m1 <- runit(c(list(workflow = "subgenome_design"), data_cols), resA)
  expect_true(file.exists(resA), info = paste(m1$msg, collapse = "\n"))
  resultA <- jsonlite::fromJSON(resA, simplifyVector = TRUE)
  expect_true(isTRUE(resultA$ok))

  # ---- staged: qc -> predict -> index -> allocate -> rank over one run_dir ---
  run_dir <- file.path(rd, "stage_run"); dir.create(run_dir, recursive = TRUE)
  resB <- file.path(rd, "staged.json")
  for (s in c("qc", "predict", "index", "allocate", "rank")) {
    out_path <- if (identical(s, "rank")) resB else file.path(rd, paste0("stageout_", s, ".json"))
    m <- runit(c(list(workflow = "stage", prediction_family = "subgenome",
                      stage = s, run_dir = run_dir), data_cols), out_path)
    expect_true(file.exists(out_path),
                info = paste0("stage '", s, "':\n", paste(m$msg, collapse = "\n")))
  }
  resultB <- jsonlite::fromJSON(resB, simplifyVector = TRUE)
  expect_true(isTRUE(resultB$ok))

  # Strip run-to-run fields (timestamps/version), then compare the whole envelope.
  stab <- function(x) { x$generated_at <- NULL; x$package_version <- NULL; x }
  expect_equal(stab(resultA), stab(resultB))

  # Load-bearing fields checked explicitly.
  expect_equal(resultA$selected_crosses, resultB$selected_crosses)
  expect_equal(resultA$plan_summary, resultB$plan_summary)
  expect_equal(resultA$subgenome, resultB$subgenome)
  expect_equal(resultA$settings, resultB$settings)
})
