# A rerun with the same inputs must produce the same answer.
#
# This is the property a breeder actually relies on: they run a season's data,
# show the plan to a colleague, rerun it, and the crosses must not move. Nothing
# guarded it until now, and several things could quietly break it -- the runner
# seeds a ridge CV fold split, the posterior draws, a 150-draw Monte Carlo for
# the multi-trait joint probability, and the optimizer. Any one of those
# consuming RNG out of a fixed order would make a rerun drift.
#
# It is also the property backend 0.28.0 had to repair from a different angle:
# per-trait seeds were derived from a trait's POSITION in the direction file, so
# reordering two rows moved a variance by 7.9e6x and changed 3 of 5 selected
# crosses. That is fixed upstream; this test guards the frontend's side of the
# same promise -- same config, same seed, same result.
#
# Deliberately compared with identical() at tolerance = 0. This is one machine
# running the same code twice, so exactness is both achievable and the point.
# (Cross-PLATFORM bit-identity is a different and unachievable claim -- see the
# backend's covariance_guards_bit_identity.R, which had to stop asserting it.)

test_that("a rerun with the same config and seed returns identical results", {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the reproducibility check (two real backend runs).")
  skip_if(!backend_available(), backend_skip_reason())

  cfg0 <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wbrep"))
  demo <- nextgenCrossWorkbench:::ngcd_demo_files(cfg0)

  rd <- tempfile("repro"); dir.create(rd, recursive = TRUE)
  dirf <- file.path(rd, "dir.csv")
  utils::write.csv(data.frame(Trait = c("yield", "disease"),
                              Selection_direction = c("increase", "decrease"),
                              stringsAsFactors = FALSE),
                   dirf, row.names = FALSE, quote = FALSE)

  # Every stochastic add-on switched ON, so this covers the posterior draws, the
  # robust re-optimization and the joint-probability Monte Carlo -- not just the
  # deterministic scoring path.
  cfgj <- list(schema = "ng_run_config.v1",
    phenotype_file = demo$phenotype, genotype_file = demo$genotype,
    map_file = demo$map, direction_file = dirf,
    phenotype_id_col = "NAME", genotype_id_col = "NAME",
    direction_trait_col = "Trait", direction_column_col = "Trait",
    direction_direction_col = "Selection_direction",
    map_marker_col = "SNP_code", map_chr_col = "Chromosome",
    map_pos_bp_col = "Position_BP", map_position_unit = "bp", bp_per_cm = 1e6,
    prediction_mode = "trait_by_trait", multi_trait_method = "auto",
    trait_value_metric = "usefulness", progeny = "DH", parent_type = "inbred",
    duplicate_action = "none",
    run_posterior_prediction = TRUE, posterior_method = "closed_form",
    n_iter = 12, burn_in = 4,
    n_crosses = 6, max_crosses_per_parent = 4, optimizer = "greedy_local",
    allocation_method = "ocs", use_ocs = TRUE, seed = 20260706,
    robust_allocation = TRUE, robustness_quantile = 0.25,
    robust_objective = "posterior_quantile", multitrait_joint_prob = TRUE)

  run_once <- function(tag) {
    cfgp <- file.path(rd, paste0("cfg_", tag, ".json"))
    resp <- file.path(rd, paste0("res_", tag, ".json"))
    jsonlite::write_json(cfgj, cfgp, auto_unbox = TRUE, null = "null", pretty = TRUE)
    runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                          package = "nextgenCrossWorkbench")
    system2("Rscript", c(runner, cfgp, resp), stdout = FALSE, stderr = FALSE)
    jsonlite::fromJSON(resp, simplifyVector = TRUE)
  }

  a <- run_once("a")
  b <- run_once("b")
  expect_true(isTRUE(a$ok), info = a$error_message %||% "run a failed")
  expect_true(isTRUE(b$ok), info = b$error_message %||% "run b failed")

  # The scored candidates and the plan: what the breeder reads and acts on.
  expect_identical(a$candidate_crosses, b$candidate_crosses)
  expect_identical(a$selected_crosses,  b$selected_crosses)
  expect_identical(a$plan_summary,      b$plan_summary)

  # The stochastic add-ons, which is where drift would appear first.
  expect_identical(a$robust_plan,      b$robust_plan)
  expect_identical(a$multitrait_joint, b$multitrait_joint)

  # Guard the guard: if the run stopped producing these, the identity checks
  # above would pass vacuously on two NULLs.
  expect_true(is.data.frame(a$candidate_crosses) && nrow(a$candidate_crosses) > 0)
  expect_false(is.null(a$robust_plan))
  expect_false(is.null(a$multitrait_joint))
})
