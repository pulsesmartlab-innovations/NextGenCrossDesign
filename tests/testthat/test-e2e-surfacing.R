# End-to-end verification against the real backend (nextgenCrossDesign >= 0.24.0).
#
# A "check" is a benchmark genotype under the reference-only redesign: it is NEVER a
# candidate parent (check x check would be a self, which the pair enumerator does not
# produce, so a check has no variance and cannot be a row in the cross table), it is
# supplied through its own genotype matrix (`check_geno`, optionally `check_pheno`), and it
# contributes one scalar reference value per trait. It never excludes a cross -- it only
# attaches per-trait reference columns (`<trait>_check_value`, `<trait>_vs_check`,
# `<trait>_check_ok`, `<trait>_p_beat_check`) plus `checks_all_ok` / `check_violation` to
# the SAME candidate table the run would have produced without it.
#
# This file previously encoded the OLD veto-era model (a check was just another candidate
# parent you compared against, `res$trait_check_diagnostics` was the top-level field) and,
# being gated on NGCD_RUN_COMBINATIONS=1, had gone stale invisibly through an entire model
# change: `check = "P01"` named an actual candidate parent, there was no `check_geno` /
# `check_progeny_size` (both are now hard-required whenever `trait_checks` is supplied),
# and the version guard still read ">= 0.14.0" (the veto release). Rewritten below to the
# reference-only model and run for real against backend 0.24.1.
#
# Backend-gated: only runs with NGCD_RUN_COMBINATIONS=1 and a local nextgenCrossDesign
# install >= 0.24.0 (when check_geno/check_progeny_size/trait_check_reference landed).

ngcd_e2e_cfg <- function(with_checks = FALSE) {
  cfg  <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wbe2e"))
  demo <- nextgenCrossWorkbench:::ngcd_demo_files(cfg)
  cfgj <- list(schema = "ng_run_config.v1",
    phenotype_file = demo$phenotype, genotype_file = demo$genotype,
    map_file = demo$map, direction_file = demo$direction,
    phenotype_id_col = "NAME", genotype_id_col = "NAME", direction_trait_col = "Trait",
    direction_column_col = "Trait", direction_direction_col = "Selection_direction",
    map_marker_col = "SNP_code", map_chr_col = "Chromosome",
    map_pos_bp_col = "Position_BP", map_position_unit = "bp", bp_per_cm = 1e6,
    prediction_mode = "trait_by_trait",
    # The demo's second trait ("disease") is a decrease trait -- carrying it alongside
    # "yield" (increase) lets the check-reference test verify vs_check's direction-aware
    # sign convention on BOTH directions in one real run, not just the increase case.
    traits_to_use = if (with_checks) list("yield", "disease") else list("yield"),
    trait_value_metric = "var_complex", uc_variance_source = "pmv",
    progeny = "DH", parent_type = "inbred", duplicate_action = "none",
    n_crosses = 8, max_crosses_per_parent = 4, optimizer = "greedy_local",
    allocation_method = "ocs", use_ocs = TRUE, seed = 1)
  if (isTRUE(with_checks)) {
    geno <- utils::read.csv(demo$genotype, check.names = FALSE)
    mk <- setdiff(names(geno), "NAME")
    set.seed(7)
    mk_row <- function(id) {
      row <- as.list(sample(0:2, length(mk), replace = TRUE, prob = c(0.3, 0.4, 0.3)))
      names(row) <- mk
      c(list(check_id = id), row)
    }
    # CHK_A/CHK_B are NOT in the parent genotype file (P01..P10): a check must never be a
    # candidate parent under the reference-only model.
    cfgj$check_geno <- list(mk_row("CHK_A"), mk_row("CHK_B"))
    # trait_value_metric = "var_complex" resolves the mean source to a phenotype basis
    # (adjusted_pheno), not a GEBV -- so the check needs a matching phenotype record via
    # check_pheno (a GEBV source would instead predict straight from check_geno and never
    # consult this table). Only CHK_A gets a record; CHK_B is genotyped but never
    # referenced by trait_checks, deliberately exercising a check line the run never scores.
    cfgj$check_pheno <- list(list(NAME = "CHK_A", yield = 55, disease = 3))
    # The k in P(beat check) = 1 - Phi((tau-mu)/sigma)^k -- the breeder's own number of
    # progeny per family, never defaulted by the backend.
    cfgj$check_progeny_size <- 20
    cfgj$trait_checks <- list(
      list(trait = "yield",   check = "CHK_A", direction = NULL),   # increase trait
      list(trait = "disease", check = "CHK_A", direction = NULL))   # decrease trait
  }
  cfgj
}

ngcd_e2e_run <- function(cfgj) {
  rd <- tempfile("run"); dir.create(rd, recursive = TRUE)
  cfgp <- file.path(rd, "config.json"); resp <- file.path(rd, "result.json")
  jsonlite::write_json(cfgj, cfgp, auto_unbox = TRUE, null = "null", pretty = TRUE)
  runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                        package = "nextgenCrossWorkbench")
  system2("Rscript", c(runner, cfgp, resp), stdout = FALSE, stderr = FALSE)
  jsonlite::fromJSON(resp, simplifyVector = TRUE)
}

ngcd_e2e_skip_guards <- function() {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the backend e2e surfacing check.")
  testthat::skip_if(!backend_available(), "Backend not available.")
  testthat::skip_if_not(packageVersion("nextgenCrossDesign") >= "0.24.0",
              "Needs backend >= 0.24.0 for check_geno/check_progeny_size/trait_check_reference.")
}

test_that("single-trait run with NO checks surfaces risk/portfolio end to end (checks are optional)", {
  ngcd_e2e_skip_guards()

  res <- ngcd_e2e_run(ngcd_e2e_cfg(with_checks = FALSE))
  expect_true(isTRUE(res$ok))

  # a check is optional: with none supplied, the reference block is simply absent, and no
  # per-cross check column leaks in.
  expect_true(is.null(res$trait_check_reference))

  sc <- res$selected_crosses
  expect_true(is.data.frame(sc) && nrow(sc) > 0)
  expect_false(any(grepl("_check_value$|_check_ok$|_p_beat_check$|_vs_check$", names(sc))))

  # single-trait risk/portfolio columns
  expect_true(all(c("cross_level", "cross_upside", "risk_bin", "portfolio_profile")
                  %in% names(sc)))
  expect_true("multi_trait_threshold_violation" %in% names(sc))

  # top-level diagnostics block
  expect_false(is.null(res$priority_risk_diagnostics))

  # report-side helpers consume the live (no-check) result without error
  p <- nextgenCrossWorkbench:::ngcd_portfolio_plotly(res)
  expect_true(inherits(p, "plotly") || inherits(p, "htmlwidget"))

  d <- nextgenCrossWorkbench:::ngcd_diagnostics(res)
  areas <- vapply(d, function(x) x$area, character(1))
  expect_true("priority_risk" %in% areas)
  expect_true("portfolio" %in% areas)
  expect_false("trait_check" %in% areas)
})

test_that("check reference surfaces end to end as a reference, not a filter, direction-aware both ways", {
  ngcd_e2e_skip_guards()

  res0 <- ngcd_e2e_run(ngcd_e2e_cfg(with_checks = FALSE))
  res  <- ngcd_e2e_run(ngcd_e2e_cfg(with_checks = TRUE))
  expect_true(isTRUE(res0$ok))
  expect_true(isTRUE(res$ok))

  ct0 <- res0$candidate_crosses
  ct  <- res$candidate_crosses
  expect_true(is.data.frame(ct0) && nrow(ct0) > 0)
  expect_true(is.data.frame(ct)  && nrow(ct)  > 0)
  # a check must never alter the candidate set (no filtering, no reordering)
  expect_equal(nrow(ct), nrow(ct0))

  ref <- res$trait_check_reference
  expect_false(is.null(ref))
  expect_true(all(c("active", "values", "source", "progeny_size", "diagnostics",
                    "p_beat_all_checks_note") %in% names(ref)))
  expect_true(is.data.frame(ref$active))
  expect_true(all(c("trait", "check", "reject_if") %in% names(ref$active)))
  expect_setequal(ref$active$trait, c("yield", "disease"))
  expect_true(all(ref$active$reject_if %in% c("above", "below")))
  expect_equal(ref$progeny_size, 20)

  dg <- ref$diagnostics
  expect_true(all(c("active", "n_wrong_side", "n_not_evaluable", "n_pev_unavailable",
                    "n_candidates") %in% names(dg)))
  expect_equal(dg$n_candidates, nrow(ct))

  # the check is never a parent
  expect_false(any(c(ct$parent1, ct$parent2) %in% ref$active$check))
  expect_false(any(c(ct$parent1, ct$parent2) %in% c("CHK_A", "CHK_B")))

  # per-cross reference columns for BOTH traits
  expect_true(all(c("yield_check_value", "yield_vs_check", "yield_check_ok",
                    "yield_p_beat_check") %in% names(ct)))
  expect_true(all(c("disease_check_value", "disease_vs_check", "disease_check_ok",
                    "disease_p_beat_check") %in% names(ct)))
  expect_true("checks_all_ok" %in% names(ct))
  expect_true("check_violation" %in% names(ct))

  # P(beat check) is a probability
  py <- ct$yield_p_beat_check[is.finite(ct$yield_p_beat_check)]
  pd <- ct$disease_p_beat_check[is.finite(ct$disease_p_beat_check)]
  expect_true(length(py) > 0 && all(py >= 0 & py <= 1))
  expect_true(length(pd) > 0 && all(pd >= 0 & pd <= 1))

  # vs_check is direction-aware -- verified against the ACTUAL mu/tau the run reports,
  # not merely assumed. yield is an increase trait (reject_if "below"): a cross scores
  # better than the check when its mean is ABOVE tau, so vs_check = mean - check_value.
  # disease is a decrease trait (reject_if "above"): better means BELOW tau, so vs_check
  # is sign-flipped: check_value - mean. Getting this backwards is the easiest mistake to
  # make in this feature, so both directions are checked in the same real run.
  tau_yield   <- unique(ct$yield_check_value)
  tau_disease <- unique(ct$disease_check_value)
  expect_length(tau_yield, 1L)
  expect_length(tau_disease, 1L)
  expect_equal(ct$yield_vs_check,   ct$yield_mean   - tau_yield,   tolerance = 1e-8)
  expect_equal(ct$disease_vs_check, tau_disease - ct$disease_mean, tolerance = 1e-8)
  # `_check_ok` agrees with the sign of `_vs_check` (ties count as ok, per the backend)
  expect_equal(ct$yield_check_ok,   ct$yield_vs_check   >= 0)
  expect_equal(ct$disease_check_ok, ct$disease_vs_check >= 0)
  # both directions are actually exercised (some crosses fall on each side of the check),
  # so the sign-convention assertions above are not vacuously true.
  expect_true(any(ct$yield_check_ok) && any(!ct$yield_check_ok))
  expect_true(any(ct$disease_check_ok) && any(!ct$disease_check_ok))

  # report-side helpers consume the checked result without error, and the diagnostics
  # helper now reports a trait_check area (it did not, in the no-checks run above).
  p <- nextgenCrossWorkbench:::ngcd_portfolio_plotly(res)
  expect_true(inherits(p, "plotly") || inherits(p, "htmlwidget"))

  d <- nextgenCrossWorkbench:::ngcd_diagnostics(res)
  areas <- vapply(d, function(x) x$area, character(1))
  expect_true("trait_check" %in% areas)
})
