# Regression cover for the three multi-trait defects fixed in workbench 0.29.0
# (backend nextgenCrossDesign 0.26.0). Two of them changed numbers a breeder was
# shown; the third turned a UI invitation into a hard error after a full run.
#
#   A  the "Robust plan" on a multi-trait run was a SINGLE-TRAIT plan, chosen by
#      the row order of the trait-direction file
#   B  the multi-trait joint P(superior progeny) used PMV as the per-progeny
#      variance, silently assumed trait independence, and mis-read direction
#      tokens the backend accepts
#   C  an economic_weight / desired_change column self-promotes multi_trait_method
#      = "auto" into a selection-index method needing P and G, which this app
#      cannot supply
library(shiny)

mtf_cfg <- function() nextgenCrossWorkbench:::ngcd_load_config(tempfile("wbmtf"))
mtf_srv <- function() nextgenCrossWorkbench:::workbench_server(mtf_cfg())

# A run config over the demo data, with the direction file swappable so a test can
# reorder the traits, and every add-on this file exercises switched on.
mtf_cfg_json <- function(direction_file, phenotype_file = NULL, ...) {
  demo <- nextgenCrossWorkbench:::ngcd_demo_files(mtf_cfg())
  base <- list(schema = "ng_run_config.v1",
    phenotype_file = phenotype_file %||% demo$phenotype,
    genotype_file = demo$genotype, map_file = demo$map,
    direction_file = direction_file,
    phenotype_id_col = "NAME", genotype_id_col = "NAME", direction_trait_col = "Trait",
    direction_column_col = "Trait", direction_direction_col = "Selection_direction",
    map_marker_col = "SNP_code", map_chr_col = "Chromosome",
    map_pos_bp_col = "Position_BP", map_position_unit = "bp", bp_per_cm = 1e6,
    prediction_mode = "trait_by_trait", multi_trait_method = "auto",
    trait_value_metric = "usefulness", progeny = "DH", parent_type = "inbred",
    duplicate_action = "none",
    run_posterior_prediction = TRUE, posterior_method = "closed_form",
    n_iter = 12, burn_in = 4,
    n_crosses = 6, max_crosses_per_parent = 4, optimizer = "greedy_local",
    allocation_method = "ocs", use_ocs = TRUE, seed = 1,
    robust_allocation = TRUE, robustness_quantile = 0.25,
    robust_objective = "posterior_quantile",
    multitrait_joint_prob = TRUE)
  # Overrides REPLACE the default of the same name; appending would leave a duplicate
  # key whose first (default) copy is the one the runner reads.
  extra <- list(...)
  if (length(extra)) base[names(extra)] <- extra
  base
}

mtf_run <- function(rd, cfgj, tag = "r") {
  cfgp <- file.path(rd, paste0("cfg_", tag, ".json"))
  resp <- file.path(rd, paste0("res_", tag, ".json"))
  jsonlite::write_json(cfgj, cfgp, auto_unbox = TRUE, null = "null", pretty = TRUE)
  runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                        package = "nextgenCrossWorkbench")
  system2("Rscript", c(runner, cfgp, resp), stdout = FALSE, stderr = FALSE)
  jsonlite::fromJSON(resp, simplifyVector = TRUE)
}

mtf_dir_file <- function(rd, traits, dirs, tag) {
  p <- file.path(rd, paste0("dir_", tag, ".csv"))
  utils::write.csv(data.frame(Trait = traits, Selection_direction = dirs,
                              stringsAsFactors = FALSE),
                   p, row.names = FALSE, quote = FALSE)
  p
}

# Unordered cross keys, sorted -- a plan is a SET of crosses, not an ordering.
mtf_keys <- function(df) sort(paste(pmin(df$parent1, df$parent2),
                                    pmax(df$parent1, df$parent2)))

mtf_skip <- function() {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the backend multi-trait checks.")
  skip_if(!backend_available(), "Backend not available.")
}

# ===========================================================================
# FIX A -- the robust plan is the INDEX's robust plan, not trait 1's
# ===========================================================================

test_that("a multi-trait robust plan is allocated on the selection index, from an exact cached quantile", {
  mtf_skip()
  skip_if(utils::packageVersion("nextgenCrossDesign") < package_version("0.26.0"),
          "Backend lacks the multi-trait index posterior (needs >= 0.26.0).")

  rd <- tempfile("mtfA"); dir.create(rd, recursive = TRUE)
  res <- mtf_run(rd, mtf_cfg_json(mtf_dir_file(rd, c("yield", "disease"),
                                               c("increase", "decrease"), "yd")), "A")
  expect_true(isTRUE(res$ok), info = res$error_message)
  rp  <- res$robust_plan
  why <- rp$error %||% ""
  expect_null(rp$error, info = why)

  # The gain column is the index the standard plan itself was ranked on -- not one
  # trait's usefulness. Before 0.29.0 this was usefulness_pmv_gebv for whichever
  # trait the direction file happened to list first.
  expect_identical(rp$gain_col, "multi_trait_score", info = why)
  expect_identical(rp$basis, "selection_index", info = why)
  expect_true("multi_trait_score" %in% names(res$selected_crosses), info = why)
  expect_match(rp$basis_label, "selection index", fixed = TRUE)

  # The index is direction-normalised higher = better for every method, and the
  # orientation is READ from the backend's stamp rather than assumed here.
  expect_identical(rp$direction, "maximize", info = why)
  expect_identical(rp$direction_source, "posterior_metadata", info = why)

  # ...and the tail is the exact cached empirical quantile of the index draws, never
  # a normal approximation (which this app must never ask for).
  s <- rp$summary
  expect_equal(s$robust_tail_probability, 0.25, info = why)
  expect_match(s$robust_quantile_source, "^multi_trait_score_post_q0_25$", info = why)
  expect_false(isTRUE(s$robustness_quantile_is_normal_approximation), info = why)
  expect_false(grepl("_post_lower$|_post_upper$", s$robust_quantile_source), info = why)

  expect_equal(nrow(rp$crosses), 6L, info = why)
  expect_equal(rp$n_shared_with_standard + rp$n_changed, 6L, info = why)
})

test_that("reordering the trait-direction file does not change what the robust plan is about, nor (on well-conditioned traits) the plan itself", {
  mtf_skip()

  rd <- tempfile("mtfA2"); dir.create(rd, recursive = TRUE)
  # The defect's signature was that swapping two rows of the direction CSV changed
  # which trait the "robust plan" was about AND flipped its orientation. Assert on
  # the demo traits that neither happens any more.
  a <- mtf_run(rd, mtf_cfg_json(mtf_dir_file(rd, c("yield", "disease"),
                                             c("increase", "decrease"), "yd")), "A2a")
  b <- mtf_run(rd, mtf_cfg_json(mtf_dir_file(rd, c("disease", "yield"),
                                             c("decrease", "increase"), "dy")), "A2b")
  expect_true(isTRUE(a$ok), info = a$error_message)
  expect_true(isTRUE(b$ok), info = b$error_message)
  expect_null(a$robust_plan$error); expect_null(b$robust_plan$error)
  expect_identical(a$robust_plan$gain_col, b$robust_plan$gain_col)
  expect_identical(a$robust_plan$basis, b$robust_plan$basis)
  # Before 0.29.0 this pair was "maximize" vs "minimize": the robust plan silently
  # answered a different question depending on spreadsheet row order.
  expect_identical(a$robust_plan$direction, b$robust_plan$direction)
  expect_identical(a$robust_plan$summary$robust_quantile_source,
                   b$robust_plan$summary$robust_quantile_source)

  # The plan ITSELF is row-order invariant once the per-trait inputs are, which needs
  # both traits to be predictable enough that the backend's ridge lambda is not chosen
  # by an RNG-position-dependent cross-validation. The demo's `disease` is not (its
  # cross-validated predictive r2 is negative), so a second trait derived from `yield`
  # is used to isolate THIS app's contribution: same index, same draws, same plan.
  demo <- nextgenCrossWorkbench:::ngcd_demo_files(mtf_cfg())
  ph <- utils::read.csv(demo$phenotype, stringsAsFactors = FALSE)
  ph$yield_b <- 2 * ph$yield + 5
  pf <- file.path(rd, "pheno_two_good.csv"); utils::write.csv(ph, pf, row.names = FALSE)
  ga <- mtf_run(rd, mtf_cfg_json(mtf_dir_file(rd, c("yield", "yield_b"),
                                              c("increase", "increase"), "gg1"),
                                 phenotype_file = pf), "A2c")
  gb <- mtf_run(rd, mtf_cfg_json(mtf_dir_file(rd, c("yield_b", "yield"),
                                              c("increase", "increase"), "gg2"),
                                 phenotype_file = pf), "A2d")
  expect_true(isTRUE(ga$ok), info = ga$error_message)
  expect_true(isTRUE(gb$ok), info = gb$error_message)
  expect_null(ga$robust_plan$error, info = ga$robust_plan$error %||% "")
  expect_null(gb$robust_plan$error, info = gb$robust_plan$error %||% "")
  expect_identical(mtf_keys(ga$robust_plan$crosses), mtf_keys(gb$robust_plan$crosses))
  # ...and it is genuinely a robust plan, not a copy of the standard one.
  expect_identical(ga$robust_plan$gain_col, "multi_trait_score")
})

test_that("a single-trait run still robust-allocates on its own per-trait posterior", {
  mtf_skip()

  rd <- tempfile("mtfA3"); dir.create(rd, recursive = TRUE)
  res <- mtf_run(rd, mtf_cfg_json(mtf_dir_file(rd, "yield", "increase", "y1")), "A3")
  expect_true(isTRUE(res$ok), info = res$error_message)
  rp <- res$robust_plan; why <- rp$error %||% ""
  expect_null(rp$error, info = why)
  expect_identical(rp$basis, "single_trait", info = why)
  expect_identical(rp$gain_col, "usefulness_pmv_gebv", info = why)
  expect_match(rp$basis_label, "yield", fixed = TRUE)
  expect_match(rp$summary$robust_quantile_source, "^usefulness_pmv_gebv_post_q0_25$", info = why)
  expect_false(isTRUE(rp$summary$robustness_quantile_is_normal_approximation), info = why)
})

test_that("with no index posterior the multi-trait robust allocation is refused with the reason, never silently reduced to trait 1", {
  mtf_skip()

  rd <- tempfile("mtfA4"); dir.create(rd, recursive = TRUE)
  # Robust allocation requested but posterior prediction off: there is no index
  # posterior to robustify. The old code fell through to posterior_predictions[[1]]
  # (or to a bare "no scores" message); it must now say what is missing and why.
  res <- mtf_run(rd, mtf_cfg_json(mtf_dir_file(rd, c("yield", "disease"),
                                               c("increase", "decrease"), "yd4"),
                                  run_posterior_prediction = FALSE), "A4")
  expect_true(isTRUE(res$ok), info = res$error_message)
  rp <- res$robust_plan
  expect_false(is.null(rp))
  expect_null(rp$crosses)
  expect_false(is.null(rp$error))
  expect_match(rp$error, "multi_trait_score", fixed = TRUE)
  expect_match(rp$error, "0.26.0", fixed = TRUE)
})

# ===========================================================================
# FIX B -- the joint P(superior progeny)
# ===========================================================================

test_that("the multi-trait joint probability uses VPM and the exact within-family cross-trait covariance", {
  mtf_skip()

  rd <- tempfile("mtfB"); dir.create(rd, recursive = TRUE)
  res <- mtf_run(rd, mtf_cfg_json(mtf_dir_file(rd, c("yield", "disease"),
                                               c("increase", "decrease"), "ydB")), "B")
  expect_true(isTRUE(res$ok), info = res$error_message)
  mj <- res$multitrait_joint
  expect_false(is.null(mj))
  why <- mj$error %||% ""
  expect_null(mj$error, info = why)

  # 2a: the per-progeny variance is VPM. PMV carries shared marker-effect
  # uncertainty, which every progeny of the cross has in common and which the
  # 1 - (1 - p)^k order statistic cannot exponentiate away.
  expect_identical(unlist(mj$variance_columns, use.names = FALSE),
                   c("yield_vpm", "disease_vpm"))
  # 2b/2d: the exact recombination-aware within-family covariance, not a population
  # proxy and never a silent diag().
  expect_identical(mj$covariance_model, "exact_within_family")
  expect_match(mj$covariance_note, "exact", fixed = TRUE)
  expect_true(is.finite(mj$mean_p) && mj$mean_p >= 0 && mj$mean_p <= 1)
  # What it is NOT: an effect-uncertainty-integrated probability. Said in the result
  # rather than left for a reader to assume.
  expect_identical(mj$effect_uncertainty, "point_estimate")

  # The reported column really is what those inputs produce: recompute it here from
  # the same candidate table by calling the backend directly.
  cc <- res$candidate_crosses
  expect_true(all(c("wf_var_yield", "wf_var_disease", "wf_cov_yield_disease",
                    "p_superior_progeny_mt") %in% names(cc)))
  ph <- utils::read.csv(nextgenCrossWorkbench:::ngcd_demo_files(mtf_cfg())$phenotype,
                        stringsAsFactors = FALSE)
  targets <- c(yield = mean(ph$yield, na.rm = TRUE), disease = mean(ph$disease, na.rm = TRUE))
  ts <- data.frame(trait = c("yield", "disease"),
                   mean_col = c("yield_mean", "disease_mean"),
                   var_col = c("yield_vpm", "disease_vpm"), stringsAsFactors = FALSE)
  ref <- nextgenCrossDesign::ng_add_p_superior_progeny_multitrait(
    scores = cc, trait_specs = ts,
    tau_lower = c(targets[["yield"]], -Inf), tau_upper = c(Inf, targets[["disease"]]),
    cross_trait_cov = cc[, c("parent1", "parent2", "wf_var_yield",
                             "wf_cov_yield_disease", "wf_var_disease"), drop = FALSE])
  expect_equal(cc$p_superior_progeny_mt, ref$p_superior_progeny_mt, tolerance = 1e-8)

  # ...and it is NOT what the pre-0.29.0 recipe produced (PMV as the per-progeny
  # variance, independence across traits). If these ever agree the fix is inert.
  old <- nextgenCrossDesign::ng_add_p_superior_progeny_multitrait(
    scores = cc,
    trait_specs = data.frame(trait = c("yield", "disease"),
                             mean_col = c("yield_mean", "disease_mean"),
                             var_col = c("yield_pmv", "disease_pmv"),
                             stringsAsFactors = FALSE),
    tau_lower = c(targets[["yield"]], -Inf), tau_upper = c(Inf, targets[["disease"]]),
    G_hat = diag(2))
  expect_gt(max(abs(old$p_superior_progeny_mt - cc$p_superior_progeny_mt), na.rm = TRUE), 0.1)
  expect_lt(mean(cc$p_superior_progeny_mt, na.rm = TRUE),
            mean(old$p_superior_progeny_mt, na.rm = TRUE))
})

test_that("direction tokens the backend accepts are not inverted by the joint probability", {
  mtf_skip()

  rd <- tempfile("mtfB2"); dir.create(rd, recursive = TRUE)
  # "max"/"min" are accepted maximize/minimize tokens. The pre-0.29.0 test was
  # `dirs %in% c("increase", "maximize")`, which treated every other accepted
  # synonym as a DECREASE trait and swapped tau_lower for tau_upper.
  a <- mtf_run(rd, mtf_cfg_json(mtf_dir_file(rd, c("yield", "disease"),
                                             c("increase", "decrease"), "tok1")), "B2a")
  b <- mtf_run(rd, mtf_cfg_json(mtf_dir_file(rd, c("yield", "disease"),
                                             c("max", "min"), "tok2")), "B2b")
  expect_true(isTRUE(a$ok), info = a$error_message)
  expect_true(isTRUE(b$ok), info = b$error_message)
  expect_null(a$multitrait_joint$error); expect_null(b$multitrait_joint$error)
  expect_equal(a$multitrait_joint$mean_p, b$multitrait_joint$mean_p, tolerance = 1e-10)
  expect_equal(a$candidate_crosses$p_superior_progeny_mt,
               b$candidate_crosses$p_superior_progeny_mt, tolerance = 1e-10)
})

test_that("the joint-probability block no longer carries its own direction vocabulary or a PMV variance", {
  # Source-level lock for the two silent defects above: the ad-hoc token set and the
  # _pmv variance column must not come back, and independence must never be the
  # unannounced fallback. Ungated -- it reads the shipped runner script.
  runner <- testthat::test_path("..", "..", "inst", "app", "tools", "run_cross_prediction_json.R")
  if (!file.exists(runner))
    runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                          package = "nextgenCrossWorkbench")
  src <- paste(readLines(runner, warn = FALSE), collapse = "\n")
  expect_false(grepl('dirs %in% c("increase", "maximize")', src, fixed = TRUE))
  expect_false(grepl('var_cols <- paste0(traits, "_pmv")', src, fixed = TRUE))
  expect_false(grepl('error = function(e) diag(length(traits))', src, fixed = TRUE))
  expect_true(grepl("ng_multitrait_spec", src, fixed = TRUE))
  expect_true(grepl("cross_trait_cov = ctc", src, fixed = TRUE))
})

# ===========================================================================
# FIX C -- the auto -> economic_index / desired_gain self-promotion
# ===========================================================================

test_that("ngcd_auto_index_promotion_message fires exactly when the backend would promote AND the matrices are missing", {
  m <- nextgenCrossWorkbench:::ngcd_auto_index_promotion_message
  plain <- data.frame(Trait = c("yield", "disease"),
                      Selection_direction = c("increase", "decrease"),
                      stringsAsFactors = FALSE)
  econ  <- cbind(plain, economic_weight = c(2, 1))
  des   <- cbind(plain, desired_change = c(1.5, 0.5))

  expect_null(m(plain, "multi", "auto"))              # nothing to promote on
  expect_null(m(econ, "single", "auto"))              # not a multi-trait objective
  expect_null(m(econ, "index", "auto"))
  expect_null(m(econ, "multi", "weighted"))           # method chosen explicitly
  expect_null(m(NULL, "multi", "auto"))               # no direction file loaded yet
  expect_null(m(cbind(plain, economic_weight = c(0, 0)), "multi", "auto"))  # not positive
  expect_null(m(cbind(plain, economic_weight = c(NA, NA)), "multi", "auto"))

  msg <- m(econ, "multi", "auto")
  expect_false(is.null(msg))
  expect_match(msg, "economic_weight", fixed = TRUE)
  expect_match(msg, "economic_index", fixed = TRUE)
  expect_match(msg, "genetic covariance (G)", fixed = TRUE)
  expect_match(msg, "phenotypic covariance (P)", fixed = TRUE)
  # The breeder is told nothing was rewritten behind their back.
  expect_match(msg, "nothing has been dropped", fixed = TRUE)

  # desired_change wins over economic_weight, exactly as the backend orders them.
  expect_match(m(des, "multi", "auto"), "desired_gain", fixed = TRUE)
  expect_match(m(cbind(des, economic_weight = c(2, 1)), "multi", "auto"),
               "desired_gain", fixed = TRUE)

  # ---- the interim refusal is gone: a BACKED promotion runs -----------------
  # Smith-Hazel needs both matrices; with only one it still names the other.
  expect_match(m(econ, "multi", "auto", has_phenotypic = TRUE, has_genetic = FALSE),
               "genetic covariance (G)", fixed = TRUE)
  expect_match(m(econ, "multi", "auto", has_phenotypic = FALSE, has_genetic = TRUE),
               "phenotypic covariance (P)", fixed = TRUE)
  expect_null(m(econ, "multi", "auto", has_phenotypic = TRUE, has_genetic = TRUE))
  # Pesek-Baker needs G alone -- P is optional there, so G by itself is enough.
  expect_match(m(des, "multi", "auto", has_phenotypic = TRUE, has_genetic = FALSE),
               "genetic covariance (G)", fixed = TRUE)
  expect_null(m(des, "multi", "auto", has_phenotypic = FALSE, has_genetic = TRUE))
})

test_that("the run gate refuses a direction file that would self-promote, and only in the workflow that builds an index", {
  testServer(mtf_srv(), {
    do.call(session$setInputs, demo_inputs())
    session$setInputs(objective_mode = "multi", multi_trait_method = "auto")
    rv$data$direction <- data.frame(Trait = c("yield", "disease"),
                                    Selection_direction = c("increase", "decrease"),
                                    economic_weight = c(2, 1), stringsAsFactors = FALSE)
    msg <- check_unsupported_combo_message()
    expect_false(is.null(msg))
    expect_match(msg, "economic_weight", fixed = TRUE)

    # An explicitly chosen method does not promote, so it is not blocked.
    session$setInputs(multi_trait_method = "weighted")
    expect_null(check_unsupported_combo_message())

    # Neither is a single-trait objective...
    session$setInputs(multi_trait_method = "auto", objective_mode = "single")
    expect_null(check_unsupported_combo_message())

    # ...nor another workflow: the polyploid and subgenome configs build no index,
    # so a direction table left over from the standard workflow must not block them.
    session$setInputs(objective_mode = "multi", workflow = "polyploid")
    expect_null(check_unsupported_combo_message())
    session$setInputs(workflow = "subgenome")
    expect_null(check_unsupported_combo_message())

    # ...and a clean direction file runs.
    session$setInputs(workflow = "standard")
    rv$data$direction <- data.frame(Trait = c("yield", "disease"),
                                    Selection_direction = c("increase", "decrease"),
                                    stringsAsFactors = FALSE)
    expect_null(check_unsupported_combo_message())
  })
})

test_that("the Selection objective help explains what each index method needs", {
  html <- paste(suppressWarnings(as.character(
    nextgenCrossWorkbench:::workbench_ui(mtf_cfg(), dev = FALSE))), collapse = " ")
  # The original text promised a path that could not run at all.
  expect_false(grepl("if your direction file carries economic weights", html, fixed = TRUE))
  # The interim text told breeders to leave those columns out entirely; both
  # methods work now, so the help must name the columns AND the two matrices.
  expect_false(grepl("refused before it starts", html, fixed = TRUE))
  expect_match(html, "economic_weight", fixed = TRUE)
  expect_match(html, "desired_change", fixed = TRUE)
  expect_match(html, "Smith-Hazel", fixed = TRUE)
  expect_match(html, "Pesek-Baker", fixed = TRUE)
  expect_match(html, "desired gains needs G alone", fixed = TRUE)
})
