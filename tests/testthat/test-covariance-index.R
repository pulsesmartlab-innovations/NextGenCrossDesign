# User-supplied phenotypic (P) and genetic (G) covariance matrices, and the two
# formal selection indices that need them: economic_index (Smith-Hazel,
# b = P^-1 G a) and desired_gain (Pesek-Baker, b = G^-1 d).
#
# THE THING THIS FILE EXISTS TO PROVE
# -----------------------------------
# jsonlite drops dimnames on a matrix round trip, and the app reaches the
# backend by writing config JSON and shelling out to
# inst/app/tools/run_cross_prediction_json.R. A bare matrix therefore arrives
# UNLABELLED, and backend 0.27.0 reads an unlabelled matrix POSITIONALLY -- its
# documented fallback, which it cannot tell apart from a reordering. The backend
# measured what that costs on a 3-trait permutation: Smith-Hazel coefficients
# moving by max |db| = 0.1708 and the emitted index re-ranking the candidate
# crosses at Spearman 0.9168, with no error and no warning.
#
# So P and G cross the bridge in LONG FORM -- {traits, cells:[{trait_row,
# trait_col, value}]} -- and the runner rebuilds them by a tiling assert. The
# assertions below are the proof that the labels survive: a PERMUTED but
# correctly labelled matrix must give a bit-identical index to the
# correctly-ordered one, both through the pure encoder/decoder pair and (under
# the backend gate) through a real end-to-end run.

ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")

# A 3-trait P/G pair with genuine off-diagonal structure: a permutation of a
# diagonal matrix would be indistinguishable from a positional read.
cov_P <- function() matrix(
  c(4.00, 1.50, 0.20,
    1.50, 9.00, -0.70,
    0.20, -0.70, 2.25), 3, 3, byrow = TRUE,
  dimnames = list(c("yield", "disease", "lodging"), c("yield", "disease", "lodging")))
cov_G <- function() matrix(
  c(2.00, 0.60, 0.10,
    0.60, 4.00, -0.30,
    0.10, -0.30, 1.00), 3, 3, byrow = TRUE,
  dimnames = list(c("yield", "disease", "lodging"), c("yield", "disease", "lodging")))

cov_table <- function(M) {
  df <- data.frame(trait = rownames(M), stringsAsFactors = FALSE)
  for (cn in colnames(M)) df[[cn]] <- unname(M[, cn])
  df
}

# ---------------------------------------------------------------------------
# 1. Reading an uploaded covariance CSV
# ---------------------------------------------------------------------------

test_that("a well-formed covariance table reads into a labelled square matrix", {
  out <- ng("ngcd_cov_from_table")(cov_table(cov_G()), "G")
  expect_true(out$ok)
  expect_identical(out$matrix, cov_G())
})

test_that("a covariance table read out of trait order still keys by NAME", {
  M <- cov_G()
  shuffled <- cov_table(M)[c(3, 1, 2), c(1, 4, 2, 3)]   # rows and columns permuted
  out <- ng("ngcd_cov_from_table")(shuffled, "G")
  expect_true(out$ok)
  # Labels win over position: realigning to the canonical order recovers M exactly.
  expect_identical(out$matrix[rownames(M), rownames(M)], M)
})

test_that("a malformed covariance table is refused with a message naming the problem", {
  from <- ng("ngcd_cov_from_table")
  M <- cov_G()

  expect_match(from(NULL, "G")$message, "could not be read", fixed = TRUE)

  not_square <- cov_table(M)[, 1:3]                      # 3 rows, 2 value columns
  expect_match(from(not_square, "G")$message, "must be square", fixed = TRUE)

  mismatched <- cov_table(M); names(mismatched)[2] <- "yeild"   # header typo
  msg <- from(mismatched, "G")$message
  expect_match(msg, "SAME traits", fixed = TRUE)
  expect_match(msg, "yeild", fixed = TRUE)               # names the offending label
  expect_match(msg, "yield", fixed = TRUE)

  dupes <- cov_table(M); dupes$trait[2] <- "yield"
  expect_match(from(dupes, "G")$message, "Duplicated row label(s): yield", fixed = TRUE)

  nonnum <- cov_table(M); nonnum$disease[1] <- "n/a"
  msg <- from(nonnum, "G")$message
  expect_match(msg, "finite number", fixed = TRUE)
  expect_match(msg, "(yield, disease)", fixed = TRUE)    # names the offending cell
})

# ---------------------------------------------------------------------------
# 2. Validating against the traits the run will actually index
# ---------------------------------------------------------------------------

test_that("extra traits are SUBSET by the caller (backend 0.27.0 errors on them)", {
  v <- ng("ngcd_validate_cov")(cov_G(), c("yield", "disease"), "G")
  expect_true(v$ok)
  expect_identical(rownames(v$matrix), c("yield", "disease"))
  expect_identical(v$matrix, cov_G()[c("yield", "disease"), c("yield", "disease")])
  expect_match(paste(v$notes, collapse = " "), "ignoring lodging", fixed = TRUE)
})

test_that("a trait of the run that the matrix does not cover is refused by name", {
  v <- ng("ngcd_validate_cov")(cov_G(), c("yield", "protein"), "G")
  expect_false(v$ok)
  expect_match(v$message, "Missing: protein", fixed = TRUE)
  expect_match(v$message, "The matrix covers: yield, disease, lodging", fixed = TRUE)
})

test_that("asymmetry, a non-positive variance and a non-PSD matrix are each refused", {
  v <- ng("ngcd_validate_cov")
  tn <- c("yield", "disease", "lodging")

  asym <- cov_G(); asym["yield", "disease"] <- 0.9
  msg <- v(asym, tn, "G")$message
  expect_match(msg, "must be symmetric", fixed = TRUE)
  expect_match(msg, "(yield, disease)", fixed = TRUE)   # names the cell AND its mirror

  zero_var <- cov_G(); zero_var["disease", "disease"] <- 0
  expect_match(v(zero_var, tn, "G")$message, "Not positive for: disease", fixed = TRUE)

  # A correlation stronger than the variances allow -> negative eigenvalue.
  npsd <- matrix(c(1, 0.99, 0.99, 0.99, 1, -0.99, 0.99, -0.99, 1), 3, 3,
                 dimnames = list(tn, tn))
  expect_match(v(npsd, tn, "G")$message, "not positive semidefinite", fixed = TRUE)
})

test_that("a near-singular matrix is refused for the index that has to invert it, and reports the condition number", {
  v <- ng("ngcd_validate_cov")
  tn <- c("yield", "disease")
  # disease is very nearly a scalar multiple of yield: PSD, but hopeless to invert.
  near <- matrix(c(1, 1 - 1e-7, 1 - 1e-7, 1), 2, 2, dimnames = list(tn, tn))

  # Not inverted by this run -> accepted (it is still a valid covariance matrix).
  expect_true(v(near, tn, "G", require_invertible = FALSE)$ok)

  # Inverted by this run -> refused, with the number that explains why. The
  # BACKEND would not refuse: it ridges (1e-6) and pseudo-inverts, returning
  # plausible-looking coefficients built out of rounding error.
  out <- v(near, tn, "G", require_invertible = TRUE)
  expect_false(out$ok)
  expect_match(out$message, "condition number", fixed = TRUE)
  expect_match(out$message, "very nearly redundant", fixed = TRUE)

  # A well-conditioned matrix passes and reports its condition number as a note.
  ok <- v(cov_G(), c("yield", "disease", "lodging"), "G", require_invertible = TRUE)
  expect_true(ok$ok)
  expect_match(paste(ok$notes, collapse = " "), "Condition number", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# 3. THE JSON BRIDGE: do the trait labels survive?
# ---------------------------------------------------------------------------

test_that("the hazard is real: a bare matrix loses its dimnames crossing jsonlite", {
  M <- cov_G()
  back <- jsonlite::fromJSON(jsonlite::toJSON(M))
  expect_true(is.matrix(back))
  expect_equal(dim(back), dim(M))
  expect_null(dimnames(back))          # <- silently unlabelled: read positionally
})

test_that("the long-form payload survives the real config -> runner round trip, labels intact", {
  M <- cov_G()
  payload <- ng("ngcd_cov_payload")(M, "G")
  expect_identical(payload$schema, "ngcd_labelled_matrix.v1")
  expect_identical(payload$traits, rownames(M))
  expect_length(payload$cells, 9L)

  # Written by the app's own config writer, read by the runner's own reader --
  # identical settings on both sides, so this exercises the real bridge.
  path <- tempfile(fileext = ".json")
  ng("ngcd_write_config")(list(genetic_covariance = payload), path)
  raw <- jsonlite::fromJSON(path, simplifyVector = TRUE,
                            simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  back <- ngcd_runner_env()$ngcd_cov_from_payload(raw$genetic_covariance,
                                                  "genetic_covariance")
  expect_identical(back, M)            # values AND both sets of dimnames
})

test_that("a PERMUTED but labelled payload rebuilds to the same matrix by name", {
  M <- cov_G()
  perm <- c("lodging", "yield", "disease")
  Mp <- M[perm, perm, drop = FALSE]

  path <- tempfile(fileext = ".json")
  ng("ngcd_write_config")(list(genetic_covariance = ng("ngcd_cov_payload")(Mp, "G")), path)
  raw <- jsonlite::fromJSON(path, simplifyVector = TRUE,
                            simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  back <- ngcd_runner_env()$ngcd_cov_from_payload(raw$genetic_covariance, "G")

  expect_identical(rownames(back), perm)                 # arrived in ITS order
  expect_identical(back[rownames(M), rownames(M)], M)    # and means the same thing
})

test_that("the runner refuses anything that could be read positionally", {
  dec <- ngcd_runner_env()$ngcd_cov_from_payload
  M <- cov_G()
  good <- ng("ngcd_cov_payload")(M, "G")

  # A bare (unlabelled) matrix -- what a naive jsonlite round trip delivers.
  expect_error(dec(unname(M), "G"), "without trait labels")
  # Labels on one dimension only -- the backend's documented silent-positional case.
  half <- M; colnames(half) <- NULL
  expect_error(dec(half, "G"), "without trait labels")
  # No payload structure at all.
  expect_error(dec(list(cells = good$cells), "G"), "labelled-matrix payload")
  expect_error(dec(list(traits = good$traits), "G"), "labelled-matrix payload")

  # A cell naming a trait the payload does not declare (the misspelling case).
  bad <- good; bad$cells[[1]]$trait_col <- "yeild"
  expect_error(dec(bad, "G"), "not one of its traits")

  # A gap in the tiling: p^2 cells must exactly cover traits x traits.
  gap <- good; gap$cells <- gap$cells[-5]
  expect_error(dec(gap, "G"), "incomplete")

  # ...and no cell may be written twice.
  dup <- good; dup$cells[[9]] <- dup$cells[[1]]
  expect_error(dup_err <- dec(dup, "G"), "more than once")
})

test_that("ngcd_cov_payload refuses to serialise a matrix that has lost its labels", {
  expect_error(ng("ngcd_cov_payload")(unname(cov_G()), "G"), "BOTH rownames and colnames")
})

# ---------------------------------------------------------------------------
# 4. Which index is on, and what it still needs
# ---------------------------------------------------------------------------

test_that("ngcd_index_trait_set follows the DIRECTION file, not the phenotype columns", {
  f <- ng("ngcd_index_trait_set")
  d <- data.frame(Trait = c("yield", "disease"),
                  Selection_direction = c("increase", "decrease"),
                  stringsAsFactors = FALSE)
  expect_identical(f(d, "Trait", "Trait", NULL), c("yield", "disease"))
  expect_identical(f(d, "Trait", "Trait", "yield"), "yield")
  expect_identical(f(NULL, NULL, NULL, NULL), character(0))
  # traits_to_use may name the phenotype COLUMN rather than the trait label,
  # exactly as ng_run_cp_trait_spec() allows.
  d2 <- data.frame(Trait = c("yield", "disease"), column = c("YLD", "DIS"),
                   Selection_direction = c("increase", "decrease"),
                   stringsAsFactors = FALSE)
  expect_identical(f(d2, "Trait", "column", "DIS"), "disease")
})

test_that("ngcd_effective_index_method mirrors the backend's auto-promotion", {
  f <- ng("ngcd_effective_index_method")
  plain <- data.frame(Trait = c("yield", "disease"), stringsAsFactors = FALSE)
  expect_true(is.na(f(plain, "single", "auto")))
  expect_identical(f(plain, "multi", "weighted"), "weighted")
  expect_identical(f(plain, "multi", "auto"), "auto")
  expect_identical(f(cbind(plain, economic_weight = c(2, 1)), "multi", "auto"), "economic_index")
  expect_identical(f(cbind(plain, desired_change = c(1, 0)), "multi", "auto"), "desired_gain")
  # desired_change wins, as it does in ng_breeder_selection_objective().
  expect_identical(f(cbind(plain, desired_change = c(1, 0), economic_weight = c(2, 1)),
                     "multi", "auto"), "desired_gain")
})

test_that("ngcd_index_method_message names the MISSING matrix, and stays silent once it is there", {
  f <- ng("ngcd_index_method_message")
  econ <- data.frame(Trait = c("yield", "disease"), economic_weight = c(2, 1),
                     stringsAsFactors = FALSE)
  des  <- data.frame(Trait = c("yield", "disease"), desired_change = c(1.5, 0),
                     stringsAsFactors = FALSE)

  # Methods that use neither matrix are never gated.
  expect_null(f("auto", econ, FALSE, FALSE))
  expect_null(f("weighted", econ, FALSE, FALSE))

  # economic_index needs BOTH.
  msg <- f("economic_index", econ, has_phenotypic = FALSE, has_genetic = FALSE)
  expect_match(msg, "phenotypic covariance (P)", fixed = TRUE)
  expect_match(msg, "genetic covariance (G)", fixed = TRUE)
  expect_match(msg, "Trait covariance matrices", fixed = TRUE)   # says WHERE to put it
  expect_match(f("economic_index", econ, TRUE, FALSE), "genetic covariance (G)", fixed = TRUE)
  expect_null(f("economic_index", econ, TRUE, TRUE))

  # desired_gain needs G ALONE -- P is optional there from backend 0.27.0.
  expect_match(f("desired_gain", des, TRUE, FALSE), "genetic covariance (G)", fixed = TRUE)
  expect_null(f("desired_gain", des, FALSE, TRUE))

  # ...and each also needs its own target column in the trait-direction file.
  expect_match(f("economic_index", des, TRUE, TRUE), "economic_weight", fixed = TRUE)
  expect_match(f("desired_gain", econ, TRUE, TRUE), "desired_change", fixed = TRUE)
})

test_that("ngcd_desired_gain_unavailable_message explains a G-only desired_gain run", {
  f <- ng("ngcd_desired_gain_unavailable_message")
  expect_null(f(NULL))
  expect_null(f(list(multitrait_desired_gain_unavailable = character(0))))
  expect_null(f(list(multitrait_desired_gain_unavailable = NA_character_)))
  msg <- f(list(multitrait_desired_gain_unavailable = "predicted_response"))
  expect_match(msg, "predicted response", fixed = TRUE)
  expect_match(msg, "genetic covariance (G) alone", fixed = TRUE)
  expect_match(msg, "unaffected", fixed = TRUE)       # the index itself is fine
  expect_match(msg, "sqrt(b' P b)", fixed = TRUE)     # and why the number is blank
})

# ---------------------------------------------------------------------------
# 5. Staged-pipeline bookkeeping: P/G belong to the `index` stage
# ---------------------------------------------------------------------------

test_that("phenotypic_covariance / genetic_covariance invalidate the index stage and nothing else", {
  sub <- ng("ngcd_stage_cfg_subset")
  p <- list(multi_trait_method = "economic_index",
            phenotypic_covariance = list(traits = "yield"),
            genetic_covariance = list(traits = "yield"),
            n_crosses = 10)
  expect_true(all(c("phenotypic_covariance", "genetic_covariance") %in% names(sub(p, "index"))))
  for (stage in c("qc", "predict", "allocate", "rank")) {
    expect_false(any(c("phenotypic_covariance", "genetic_covariance") %in% names(sub(p, stage))))
  }
})

# ---------------------------------------------------------------------------
# 6. Server wiring: the payload really reaches the config, and the gate refuses
# ---------------------------------------------------------------------------

cov_srv <- function()
  nextgenCrossWorkbench:::workbench_server(
    nextgenCrossWorkbench:::ngcd_load_config(tempfile("wbcov")))

# The demo direction file names yield + disease, so P/G over those two traits
# (plus a third the run does not use, to exercise the caller-side subsetting).
demo_cov_files <- function() {
  M <- cov_G(); P <- cov_P()
  gp <- tempfile(fileext = ".csv"); pp <- tempfile(fileext = ".csv")
  utils::write.csv(cov_table(M), gp, row.names = FALSE)
  utils::write.csv(cov_table(P), pp, row.names = FALSE)
  list(g = gp, p = pp)
}

test_that("build_params() sends P and G as a LABELLED payload, subset to the run's traits", {
  f <- demo_cov_files()
  shiny::testServer(cov_srv(), {
    do.call(session$setInputs, demo_inputs())
    session$setInputs(objective_mode = "multi", multi_trait_method = "economic_index",
                      f_pcov = list(datapath = f$p, name = "P.csv"),
                      f_gcov = list(datapath = f$g, name = "G.csv"))
    session$flushReact()

    p <- build_params()
    expect_false(is.null(p$genetic_covariance))
    expect_false(is.null(p$phenotypic_covariance))
    # Subset to the run's two traits -- backend 0.27.0 errors on the extra
    # "lodging" label rather than silently narrowing a program-wide matrix.
    expect_identical(p$genetic_covariance$traits, c("yield", "disease"))
    expect_length(p$genetic_covariance$cells, 4L)
    # Every value carries its own row AND column label: nothing can be read
    # positionally, even in principle.
    expect_true(all(vapply(p$genetic_covariance$cells,
      function(c) all(c("trait_row", "trait_col", "value") %in% names(c)), logical(1))))

    # ...and the runner rebuilds exactly the sub-matrix, by name.
    back <- ngcd_runner_env()$ngcd_cov_from_payload(p$genetic_covariance, "G")
    expect_identical(back, cov_G()[c("yield", "disease"), c("yield", "disease")])

    # A single-trait objective combines nothing, so neither matrix is sent.
    session$setInputs(objective_mode = "single")
    expect_null(build_params()$genetic_covariance)
    expect_null(build_params()$phenotypic_covariance)
  })
})

# Uploading is used rather than poking rv$data directly: any change to a file
# input re-runs load_data(), which REPLACES rv$data wholesale -- so a
# hand-assigned direction table would be silently wiped by the next upload.
cov_upload_inputs <- function(direction_file, ...) {
  d <- demo_paths()
  demo_inputs(data_source = "upload",
    f_geno  = list(datapath = d$genotype,  name = "g.csv"),
    f_pheno = list(datapath = d$phenotype, name = "p.csv"),
    f_map   = list(datapath = d$map,       name = "m.csv"),
    f_dir   = list(datapath = direction_file, name = "d.csv"),
    objective_mode = "multi", ...)
}

# The demo direction file with a target column bolted on, written to disk.
cov_direction_file <- function(extra) {
  d <- utils::read.csv(demo_paths()$direction, check.names = FALSE)
  for (nm in names(extra)) d[[nm]] <- extra[[nm]]
  path <- tempfile(fileext = ".csv"); utils::write.csv(d, path, row.names = FALSE)
  path
}

test_that("the run gate refuses an index whose matrix is missing, and names it", {
  f <- demo_cov_files()
  econ_dir <- cov_direction_file(list(economic_weight = c(2, 1)))
  shiny::testServer(cov_srv(), {
    do.call(session$setInputs,
            cov_upload_inputs(econ_dir, multi_trait_method = "economic_index"))
    session$flushReact()
    msg <- check_unsupported_combo_message()
    expect_false(is.null(msg))
    expect_match(msg, "phenotypic covariance (P)", fixed = TRUE)
    expect_match(msg, "genetic covariance (G)", fixed = TRUE)

    # Supplying only G still blocks Smith-Hazel, and now names only P.
    session$setInputs(f_gcov = list(datapath = f$g, name = "G.csv"))
    session$flushReact()
    msg <- check_unsupported_combo_message()
    expect_match(msg, "phenotypic covariance (P)", fixed = TRUE)
    expect_false(grepl("and the genetic covariance (G)", msg, fixed = TRUE))

    # With both matrices, Smith-Hazel runs.
    session$setInputs(f_pcov = list(datapath = f$p, name = "P.csv"))
    session$flushReact()
    expect_null(check_unsupported_combo_message())
  })
})

test_that("desired_gain is satisfied by G alone -- P is optional there", {
  f <- demo_cov_files()
  des_dir <- cov_direction_file(list(desired_change = c(1.5, 0.5)))
  shiny::testServer(cov_srv(), {
    do.call(session$setInputs,
            cov_upload_inputs(des_dir, multi_trait_method = "desired_gain"))
    session$flushReact()
    expect_match(check_unsupported_combo_message(), "genetic covariance (G)", fixed = TRUE)

    session$setInputs(f_gcov = list(datapath = f$g, name = "G.csv"))
    session$flushReact()
    expect_null(check_unsupported_combo_message())        # G alone is enough
    # ...and G alone is what gets sent: P is simply absent, not faked.
    p <- build_params()
    expect_false(is.null(p$genetic_covariance))
    expect_null(p$phenotypic_covariance)
  })
})

test_that("the interim auto-promotion refusal is replaced by a real path", {
  f <- demo_cov_files()
  # A desired_change column self-promotes "auto" to desired_gain in the backend.
  des_dir <- cov_direction_file(list(desired_change = c(1.5, 0.5)))
  shiny::testServer(cov_srv(), {
    do.call(session$setInputs, cov_upload_inputs(des_dir, multi_trait_method = "auto"))
    session$flushReact()
    expect_identical(cov_state()$method, "desired_gain")   # the promotion is seen
    msg <- check_unsupported_combo_message()
    expect_false(is.null(msg))                             # still refused with no G...
    expect_match(msg, "genetic covariance (G)", fixed = TRUE)
    expect_match(msg, "Trait covariance matrices", fixed = TRUE)  # ...pointing at the card

    session$setInputs(f_gcov = list(datapath = f$g, name = "G.csv"))
    session$flushReact()
    expect_null(check_unsupported_combo_message())         # ...and now it simply runs
    expect_false(is.null(build_params()$genetic_covariance))
  })
})

test_that("a broken matrix blocks only a run that would use one", {
  bad <- tempfile(fileext = ".csv")
  M <- cov_G(); M["yield", "disease"] <- 0.9        # asymmetric
  utils::write.csv(cov_table(M), bad, row.names = FALSE)
  des_dir <- cov_direction_file(list(desired_change = c(1.5, 0.5)))
  shiny::testServer(cov_srv(), {
    do.call(session$setInputs,
            cov_upload_inputs(des_dir, multi_trait_method = "weighted",
                              f_gcov = list(datapath = bad, name = "G.csv")))
    session$flushReact()
    # "weighted" never touches G, so a bad G must not block it...
    expect_null(check_unsupported_combo_message())
    expect_null(build_params()$genetic_covariance)   # and it is never sent

    # ...but it does block the index that would solve from it.
    session$setInputs(multi_trait_method = "desired_gain")
    session$flushReact()
    msg <- check_unsupported_combo_message()
    expect_match(msg, "must be symmetric", fixed = TRUE)
    expect_match(msg, "(yield, disease)", fixed = TRUE)
  })
})

# ---------------------------------------------------------------------------
# 7. END TO END against the real backend (nextgenCrossDesign >= 0.27.0)
#
# This is the assertion the whole design exists for: a PERMUTED but correctly
# labelled P/G must produce a bit-identical index to the correctly-ordered one.
# Backend-gated (NGCD_RUN_COMBINATIONS=1) because it drives real runs.
# ---------------------------------------------------------------------------

cov_e2e_guards <- function() {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
    "Set NGCD_RUN_COMBINATIONS=1 to run the backend covariance-index checks.")
  testthat::skip_if(!backend_available(), "Backend not available.")
  testthat::skip_if_not(packageVersion("nextgenCrossDesign") >= "0.27.0",
    "Needs backend >= 0.27.0 for user-supplied phenotypic_covariance / genetic_covariance.")
}

# The demo direction file with the target column each index needs bolted on.
cov_e2e_direction <- function(extra) {
  d <- utils::read.csv(demo_paths()$direction, check.names = FALSE)
  for (nm in names(extra)) d[[nm]] <- extra[[nm]]
  path <- tempfile(fileext = ".csv")
  utils::write.csv(d, path, row.names = FALSE)
  path
}

# P and G over the demo's two traits (yield, disease), deliberately NOT diagonal.
cov_e2e_P <- function() matrix(c(6.0, -1.2, -1.2, 2.5), 2, 2,
  dimnames = list(c("yield", "disease"), c("yield", "disease")))
cov_e2e_G <- function() matrix(c(3.0, -0.8, -0.8, 1.1), 2, 2,
  dimnames = list(c("yield", "disease"), c("yield", "disease")))

cov_e2e_cfg <- function(method, direction_file, P = NULL, G = NULL) {
  cfg <- list(schema = "ng_run_config.v1",
    phenotype_file = demo_paths()$phenotype, genotype_file = demo_paths()$genotype,
    map_file = demo_paths()$map, direction_file = direction_file,
    phenotype_id_col = "NAME", genotype_id_col = "NAME",
    direction_trait_col = "Trait", direction_column_col = "Trait",
    direction_direction_col = "Selection_direction",
    map_marker_col = "SNP_code", map_chr_col = "Chromosome",
    map_pos_bp_col = "Position_BP", map_position_unit = "bp", bp_per_cm = 1e6,
    prediction_mode = "trait_by_trait", multi_trait_method = method,
    trait_value_metric = "var_complex", uc_variance_source = "pmv",
    progeny = "DH", parent_type = "inbred", duplicate_action = "none",
    n_crosses = 8, max_crosses_per_parent = 4, optimizer = "greedy_local",
    allocation_method = "ocs", use_ocs = TRUE, seed = 1)
  pay <- nextgenCrossWorkbench:::ngcd_cov_payload
  if (!is.null(P)) cfg$phenotypic_covariance <- pay(P, "P")
  if (!is.null(G)) cfg$genetic_covariance <- pay(G, "G")
  cfg
}

cov_e2e_run <- function(cfg) {
  rd <- tempfile("covrun"); dir.create(rd, recursive = TRUE)
  cfgp <- file.path(rd, "config.json"); resp <- file.path(rd, "result.json")
  nextgenCrossWorkbench:::ngcd_write_config(cfg, cfgp)
  runner <- testthat::test_path("..", "..", "inst", "app", "tools",
                                "run_cross_prediction_json.R")
  if (!file.exists(runner))
    runner <- system.file("app", "tools", "run_cross_prediction_json.R",
                          package = "nextgenCrossWorkbench")
  log <- suppressWarnings(system2("Rscript", c(shQuote(runner), shQuote(cfgp), shQuote(resp)),
                                  stdout = TRUE, stderr = TRUE))
  res <- if (file.exists(resp))
    jsonlite::fromJSON(resp, simplifyVector = TRUE) else list(ok = FALSE)
  res$.log <- paste(log, collapse = "\n")
  res
}

test_that("economic_index: a PERMUTED but labelled P/G gives the SAME index as the ordered one", {
  cov_e2e_guards()
  dir_file <- cov_e2e_direction(list(economic_weight = c(2, 1)))
  P <- cov_e2e_P(); G <- cov_e2e_G()

  ordered <- cov_e2e_run(cov_e2e_cfg("economic_index", dir_file, P, G))
  expect_true(isTRUE(ordered$ok), info = ordered$error_message %||% ordered$.log)

  perm <- c("disease", "yield")
  permuted <- cov_e2e_run(cov_e2e_cfg("economic_index", dir_file,
                                      P[perm, perm, drop = FALSE],
                                      G[perm, perm, drop = FALSE]))
  expect_true(isTRUE(permuted$ok), info = permuted$error_message %||% permuted$.log)

  # THE assertion: the labels, not the order, decide the index.
  expect_identical(ordered$selected_crosses$multi_trait_score,
                   permuted$selected_crosses$multi_trait_score)
  expect_identical(ordered$selected_crosses$parent1, permuted$selected_crosses$parent1)
  expect_identical(ordered$selected_crosses$parent2, permuted$selected_crosses$parent2)
  expect_identical(ordered$plan_summary$multitrait_economic_index_coefficients,
                   permuted$plan_summary$multitrait_economic_index_coefficients)
})

test_that("a MISLABELLED matrix is refused, and the error names the bad label", {
  cov_e2e_guards()
  dir_file <- cov_e2e_direction(list(economic_weight = c(2, 1)))
  G <- cov_e2e_G()
  bad <- G; dimnames(bad) <- list(c("yield", "diseasee"), c("yield", "diseasee"))

  res <- cov_e2e_run(cov_e2e_cfg("economic_index", dir_file, cov_e2e_P(), bad))
  expect_false(isTRUE(res$ok))
  blob <- paste(c(res$error_message, res$.log), collapse = " ")
  expect_match(blob, "diseasee", fixed = TRUE)   # names the offending label...
  expect_match(blob, "disease", fixed = TRUE)    # ...and the one it should have been
})

test_that("desired_gain runs on G alone, and says which reported quantity is missing", {
  cov_e2e_guards()
  dir_file <- cov_e2e_direction(list(desired_change = c(1.5, 0.5)))
  G <- cov_e2e_G()

  g_only <- cov_e2e_run(cov_e2e_cfg("desired_gain", dir_file, P = NULL, G = G))
  expect_true(isTRUE(g_only$ok), info = g_only$error_message %||% g_only$.log)

  # The coefficients b = G^-1 d never touch P, so the index is complete...
  both <- cov_e2e_run(cov_e2e_cfg("desired_gain", dir_file, P = cov_e2e_P(), G = G))
  expect_true(isTRUE(both$ok), info = both$error_message %||% both$.log)
  expect_identical(g_only$selected_crosses$multi_trait_score,
                   both$selected_crosses$multi_trait_score)

  # ...only the P-scaled REPORTED quantity is NA, and it is named, not blank.
  ps <- g_only$plan_summary
  expect_true("predicted_response" %in%
                as.character(unlist(ps$multitrait_desired_gain_unavailable)))
  expect_true(all(is.na(unlist(ps$multitrait_desired_gain_predicted_response))))
  expect_false(any(is.na(unlist(both$plan_summary$multitrait_desired_gain_predicted_response))))

  # ...and the app turns that stamp into a sentence rather than a blank cell,
  # on the Results screen AND in the exported report.
  msg <- nextgenCrossWorkbench:::ngcd_desired_gain_unavailable_message(ps)
  expect_match(msg, "predicted response", fixed = TRUE)
  expect_null(nextgenCrossWorkbench:::ngcd_desired_gain_unavailable_message(both$plan_summary))
  html <- nextgenCrossWorkbench:::ngcd_exec_summary_html(g_only)
  expect_match(html, "predicted response not reported", fixed = TRUE)
  expect_false(grepl("predicted response not reported",
                     nextgenCrossWorkbench:::ngcd_exec_summary_html(both), fixed = TRUE))
})

test_that("economic_index still refuses without P", {
  cov_e2e_guards()
  dir_file <- cov_e2e_direction(list(economic_weight = c(2, 1)))
  res <- cov_e2e_run(cov_e2e_cfg("economic_index", dir_file, P = NULL, G = cov_e2e_G()))
  expect_false(isTRUE(res$ok))
  expect_match(paste(c(res$error_message, res$.log), collapse = " "),
               "requires both phenotypic_covariance", fixed = TRUE)
})
