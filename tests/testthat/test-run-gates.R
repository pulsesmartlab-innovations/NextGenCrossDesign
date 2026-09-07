# Guards for four backend contracts the app used to violate from the UI. Each one
# was a hard backend error a breeder could reach in a click or two, so each gets a
# regression test at the level the fix lives at: what build_params() sends, what the
# dropdown offers, and what the run gate refuses.
library(shiny)

gate_cfg <- function() nextgenCrossWorkbench:::ngcd_load_config(tempfile("wbgate"))
gate_srv <- function() nextgenCrossWorkbench:::workbench_server(gate_cfg())

# ---------------------------------------------------------------------------
# 1. mate_relatedness supersedes the raw lambda_mating.
#
# The backend refuses both at once ("Set per-cross relatedness via EITHER
# mate_relatedness OR the raw lambda_mating / lambda_progeny_inbreeding, not
# both"), and mate_relatedness is the unified control that exists to prevent
# exactly that stacking -- so it must win, silently to the backend and visibly in
# the UI.
# ---------------------------------------------------------------------------

test_that("build_params: any mate_relatedness behaviour suppresses lambda_mating", {
  testServer(gate_srv(), {
    do.call(session$setInputs, demo_inputs(lambda_mating = 0.05))

    session$setInputs(mate_relatedness = "off")
    p <- build_params()
    expect_true("lambda_mating" %in% names(p))
    expect_equal(p$lambda_mating, 0.05)          # raw lambda in force when Off

    for (behaviour in c("avoid_inbreeding", "favor_complementarity")) {
      session$setInputs(mate_relatedness = behaviour)
      p <- build_params()
      expect_null(p$lambda_mating)               # unified control wins
      expect_equal(p$mate_relatedness, behaviour)
    }
  })
})

test_that("build_params: a NULL lambda_mating is dropped from the written config, not sent as null", {
  # ngcd_write_config() drops NULL/zero-length entries, so suppressing the lambda
  # really does remove the key the backend guard keys on.
  p <- list(mate_relatedness = "avoid_inbreeding", lambda_mating = NULL, lambda_group = 0.05)
  f <- tempfile(fileext = ".json")
  nextgenCrossWorkbench:::ngcd_write_config(p, f)
  written <- jsonlite::fromJSON(f, simplifyVector = TRUE)
  expect_false("lambda_mating" %in% names(written))
  expect_equal(written$mate_relatedness, "avoid_inbreeding")
})

test_that("the raw lambda_mating input is shown only while mate_relatedness is Off", {
  html <- suppressWarnings(as.character(nextgenCrossWorkbench:::workbench_ui(gate_cfg(), dev = FALSE)))
  html <- paste(html, collapse = " ")
  # The input still exists (it is the escape hatch when the unified control is Off)
  expect_match(html, "lambda_mating", fixed = TRUE)
  # ...but it is wrapped in a conditionalPanel keyed on mate_relatedness, and the
  # replacement note explains why it is inactive rather than leaving a live-looking
  # number that is not sent.
  expect_match(html, "input.mate_relatedness ==", fixed = TRUE)   # quotes are HTML-escaped
  expect_match(html, "superseded by the Mate-relatedness", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# 2. Multi-trait methods: the two REAL selection indices are offered again; the
#    one the backend rejects outright still is not.
#
# economic_index (Smith-Hazel) and desired_gain (Pesek-Baker) were dropped from
# the dropdown while the app had no way to collect the P and G covariance
# matrices they solve from -- every run with either was a guaranteed hard error.
# Backend 0.27.0 + the Data screen's covariance-matrix import card supply them,
# so both are offered again, and stay offered whatever is loaded: what changes
# when a matrix is missing is the note under the dropdown and the run gate, which
# NAME the missing matrix rather than hiding a method the breeder asked for.
#
# `threshold` remains dropped: the backend capability registry declares it, but
# ng_breeder_selection_objective() rejects it -- "method must be one of: auto,
# weighted, economic_index, desired_gain".
# ---------------------------------------------------------------------------

test_that("the multi-trait dropdown offers both formal selection indices, but never `threshold`", {
  cfg <- gate_cfg()
  ch <- nextgenCrossWorkbench:::ngcd_control_choices(
    cfg$backend_registry, "multi_trait_method",
    c("Automatic" = "auto", "Relative weights" = "weighted",
      "Economic index (Smith-Hazel)" = "economic_index",
      "Desired gains (Pesek-Baker)" = "desired_gain"),
    drop = c("threshold"))
  expect_true(all(c("auto", "weighted", "economic_index", "desired_gain") %in% unname(ch)))
  expect_false("threshold" %in% unname(ch))

  # ...and the rendered app really offers them: the <option>s carry both values.
  html <- paste(suppressWarnings(as.character(
    nextgenCrossWorkbench:::workbench_ui(cfg, dev = FALSE))), collapse = " ")
  block <- regmatches(html, regexpr("id=\"multi_trait_method\".*?</select>", html))
  expect_length(block, 1L)
  expect_match(block, "auto", fixed = TRUE)
  expect_match(block, "weighted", fixed = TRUE)
  expect_match(block, "economic_index", fixed = TRUE)
  expect_match(block, "desired_gain", fixed = TRUE)
  expect_false(grepl(">threshold<", block, fixed = TRUE))
  # The help hint that claimed the values came from the trait-direction file alone
  # was false (they need P and G too) and is still gone.
  expect_false(grepl("desired-gain VALUES are read from", html, fixed = TRUE))
})

test_that("ngcd_control_choices drop survives the registry merge", {
  # The registry can ADD choices the fallback lacks, so dropping a value from the
  # fallback alone would let it back in through the merge. `drop` must beat both.
  cc <- nextgenCrossWorkbench:::ngcd_control_choices
  reg <- list(controls = list(list(id = "m", choices = list(
    list(value = "auto", label = "Auto"),
    list(value = "economic_index", label = "Economic index"),
    list(value = "threshold", label = "Threshold")))))
  out <- cc(reg, "m", c(Auto = "auto", `Economic index` = "economic_index"),
            drop = c("economic_index", "threshold"))
  expect_equal(unname(out), "auto")
  # no drop list -> unchanged behaviour (the merge still appends registry extras)
  out2 <- cc(reg, "m", c(Auto = "auto"))
  expect_true(all(c("auto", "economic_index", "threshold") %in% unname(out2)))
})

# ---------------------------------------------------------------------------
# 3. A finite budget requires a cost column.
# ---------------------------------------------------------------------------

test_that("ngcd_budget_cost_message fires only for a finite budget with no cost column", {
  m <- nextgenCrossWorkbench:::ngcd_budget_cost_message
  expect_null(m(NA, ""))            # no budget typed
  expect_null(m(NULL, ""))
  expect_null(m(Inf, ""))           # "no cap" is not a finite budget
  expect_null(m(1e6, "cost"))       # cost column chosen -> supported
  msg <- m(1e6, "")
  expect_false(is.null(msg))
  expect_match(msg, "cost column", fixed = TRUE)
  expect_match(msg, "1000000", fixed = TRUE) # the breeder's own number is named, not in e-notation
  expect_null(m(1e6, "  cost  "))            # whitespace is not a column name...
  expect_false(is.null(m(1e6, "   ")))       # ...but blank still blocks
})

test_that("build_params omits budget with no cost column, and the run gate says why", {
  testServer(gate_srv(), {
    do.call(session$setInputs, demo_inputs(budget = 1e6))
    p <- build_params()
    expect_null(p$budget)                    # never reaches the backend guard
    expect_null(p$cost_col)
    expect_equal(p$lambda_cost, 0)           # cost/logistic lambdas are safe alone
    msg <- check_unsupported_combo_message()
    expect_false(is.null(msg))               # ...but the breeder is told, not ignored
    expect_match(msg, "budget cap", fixed = TRUE)

    session$setInputs(budget = NA)
    expect_null(check_unsupported_combo_message())
  })
})

test_that("the budget gate is scoped to the workflow that sends a budget", {
  testServer(gate_srv(), {
    do.call(session$setInputs, demo_inputs(budget = 1e6))
    expect_false(is.null(check_unsupported_combo_message()))
    # The polyploid and subgenome configs carry no budget at all, so a stale value
    # left in the (always-registered) input must not block them.
    session$setInputs(workflow = "polyploid")
    expect_null(check_unsupported_combo_message())
    session$setInputs(workflow = "subgenome")
    expect_null(check_unsupported_combo_message())
  })
})

# ---------------------------------------------------------------------------
# 4. Polyploid dominance is refused without the experimental acknowledgement.
# ---------------------------------------------------------------------------

test_that("ngcd_experimental_dominance_message explains the shared ridge penalty in breeder terms", {
  m <- nextgenCrossWorkbench:::ngcd_experimental_dominance_message
  expect_null(m(FALSE, FALSE))     # dominance off -> nothing to say
  expect_null(m(FALSE, TRUE))
  expect_null(m(TRUE, TRUE))       # acknowledged -> allowed
  msg <- m(TRUE, FALSE)
  expect_false(is.null(msg))
  expect_match(msg, "experimental", fixed = TRUE)
  expect_match(msg, "single ", fixed = TRUE)          # ...a single ridge penalty
  expect_match(msg, "ridge penalty", fixed = TRUE)
  expect_match(msg, "not reliable", fixed = TRUE)
})

test_that("polyploid dominance: refused without the opt-in, flag forwarded with it, absent when off", {
  testServer(gate_srv(), {
    do.call(session$setInputs, demo_inputs())
    session$setInputs(workflow = "polyploid", poly_trait_col = "yield",
                      poly_gain = "usefulness", poly_grm_method = "vanraden",
                      poly_double_reduction = 0, poly_run_qc = TRUE)

    # dominance off -> no experimental flag, no refusal
    session$setInputs(poly_dominance = FALSE, poly_allow_experimental_dominance = FALSE)
    p <- build_poly_params()
    expect_false(isTRUE(p$dominance))
    expect_false(isTRUE(p$allow_experimental_dominance))
    expect_null(check_unsupported_combo_message())

    # dominance on, opt-in off -> refused (NOT silently demoted to additive)
    session$setInputs(poly_dominance = TRUE)
    p <- build_poly_params()
    expect_true(isTRUE(p$dominance))          # the request is not rewritten...
    expect_false(isTRUE(p$allow_experimental_dominance))
    msg <- check_unsupported_combo_message()
    expect_false(is.null(msg))                # ...the run is stopped instead
    expect_match(msg, "experimental", fixed = TRUE)

    # opt-in ticked -> flag forwarded, run allowed
    session$setInputs(poly_allow_experimental_dominance = TRUE)
    p <- build_poly_params()
    expect_true(isTRUE(p$allow_experimental_dominance))
    expect_null(check_unsupported_combo_message())
  })
})

test_that("the dominance gate does not block a standard diploid run", {
  testServer(gate_srv(), {
    do.call(session$setInputs, demo_inputs())
    # poly_dominance lives in a conditionalPanel, so its value persists after the
    # breeder switches back to the standard workflow; it must not block there.
    session$setInputs(workflow = "standard", poly_dominance = TRUE,
                      poly_allow_experimental_dominance = FALSE)
    expect_null(check_unsupported_combo_message())
  })
})

test_that("the runner forwards allow_experimental_dominance only when the config sets it", {
  # poly_design_args() reads the polyploid genotype through the backend, so it
  # needs nextgenCrossDesign on the library path. CI checks the frontend alone.
  skip_if(!backend_available(), backend_skip_reason())
  e <- ngcd_runner_env()
  gf <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(NAME = c("A", "B", "C"), M1 = c(0, 2, 4), M2 = c(4, 2, 0)),
                   gf, row.names = FALSE)
  base <- list(genotype_file = gf, genotype_id_col = "NAME", ploidy = 4, dominance = TRUE)

  a_off <- e$poly_design_args(base)
  expect_true(isTRUE(a_off$dominance))
  expect_false(isTRUE(a_off$allow_experimental_dominance))

  a_on <- e$poly_design_args(c(base, list(allow_experimental_dominance = TRUE)))
  expect_true(isTRUE(a_on$allow_experimental_dominance))

  # and it is a real formal of the backend entry point, not a silently ignored key
  skip_if_not(requireNamespace("nextgenCrossDesign", quietly = TRUE), "Backend not installed.")
  expect_true("allow_experimental_dominance" %in%
                names(formals(nextgenCrossDesign::ng_polyploid_design_crosses)))
})
