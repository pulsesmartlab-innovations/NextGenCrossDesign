# ===========================================================================
# diagnostics.R  -  turn a run result into actionable "why + what to change"
# ===========================================================================
# Every procedure (automatic cross-number sweep, mate allocation, robust
# posterior re-optimization, trait reliability, QC, polyploid design) can leave
# a plan looking "off". These helpers inspect the result and explain WHY the
# procedure produced what it did, and WHICH parameter to change to steer it.
#
# ngcd_diagnostics(res) returns a list of items:
#   list(area, severity, title, detail, suggestion)
# severity is one of "ok" (green, informational), "note" (worth knowing), or
# "warn" (the result is likely not what you want; act on it).
# (`%||%` is provided package-wide by config.R.)

ngcd_diag_num <- function(x, d = 3) {
  if (is.null(x) || length(x) == 0L || !is.finite(suppressWarnings(as.numeric(x[1]))))
    return("--")
  formatC(as.numeric(x[1]), format = "f", digits = d)
}
ngcd_diag_pct <- function(x, d = 0) {
  if (is.null(x) || !is.finite(suppressWarnings(as.numeric(x[1])))) return("--")
  paste0(formatC(100 * as.numeric(x[1]), format = "f", digits = d), "%")
}

ngcd_diag_item <- function(area, severity, title, detail, suggestion = NULL)
  list(area = area, severity = severity, title = title,
       detail = detail, suggestion = suggestion)

# -- automatic cross-number sweep -------------------------------------------
ngcd_diag_cross_number <- function(res) {
  sw <- res$cross_number_sweep
  if (is.null(sw) || is.null(sw$recommended_k)) return(list())
  out <- list()
  K   <- as.integer(sw$recommended_k)
  kr  <- sw$k_range
  crit <- sw$criterion %||% "elbow_relative"
  cur <- sw$curve
  kmin <- if (length(kr)) min(kr) else NA_integer_
  kmax <- if (length(kr)) max(kr) else NA_integer_
  crit_lab <- c(elbow_relative = "diminishing-returns (relative marginal gain)",
                elbow_kneedle  = "diminishing-returns (kneedle)",
                ne_target      = "effective-population-size floor",
                coancestry_budget = "coancestry budget")[[crit]] %||% crit

  # 1. boundary diagnosis - the #1 reason a recommendation looks wrong
  if (!is.na(kmax) && K >= kmax) {
    out <- c(out, list(ngcd_diag_item("cross_number", "warn",
      sprintf("Recommended K = %d is at the TOP of your swept range (%d-%d)", K, kmin, kmax),
      paste0("The ", crit_lab, " rule never crossed its stopping point inside the range, so the ",
             "true optimum is probably larger than ", kmax, ". The recommendation is capped by the ",
             "range, not by the data."),
      sprintf("Increase 'K max' on the Mate allocation screen (e.g. to %d) and re-run. If K keeps landing at the top, your parents support many useful crosses.",
              as.integer(kmax * 2)))))
  } else if (!is.na(kmin) && K <= kmin) {
    out <- c(out, list(ngcd_diag_item("cross_number", "note",
      sprintf("Recommended K = %d is at the BOTTOM of your swept range (%d-%d)", K, kmin, kmax),
      paste0("The rule saturates almost immediately - extra crosses add little. That can be real ",
             "(few complementary parents) or an artifact of too high a threshold."),
      "Lower 'K min' to explore smaller plans, or lower the recommendation threshold to keep more crosses.")))
  } else {
    # interior elbow: quantify how flat it is beyond K
    relnext <- NA_real_
    if (is.data.frame(cur) && all(c("K", "relative_marginal") %in% names(cur))) {
      nxt <- cur$relative_marginal[cur$K == (K + 1L)]
      if (length(nxt)) relnext <- suppressWarnings(as.numeric(nxt[1]))
    }
    out <- c(out, list(ngcd_diag_item("cross_number", "ok",
      sprintf("Recommended K = %d is a stable interior elbow (range %d-%d)", K, kmin, kmax),
      if (is.finite(relnext))
        sprintf("Beyond K = %d each extra cross adds only %s of the first cross's marginal gain (the %s rule).",
                K, ngcd_diag_pct(relnext), crit_lab)
      else sprintf("The %s rule found a clear elbow inside your range.", crit_lab),
      "To bias toward more gain (more crosses) lower the threshold; for a leaner plan, raise it.")))
  }

  # 2. what you gave up vs the largest plan, on gain and diversity
  if (is.data.frame(cur) && all(c("K", "total_gain") %in% names(cur)) && !is.na(kmax)) {
    tg  <- cur$total_gain[cur$K == K]
    tgm <- cur$total_gain[cur$K == kmax]
    if (length(tg) && length(tgm) && is.finite(tg) && is.finite(tgm) && tgm > 0 && K < kmax) {
      gc  <- if ("group_coancestry" %in% names(cur)) cur$group_coancestry[cur$K == K] else NA
      gcm <- if ("group_coancestry" %in% names(cur)) cur$group_coancestry[cur$K == kmax] else NA
      msg <- sprintf("Choosing K = %d instead of the largest plan (K = %d) gives up %s of total gain",
                     K, kmax, ngcd_diag_pct((tgm - tg) / tgm))
      if (length(gc) && length(gcm) && is.finite(gc) && is.finite(gcm))
        msg <- paste0(msg, sprintf(" while changing group coancestry from %s to %s (lower = more diverse)",
                                   ngcd_diag_num(gcm), ngcd_diag_num(gc)))
      out <- c(out, list(ngcd_diag_item("cross_number", "note",
        "Trade-off at the recommended K", paste0(msg, "."),
        "If that lost gain matters more than diversity, raise K (or switch to a fixed number of crosses).")))
    }
  }

  # 3. criterion-specific steering
  if (crit == "ne_target" && is.data.frame(cur) && "Ne_estimate" %in% names(cur)) {
    ne <- cur$Ne_estimate[cur$K == K]
    if (length(ne) && is.finite(ne))
      out <- c(out, list(ngcd_diag_item("cross_number", "note",
        sprintf("Effective population size at K = %d is Ne ~= %s", K, ngcd_diag_num(ne, 1)),
        "The number of crosses was chosen to keep Ne above your floor.",
        "Raise 'Minimum Ne' to force a larger, more diverse plan; lower it to allow fewer crosses.")))
  }
  out
}

# -- binding allocation constraints -----------------------------------------
ngcd_diag_allocation <- function(res) {
  ps <- res$plan_summary %||% list(); st <- res$settings %||% list()
  sc <- res$selected_crosses
  out <- list()

  # pairwise-kinship cap binding?
  cap <- suppressWarnings(as.numeric(st$max_pair_kinship %||% NA))
  if (is.finite(cap) && is.data.frame(sc) && "pair_kinship" %in% names(sc) && nrow(sc)) {
    mx <- suppressWarnings(max(sc$pair_kinship, na.rm = TRUE))
    if (is.finite(mx) && mx >= 0.98 * cap)
      out <- c(out, list(ngcd_diag_item("allocation", "warn",
        sprintf("The pairwise-kinship cap is binding (max_pair_kinship = %s)", ngcd_diag_num(cap)),
        sprintf("At least one selected cross sits right at the cap (max pair kinship in the plan = %s). Higher-merit but more-related crosses were excluded.", ngcd_diag_num(mx)),
        "Raise 'Max pairwise kinship' on Mate allocation to admit higher-merit related crosses, or lower it to force more diversity.")))
  }

  # parent-use cap binding?
  mu  <- suppressWarnings(as.integer(st$max_crosses_per_parent %||% NA))
  mpu <- suppressWarnings(as.integer(ps$max_parent_use %||% NA))
  if (is.finite(mu) && is.finite(mpu) && mpu >= mu && mu > 0)
    out <- c(out, list(ngcd_diag_item("allocation", "note",
      sprintf("The parent-use cap is binding (max uses per parent = %d)", mu),
      sprintf("At least one parent is used the maximum %d times - a few elite parents are carrying the plan.", mu),
      "Raise 'Max uses per parent' to concentrate gain on elite parents, or lower it to spread the plan across more parents (more diversity).")))

  # minimum-unique-parents binding?
  mup <- suppressWarnings(as.integer(st$min_unique_parents %||% NA))
  up  <- suppressWarnings(as.integer(ps$unique_parents %||% NA))
  if (is.finite(mup) && is.finite(up) && up <= mup && mup > 0)
    out <- c(out, list(ngcd_diag_item("allocation", "note",
      sprintf("The minimum-unique-parents floor is binding (min = %d)", mup),
      sprintf("The plan uses exactly %d unique parents - the floor forced in parents that merit alone would not have selected.", up),
      "Lower 'Minimum unique parents' to let the optimizer concentrate on the best parents, or keep it for a broader genetic base.")))
  out
}

# -- robust posterior re-optimization ---------------------------------------
ngcd_diag_robust <- function(res) {
  rp <- res$robust_plan
  if (is.null(rp)) return(list())
  if (!is.null(rp$error))
    return(list(ngcd_diag_item("robust", "warn",
      "Robust posterior allocation could not be computed", rp$error,
      "Enable 'Robust posterior allocation' on Mate allocation (it turns on posterior prediction). If scores were empty, raise the posterior draws (n_iter) in Prediction & scoring.")))
  nch <- suppressWarnings(as.integer(rp$n_changed %||% NA))
  nk  <- suppressWarnings(as.integer(rp$n_crosses %||% NA))
  sm  <- rp$summary %||% list()
  q   <- sm$robustness_quantile %||% NA
  if (is.finite(nch) && nch == 0L)
    return(list(ngcd_diag_item("robust", "ok",
      "Robust and point-estimate plans agree",
      sprintf("All %s crosses are unchanged under the pessimistic posterior objective - your ranking is stable under marker-effect uncertainty.", nk %||% "?"),
      NULL)))
  if (is.finite(nch) && nch > 0L)
    return(list(ngcd_diag_item("robust", "note",
      sprintf("%d of %s crosses changed under the robust objective", nch, nk %||% "?"),
      sprintf("Those crosses' rankings are sensitive to prediction uncertainty (robustness quantile = %s). The point-estimate plan is optimistic for them.",
              ngcd_diag_num(q, 2)),
      "Move the robustness quantile toward 0.5 for a middle ground between optimistic and pessimistic; gather more training data to sharpen the posteriors; or trust the robust plan if you want a safe choice.")))
  list()
}

# -- trait model reliability -------------------------------------------------
ngcd_diag_reliability <- function(res) {
  es <- res$effect_summary
  if (!is.data.frame(es) || !"marker_effect_reliability" %in% names(es) || !nrow(es)) return(list())
  st <- res$settings %||% list()
  floor <- suppressWarnings(as.numeric(st$min_effect_reliability %||% 0.35))
  out <- list()
  rel <- suppressWarnings(as.numeric(es$marker_effect_reliability))
  low <- which(is.finite(rel) & rel < max(0.2, floor * 0.6))
  for (i in low) {
    tr <- es$trait[i]
    out <- c(out, list(ngcd_diag_item("reliability", "warn",
      sprintf("Trait '%s' has very low marker-effect reliability (%s)", tr, ngcd_diag_num(rel[i], 3)),
      "Its predicted cross values are mostly noise, which can dominate a multi-trait score and distort the plan.",
      sprintf("Down-weight or drop '%s' in Selection objective, or raise 'Min marker-effect reliability' (currently %s) to exclude unreliable traits automatically.", tr, ngcd_diag_num(floor, 2)))))
  }
  out
}

# -- QC signals --------------------------------------------------------------
ngcd_diag_qc <- function(res) {
  qc <- res$qc %||% list(); cn <- qc$counts %||% list()
  out <- list()
  blockers <- suppressWarnings(as.integer(cn$blockers %||% 0))
  warns    <- suppressWarnings(as.integer(cn$warnings %||% 0))
  if (is.finite(blockers) && blockers > 0)
    out <- c(out, list(ngcd_diag_item("qc", "warn",
      sprintf("Data QC reported %d blocker(s)", blockers),
      "Blocking QC issues can make the run fall back to defaults or drop data.",
      "Open the 'QC audit' tab, resolve the flagged items (duplicates, missingness, heterozygosity), and re-run.")))
  else if (is.finite(warns) && warns > 0)
    out <- c(out, list(ngcd_diag_item("qc", "note",
      sprintf("Data QC reported %d warning(s)", warns),
      "The run proceeded, but flagged data-quality warnings may bias the plan.",
      "Review the 'QC audit' tab; consider the missingness / MAF filters and the residual-heterozygosity exclusion in Data quality.")))
  out
}

# -- polyploid design --------------------------------------------------------
ngcd_diag_poly <- function(res) {
  if (!isTRUE(res$poly_design)) return(list())
  pp <- res$poly_plan %||% list(); qc <- pp$qc %||% list()
  out <- list()
  drop_m <- suppressWarnings(as.integer(qc$markers_dropped %||% qc$monomorphic_dropped %||% 0))
  if (is.finite(drop_m) && drop_m > 0)
    out <- c(out, list(ngcd_diag_item("polyploid", "note",
      sprintf("%d marker(s) were dropped in ploidy-aware QC", drop_m),
      "Monomorphic or low-quality markers were removed before dosage scoring.",
      "This is usually fine; if too many drop, check the dosage coding matches the selected ploidy on the Data screen.")))
  out
}

# -- breeder mating constraints (min-use / group quota / disallowed pairings /
#    committed crosses / min-unique relaxation) --------------------------------
# These reshape the delivered plan silently at the backend; surface what they did.
ngcd_diag_constraints <- function(res) {
  cd <- res$constraint_diagnostics
  if (is.null(cd)) return(list())
  out <- list()
  req <- suppressWarnings(as.integer(cd$n_crosses_requested %||% NA))
  got <- suppressWarnings(as.integer(cd$n_crosses_delivered %||% NA))
  ra   <- suppressWarnings(as.integer(cd$min_unique_relaxation_attempts %||% 0))
  mreq <- suppressWarnings(as.integer(cd$min_unique_requested %||% NA))
  mused <- suppressWarnings(as.integer(cd$min_unique_used %||% NA))
  nc  <- suppressWarnings(as.integer(cd$n_committed %||% 0))
  mif <- suppressWarnings(as.integer(cd$min_use_if_used %||% NA))
  any_constraint <- isTRUE(cd$group_quota_active) || isTRUE(cd$group_permission_active) ||
    isTRUE(cd$budget_active) || (is.finite(nc) && nc > 0) ||
    (is.finite(mif) && mif > 1) || (is.finite(ra) && ra > 0)

  # plan delivered fewer crosses than requested
  if (is.finite(req) && is.finite(got) && got < req) {
    if (isTRUE(any_constraint))
      out <- c(out, list(ngcd_diag_item("constraints", "warn",
        sprintf("Breeder constraints shrank the plan to %d of %d requested crosses", got, req),
        "A binding constraint (min-use-if-used, group quota, disallowed group pairing, or budget) removed crosses that would otherwise have been selected, so fewer were delivered than requested.",
        "To recover the full count, relax the binding constraint: raise the group quota, allow the disallowed pairing, lower 'min uses if used', or increase the budget. If the smaller plan is acceptable, no action is needed.")))
    else
      out <- c(out, list(ngcd_diag_item("constraints", "note",
        sprintf("Only %d of %d requested crosses could be formed", got, req),
        "Not enough distinct candidate crosses satisfied the per-parent cap to reach the requested number - this is candidate scarcity, not a breeder constraint.",
        "Add more candidate parents, or raise 'Max uses per parent' so existing parents can appear in more crosses.")))
  }

  # minimum-unique-parents auto-relaxed to stay feasible
  if (is.finite(ra) && ra > 0 && is.finite(mreq) && is.finite(mused) && mused < mreq)
    out <- c(out, list(ngcd_diag_item("constraints", "warn",
      sprintf("Minimum-unique-parents was relaxed from %d to %d to stay feasible", mreq, mused),
      "The requested parent-diversity floor could not be met alongside the other constraints (per-parent cap, cross count), so the allocator lowered it automatically instead of failing.",
      "Raise 'Max uses per parent' or add candidate parents so the floor fits, or set 'Minimum unique parents' to the value actually used.")))

  # committed crosses force-included
  if (is.finite(nc) && nc > 0)
    out <- c(out, list(ngcd_diag_item("constraints", "note",
      sprintf("%d cross(es) were force-included as committed matings", nc),
      "You locked these crosses in; the optimizer filled the rest of the plan around them, which can displace higher-merit unconstrained crosses.",
      "Remove entries from 'Committed crosses' to give the optimizer full freedom, or keep them if they are agronomic must-dos.")))

  # min-use-if-used active
  if (is.finite(mif) && mif > 1)
    out <- c(out, list(ngcd_diag_item("constraints", "note",
      sprintf("Min-use-if-used is active - any used parent appears at least %d times", mif),
      "Every parent that enters the plan is used at least this many times to justify producing/maintaining it, which concentrates the plan onto fewer distinct parents.",
      "Lower 'min uses if used' to admit parents used only once, or keep it to reduce the number of parents you must manage.")))
  out
}

# -- marker steering & lethal-allele guarding (Module 4) ---------------------
ngcd_diag_marker_lethal <- function(res) {
  cd <- res$constraint_diagnostics
  if (is.null(cd)) return(list())
  out <- list()

  # lethal-allele guarding
  if (isTRUE(cd$lethal_active)) {
    dropped <- suppressWarnings(as.integer(cd$lethal_dropped %||% 0))
    nloci   <- suppressWarnings(as.integer(cd$lethal_n_loci %||% 0))
    pre     <- suppressWarnings(as.integer(cd$lethal_candidates_pre %||% NA))
    if (is.finite(dropped) && dropped > 0 && isTRUE(cd$lethal_dropped_from_plan)) {
      pctxt <- if (is.finite(pre) && pre > 0)
        sprintf(" (%s of %d candidate crosses)", ngcd_diag_pct(dropped / pre), pre) else ""
      out <- c(out, list(ngcd_diag_item("marker_guard", "note",
        sprintf("Lethal-allele guarding removed %d carrier x carrier cross(es)%s", dropped, pctxt),
        sprintf("At the %d nominated deleterious-recessive locus/loci, matings where BOTH parents carry the risk allele were dropped before allocation, so no plan cross can segregate the lethal genotype.", nloci),
        "This is protective and usually desired. To keep those crosses for inspection, turn off 'drop lethal-carrier crosses' - they are then flagged but retained.")))
    } else if (is.finite(dropped) && dropped > 0) {
      out <- c(out, list(ngcd_diag_item("marker_guard", "warn",
        sprintf("Lethal-allele guarding flagged %d carrier x carrier cross(es) but kept them", dropped),
        sprintf("Both parents carry the risk allele at %d nominated locus/loci; the crosses were flagged but NOT removed because dropping is turned off, so the plan may include lethal-segregating matings.", nloci),
        "Turn on 'drop lethal-carrier crosses' to exclude them automatically.")))
    } else {
      out <- c(out, list(ngcd_diag_item("marker_guard", "ok",
        sprintf("Lethal-allele guarding active on %d locus/loci - no carrier x carrier crosses found", nloci),
        "None of your candidate crosses mate two carriers of a nominated risk allele, so nothing had to be removed.",
        NULL)))
    }
  }

  # marker steering
  if (isTRUE(cd$marker_steering_active)) {
    nloci <- suppressWarnings(as.integer(cd$marker_n_loci %||% 0))
    lam   <- suppressWarnings(as.numeric(cd$marker_lambda %||% 0))
    if (isTRUE(cd$marker_blended) && is.finite(lam) && lam != 0)
      out <- c(out, list(ngcd_diag_item("marker_guard", "note",
        sprintf("Marker steering is driving allocation (%d target locus/loci, weight = %s)", nloci, ngcd_diag_num(lam, 2)),
        "The plan optimizes a blend of predicted merit and progress toward your target-allele frequencies, so some merit is traded for allele-frequency movement at the steered loci.",
        "Lower the steering weight to prioritize merit, raise it to push the target alleles harder, or set it to 0 to rank on merit alone (targets are still reported).")))
    else
      out <- c(out, list(ngcd_diag_item("marker_guard", "note",
        sprintf("Marker-target progress is reported for %d locus/loci (steering weight 0)", nloci),
        "Per-cross target-allele progress is shown, but the target is NOT influencing which crosses are selected because the steering weight is zero.",
        "Set a positive steering weight to make allocation actively pursue the target frequencies.")))
  }
  out
}

# -- cost & logistics --------------------------------------------------------
ngcd_diag_cost <- function(res) {
  cd <- res$constraint_diagnostics
  if (is.null(cd)) return(list())
  out <- list()
  if (isTRUE(cd$budget_active)) {
    budget <- suppressWarnings(as.numeric(cd$budget %||% NA))
    spent  <- suppressWarnings(as.numeric(cd$plan_total_cost %||% NA))
    if (is.finite(budget) && is.finite(spent) && budget > 0) {
      if (spent >= 0.98 * budget)
        out <- c(out, list(ngcd_diag_item("cost", "warn",
          sprintf("The budget is binding (plan cost %s of %s)", ngcd_diag_num(spent, 2), ngcd_diag_num(budget, 2)),
          "The plan spends essentially the whole budget; higher-merit but more expensive crosses were left out to stay within it.",
          "Raise 'Budget' to admit more/costlier crosses, or reduce per-cross costs. Lower it to spend less, at some merit cost.")))
      else
        out <- c(out, list(ngcd_diag_item("cost", "ok",
          sprintf("The plan is within budget (cost %s of %s)", ngcd_diag_num(spent, 2), ngcd_diag_num(budget, 2)),
          "The budget was not the binding constraint - the chosen plan cost less than the cap.",
          NULL)))
    }
  }
  lam_c <- suppressWarnings(as.numeric(cd$cost_emphasis %||% 0))
  if (is.finite(lam_c) && lam_c != 0)
    out <- c(out, list(ngcd_diag_item("cost", "note",
      sprintf("Cost is penalized in the objective (weight = %s)", ngcd_diag_num(lam_c, 2)),
      "Mate allocation trades predicted merit against per-cross cost, so cheaper crosses are favored even at slightly lower merit.",
      "Lower the cost weight to prioritize merit, or raise it to lean harder on cheaper crosses.")))
  lam_l <- suppressWarnings(as.numeric(cd$logistic_emphasis %||% 0))
  if (is.finite(lam_l) && lam_l != 0)
    out <- c(out, list(ngcd_diag_item("cost", "note",
      sprintf("Logistics are penalized in the objective (weight = %s)", ngcd_diag_num(lam_l, 2)),
      "A per-cross logistic burden is traded against merit, so operationally easier crosses are favored.",
      "Lower the logistic weight to prioritize merit, or raise it to favor easier crosses.")))
  out
}

# -- risk / portfolio (backend 0.13.0; single-trait only) -------------------
ngcd_diag_priority_risk <- function(res) {
  sc <- res$selected_crosses
  if (!is.data.frame(sc) || !all(c("risk_bin", "priority_tier") %in% names(sc))) return(list())
  out <- list()
  top <- levels(sc$priority_tier)[[1L]]
  in_top <- as.character(sc$priority_tier) == top
  hi <- as.character(sc$risk_bin) == "high"
  k <- sum(in_top & hi, na.rm = TRUE); n <- sum(in_top, na.rm = TRUE)
  if (n > 0 && k > 0)
    out <- c(out, list(ngcd_diag_item("priority_risk", if (k >= ceiling(n/2)) "warn" else "note",
      sprintf("%d of %d top-tier crosses rely on low-confidence predictions", k, n),
      "These crosses rank highly on the point estimate but their predictions are uncertain (high risk).",
      "Hedge with lower-risk 'workhorse' crosses, or enable posterior prediction to sharpen the estimates.")))
  cm <- res$priority_risk_diagnostics$confidence_method %||% (sc$confidence_method[[1L]] %||% NA)
  if (isTRUE(cm %in% c("midparent_pev_partial", "reliability_coarse_variance", "reliability")))
    out <- c(out, list(ngcd_diag_item("priority_risk", "note",
      "Risk is estimated from a partial/coarse signal",
      sprintf("confidence_method = '%s' — the variance-estimation part of risk is not fully captured.", cm),
      "Enable posterior prediction for a complete per-cross risk column.")))
  out
}

# -- portfolio profile (metric-aware guidance) ------------------------------
ngcd_diag_portfolio <- function(res) {
  sc <- res$selected_crosses
  if (!is.data.frame(sc) || !("portfolio_profile" %in% names(sc))) return(list())
  metric <- res$settings$trait_value_metric %||% "usefulness"
  n_up <- sum(as.character(sc$portfolio_profile) %in% c("breakthrough", "long_shot"), na.rm = TRUE)
  msg <- if (identical(metric, "mean"))
    sprintf("You scored on 'mean'; %d cross(es) carry within-family variance (upside) you did not rank.", n_up)
  else "The portfolio shows the genetic decomposition (level x within-family SD) of your plan (relative to this run)."
  list(ngcd_diag_item("portfolio", "note", "Portfolio view available",
    msg, "Open the 'Portfolio & risk' tab: x = genetic SD (upside), y = mean (level), colour = risk."))
}

# -- per-trait check-threshold veto (backend 0.14.0) ------------------------
ngcd_diag_trait_check <- function(res) {
  d <- res$trait_check_diagnostics
  if (is.null(d)) return(list())
  out <- list()
  nf <- suppressWarnings(as.integer(d$n_flagged %||% 0))
  nx <- suppressWarnings(as.integer(d$n_excluded %||% 0))
  ne <- suppressWarnings(as.integer(d$n_not_evaluable %||% 0))
  ntr <- if (is.data.frame(d$active)) nrow(d$active) else 0L
  if (is.finite(nf) && nf > 0)
    out <- c(out, list(ngcd_diag_item("trait_check", if (nx > 0) "warn" else "note",
      sprintf("%d cross(es) fail a per-trait check%s", nf,
              if (nx > 0) sprintf("; %d excluded from the plan", nx) else " (flagged, kept)"),
      sprintf("Their mid-parent is on the worse side of your check line for %d trait(s).", ntr),
      "Turn on 'exclude threshold violators' to drop them, or relax/remove the trait check.")))
  if (is.finite(ne) && ne > 0)
    out <- c(out, list(ngcd_diag_item("trait_check", "note",
      sprintf("%d trait-check comparison(s) were not evaluable", ne),
      "A parent or the check line had no value on the chosen basis (e.g. phenotype missing).",
      "Use a GEBV basis, or pick a check line/traits that are measured.")))
  out
}

# -- top-level assembler -----------------------------------------------------
#' Assemble actionable diagnostics for a run result.
#' @param res parsed backend result (ng_run_result.v1)
#' @return list of diagnostic items (area, severity, title, detail, suggestion)
#' @noRd
ngcd_diagnostics <- function(res) {
  if (is.null(res)) return(list())
  items <- c(
    ngcd_diag_cross_number(res),
    ngcd_diag_allocation(res),
    ngcd_diag_constraints(res),
    ngcd_diag_marker_lethal(res),
    ngcd_diag_cost(res),
    ngcd_diag_robust(res),
    ngcd_diag_reliability(res),
    ngcd_diag_qc(res),
    ngcd_diag_poly(res),
    ngcd_diag_priority_risk(res),
    ngcd_diag_portfolio(res),
    ngcd_diag_trait_check(res))
  # order: warn first, then note, then ok
  ord <- c(warn = 1L, note = 2L, ok = 3L)
  sev <- vapply(items, function(x) ord[[x$severity %||% "note"]] %||% 2L, integer(1))
  items[order(sev)]
}

# Render diagnostics as HTML (used by the report and, via shiny::HTML, the app).
ngcd_diagnostics_html <- function(res) {
  items <- ngcd_diagnostics(res)
  if (!length(items))
    return("<p class='cap'>No tuning diagnostics for this run - the procedures ran within normal ranges.</p>")
  badge <- c(ok = "OK", note = "NOTE", warn = "CHECK")
  colour <- c(ok = "#1f7a4d", note = "#8a6d1a", warn = "#a3341f")
  bg     <- c(ok = "#eef6f1", note = "#fdf7e6", warn = "#fbecea")
  blocks <- vapply(items, function(it) {
    sev <- it$severity %||% "note"
    sprintf(paste0("<div style='border-left:4px solid %s;background:%s;padding:8px 12px;",
                   "margin:8px 0;border-radius:4px'>",
                   "<b style='color:%s'>[%s]</b> <b>%s</b><div style='margin-top:3px'>%s</div>%s</div>"),
            colour[[sev]], bg[[sev]], colour[[sev]], badge[[sev]], it$title, it$detail,
            if (!is.null(it$suggestion))
              sprintf("<div style='margin-top:4px'><i>What to change:</i> %s</div>", it$suggestion)
            else "")
  }, character(1))
  paste0("<div class='ngcd-diagnostics'>", paste(blocks, collapse = "\n"), "</div>")
}
