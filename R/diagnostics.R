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
      sprintf("Increase 'K max' on the Allocation screen (e.g. to %d) and re-run. If K keeps landing at the top, your parents support many useful crosses.",
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
        "Raise 'Max pairwise kinship' on Allocation to admit higher-merit related crosses, or lower it to force more diversity.")))
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
      "Enable 'Robust posterior allocation' on Allocation (it turns on posterior prediction). If scores were empty, raise the posterior draws (n_iter) on the Advanced screen.")))
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
      sprintf("Down-weight or drop '%s' on the Objective screen, or raise 'Min marker-effect reliability' (currently %s) to exclude unreliable traits automatically.", tr, ngcd_diag_num(floor, 2)))))
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
      "Review the 'QC audit' tab; consider the missingness / MAF filters and the residual-heterozygosity exclusion on the QC screen.")))
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
    ngcd_diag_robust(res),
    ngcd_diag_reliability(res),
    ngcd_diag_qc(res),
    ngcd_diag_poly(res))
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
