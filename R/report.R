# ===========================================================================
# report.R  -  executive summary + figures for the run report (HTML + PDF)
#
#   * HTML report: interactive plotly figures, a cross-linked table of
#     contents (anchor nav + back-to-top on every figure), self-contained
#     (plotly.js inlined; falls back to static PNGs if plotly is unavailable).
#   * PDF report: static base-R figures.
#   * Both report builders and the in-app Report tab draw only the figures that
#     actually apply to the run (a figure with no data is omitted entirely -
#     no "no data" placeholders).
#   * The gain-diversity frontier is included whenever the run produced one.
# ===========================================================================

NGCD_TIER_COL <- c(highly_priority = "#00583d", priority = "#4a9c78",
                   medium_priority = "#e0a800", low_priority = "#c0562f")
NGCD_TIER_ORD <- c("highly_priority", "priority", "medium_priority", "low_priority")

# Formatted mid-parent GEBV (<trait>_mean_gebv) per cross (rows) x trait (cols),
# "--" where a trait has no GEBV column. Shared by the heatmap hover (plotly),
# the heatmap cell labels (PDF) and the scatter point labels (PDF).
ngcd_gebv_text_matrix <- function(sc, traits, digits = 2) {
  g <- vapply(traits, function(tr) {
    col <- paste0(tr, "_mean_gebv")
    if (col %in% names(sc)) suppressWarnings(as.numeric(sc[[col]])) else rep(NA_real_, nrow(sc))
  }, numeric(nrow(sc)))
  g <- matrix(g, nrow = nrow(sc))
  matrix(ifelse(is.finite(g), formatC(g, format = "f", digits = digits), "--"), nrow = nrow(sc))
}

# --- PulseSmartLab lab logo (Dr. Sikiru Atanda) ----------------------------
# Returns the inline <svg> markup so the report is self-contained. Falls back
# to the packaged file if present; otherwise a compact built-in copy.
ngcd_logo_svg <- function() {
  f <- system.file("app", "www", "pulsesmartlab-logo.svg",
                   package = "nextgenCrossWorkbench")
  if (nzchar(f) && file.exists(f)) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    # strip any XML prolog so it embeds cleanly inside HTML
    return(sub("^\\s*<\\?xml[^>]*\\?>", "", txt))
  }
  paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 980 300'>",
    "<text x='175' y='188' textLength='700' lengthAdjust='spacingAndGlyphs' ",
    "font-family='Georgia,serif' font-size='118' font-weight='700' fill='#16315B'>PulseSmartLab</text>",
    "<text x='525' y='270' text-anchor='middle' font-family='Arial,sans-serif' ",
    "font-size='37' font-weight='600' fill='#2E8B2E'>Pulse Breeding, Genetics &amp; Innovation</text></svg>")
}

# --- disomic-subgenome executive summary --------------------------------------
# The subgenome design path returns a poly-style result (plan_summary = n_crosses
# + mean_gain, subgenome-specific settings) that does not carry the standard
# metric / allocation / coancestry / priority-tier / QC fields, so the generic
# summary below would print placeholders for all of them. This branch writes an
# accurate summary from the fields the subgenome path actually reports.
ngcd_exec_summary_subgenome_html <- function(res, figs = NULL) {
  ps <- res$plan_summary %||% list(); st <- res$settings %||% list()
  sg <- res$subgenome %||% list(); sc <- res$selected_crosses
  fmt <- function(x, d = 3) if (is.null(x) || !is.finite(suppressWarnings(as.numeric(x[1])))) "&ndash;" else formatC(as.numeric(x[1]), format = "f", digits = d)
  cnt <- function(x) if (length(x) != 1L || is.na(suppressWarnings(as.numeric(x)))) "&ndash;" else as.character(x)
  n_sel <- if (is.data.frame(sc)) nrow(sc) else (ps$n_crosses %||% NA)
  sg_names <- unlist(sg$names)
  markers_txt <- if (!is.null(sg$markers_per_subgenome))
    paste(sprintf("%s=%s", names(sg$markers_per_subgenome), unlist(sg$markers_per_subgenome)), collapse = ", ") else "&ndash;"
  vmodel <- sg$variance_model %||% st$variance_model %||% "recombination_aware"
  vmodel_txt <- if (grepl("recomb", vmodel, ignore.case = TRUE))
    "recombination-aware (exact per-subgenome, summed)" else "an unlinked-loci approximation"
  grm <- st$grm_method %||% sg$grm_method %||% "vanraden"
  progeny_target <- sg$progeny_target %||% st$progeny_target %||% "DH"
  fig_caption <- if (!is.null(figs) && length(figs))
    paste0("<p class='cap'>Figures in this report: ",
           paste(vapply(figs, function(f) f$title, character(1)), collapse = ", "), ".</p>") else ""
  paste0(
    "<h2 id='summary'>Executive summary</h2>",
    "<p>This report summarises a genomic cross-prediction and mate-allocation run performed with the ",
    "<b>nextgenCrossDesign</b> backend (v", res$package_version %||% "?", "), using the ",
    "<b>disomic-subgenome</b> design path for a true allopolyploid with <b>", length(sg_names),
    "</b> subgenomes (", paste(sg_names, collapse = ", "), "). Each subgenome is inherited diploidly and is ",
    "QC'd, effect-fit and scored as a diploid; markers per subgenome: ", markers_txt, ".</p>",

    "<p>Within-family usefulness variance is <b>", vmodel_txt, "</b>, the relationship matrix uses the <b>",
    grm, "</b> GRM computed per subgenome, and progeny are modelled as <b>", progeny_target,
    "</b>. Mate allocation used coancestry-constrained OCS (mode: <b>", sg$mode %||% "ocs", "</b>).</p>",

    "<p>The plan recommends <b>", cnt(n_sel), "</b> crosses with a mean direction-aware gain of <b>",
    fmt(ps$mean_gain), "</b>. It uses <b>", cnt(ps$unique_parents), "</b> unique parents, with no parent used ",
    "more than <b>", cnt(ps$max_parent_use), "</b> times, and a mean pairwise kinship of <b>",
    fmt(ps$mean_pair_kinship), "</b> (lower = more diverse).</p>", fig_caption)
}

# --- executive scientific summary (returns an HTML string) -----------------
ngcd_exec_summary_html <- function(res, figs = NULL) {
  if (identical(res$prediction_mode, "subgenome_design") || !is.null(res$subgenome))
    return(ngcd_exec_summary_subgenome_html(res, figs))
  ps <- res$plan_summary %||% list(); st <- res$settings %||% list()
  sc <- res$selected_crosses; cc <- res$candidate_crosses
  n_sel <- if (is.data.frame(sc)) nrow(sc) else (ps$n_crosses %||% NA)
  n_cand <- if (is.data.frame(cc)) nrow(cc) else NA
  n_traits <- if (is.data.frame(res$effect_summary)) nrow(res$effect_summary) else NA
  fmt <- function(x, d = 3) if (is.null(x) || !is.finite(suppressWarnings(as.numeric(x[1])))) "&ndash;" else formatC(as.numeric(x[1]), format = "f", digits = d)
  # display-safe integer counts: never surface a raw "NA" to the reader
  cnt <- function(x) if (length(x) != 1L || is.na(suppressWarnings(as.numeric(x)))) "&ndash;" else as.character(x)
  n_sel_txt <- cnt(n_sel)

  tiers <- if (is.data.frame(sc) && "priority_tier" %in% names(sc)) table(factor(sc$priority_tier, NGCD_TIER_ORD)) else NULL
  tier_txt <- if (!is.null(tiers)) paste(sprintf("%d %s", as.integer(tiers), gsub("_", " ", names(tiers))), collapse = ", ") else "&ndash;"

  rel_txt <- ""
  es <- res$effect_summary
  if (is.data.frame(es) && "marker_effect_reliability" %in% names(es)) {
    o <- order(es$marker_effect_reliability, decreasing = TRUE)
    top <- es$trait[o][1]; topv <- es$marker_effect_reliability[o][1]
    low <- es$trait[o][length(o)]; lowv <- es$marker_effect_reliability[o][length(o)]
    lowcap <- if (is.finite(lowv) && lowv < 0.2) sprintf(" Interpret %s cautiously (low model reliability, %.2f).", low, lowv) else ""
    rel_txt <- sprintf("Marker-effect reliability was highest for <b>%s</b> (%.2f) and lowest for <b>%s</b> (%.2f).%s",
                       top, topv, low, lowv, lowcap)
  }
  qc <- res$qc %||% list(); qc_status <- qc$status %||% "unknown"

  sw <- res$cross_number_sweep
  crit_lab <- c(elbow_relative = "diminishing-returns (relative marginal gain)",
                elbow_kneedle = "diminishing-returns (kneedle)",
                ne_target = "effective-population-size floor",
                coancestry_budget = "coancestry budget")
  sweep_txt <- if (!is.null(sw) && !is.null(sw$recommended_k)) {
    kr <- sw$k_range
    sprintf(paste0("<p>The number of crosses was chosen <b>automatically</b>: the ",
                   "%s rule recommended <b>K = %d</b>%s.</p>"),
            crit_lab[[sw$criterion %||% "elbow_relative"]] %||% "diminishing-returns",
            as.integer(sw$recommended_k),
            if (length(kr)) sprintf(" over the range K = %d&ndash;%d", min(kr), max(kr)) else "")
  } else ""

  fig_caption <- if (!is.null(figs) && length(figs))
    paste0("<p class='cap'>Figures in this report: ",
           paste(vapply(figs, function(f) f$title, character(1)), collapse = ", "), ".</p>")
  else ""

  paste0(
    "<h2 id='summary'>Executive summary</h2>",
    "<p>This report summarises a genomic cross-prediction and mate-allocation run performed with the ",
    "<b>nextgenCrossDesign</b> backend (v", res$package_version %||% "?", "), in <b>",
    (res$prediction_mode %||% "?"), "</b> mode for a <b>", (st$progeny %||% "?"),
    "</b> breeding system. Crosses were scored with the <b>", (st$trait_value_metric %||% "?"),
    "</b> metric and allocated with the <b>", (st$allocation_method %||% "?"), "</b> method (optimizer: <b>",
    (st$optimizer %||% st$optimizer_method %||% "?"), "</b>).</p>",

    "<p>From <b>", (if (is.na(n_cand)) "&ndash;" else n_cand), "</b> candidate crosses, the workbench recommends <b>",
    n_sel_txt, "</b> crosses. The plan has a mean direction-aware multi-trait gain of <b>", fmt(ps$mean_gain),
    "</b> and a group coancestry of <b>", fmt(ps$group_coancestry),
    "</b> (lower = more diverse). It uses <b>", (ps$unique_parents %||% "&ndash;"),
    "</b> unique parents, with no parent used more than <b>", (ps$max_parent_use %||% "&ndash;"),
    "</b> times, and a mean expected progeny inbreeding of <b>", fmt(ps$mean_progeny_inbreeding),
    "</b>.</p>", sweep_txt,

    "<p>The plan spans <b>", (if (is.na(n_traits)) "&ndash;" else n_traits),
    "</b> traits. ", rel_txt, "</p>",

    "<p>Priority tiers: ", tier_txt, ". Data QC status: <b>", qc_status, "</b>",
    (if (!is.null(qc$counts)) sprintf(" (blockers %s, warnings %s)", qc$counts$blockers %||% 0, qc$counts$warnings %||% 0) else ""),
    ".</p>", fig_caption)
}

# ===========================================================================
# applicability predicates (a figure is drawn only when its data are present)
# ===========================================================================
ngcd_has_tiers    <- function(res) { sc <- res$selected_crosses; is.data.frame(sc) && "priority_tier" %in% names(sc) && nrow(sc) > 0 }
ngcd_has_scores   <- function(res) { cc <- res$candidate_crosses; is.data.frame(cc) && "multi_trait_score" %in% names(cc) && nrow(cc) > 0 }
ngcd_has_scatter  <- function(res) { cc <- res$candidate_crosses; is.data.frame(cc) && all(c("multi_trait_score","pair_kinship") %in% names(cc)) && nrow(cc) > 0 }
ngcd_has_parents  <- function(res) { sc <- res$selected_crosses; is.data.frame(sc) && all(c("parent1","parent2") %in% names(sc)) && nrow(sc) > 0 }
ngcd_has_reliab   <- function(res) { es <- res$effect_summary; is.data.frame(es) && "marker_effect_reliability" %in% names(es) && nrow(es) > 0 }
ngcd_has_traitmap <- function(res) { sc <- res$selected_crosses; is.data.frame(sc) && length(grep("_value$", names(sc))) > 0 && nrow(sc) > 0 }
ngcd_has_frontier <- function(res) { fr <- res$plan_summary$frontier; is.data.frame(fr) && nrow(fr) > 1 }
ngcd_sweep_curve <- function(res) {
  cv <- res$cross_number_sweep$curve
  if (is.data.frame(cv) && nrow(cv) > 1 && "K" %in% names(cv)) cv else NULL
}
ngcd_has_sweep <- function(res) !is.null(ngcd_sweep_curve(res))
ngcd_dup_pairs <- function(res) {
  qc <- res$qc %||% list()
  pd <- qc$putative_duplicates %||% qc$cleaning$putative_duplicates
  pairs <- pd$pairs
  if (is.data.frame(pairs) && nrow(pairs) > 0) pairs else NULL
}
ngcd_has_dups <- function(res) !is.null(ngcd_dup_pairs(res))

# ===========================================================================
# base-R figures (for the PDF)
# ===========================================================================
ngcd_fig_priority_tiers <- function(res) {
  sc <- res$selected_crosses
  tb <- table(factor(sc$priority_tier, NGCD_TIER_ORD)); tb <- tb[tb > 0]
  bp <- barplot(as.integer(tb), names.arg = gsub("_", " ", names(tb)),
                col = NGCD_TIER_COL[names(tb)], ylab = "Number of selected crosses",
                main = "Priority tiers for selected crosses", border = NA,
                ylim = c(0, max(as.integer(tb)) * 1.15))
  text(bp, as.integer(tb), labels = as.integer(tb), pos = 3, xpd = NA)
}
ngcd_fig_score_dist <- function(res) {
  cc <- res$candidate_crosses; sc <- res$selected_crosses
  hist(cc$multi_trait_score, breaks = 40, col = "#dbe7e0", border = "#c3d3ca",
       main = "Multi-trait score distribution", xlab = "Direction-aware multi-trait score")
  if (is.data.frame(sc) && "multi_trait_score" %in% names(sc))
    rug(sc$multi_trait_score, col = "#b3261e", lwd = 2)
}
ngcd_fig_parent_use <- function(res) {
  sc <- res$selected_crosses
  pu <- sort(table(c(sc$parent1, sc$parent2)))
  par(mar = c(4, 8, 3, 1))
  barplot(as.integer(pu), names.arg = names(pu), horiz = TRUE, las = 1,
          col = "#5b83a8", border = NA, xlab = "Selected crosses",
          main = "Parent use in recommended plan")
}
ngcd_fig_reliability <- function(res) {
  es <- res$effect_summary
  o <- order(es$marker_effect_reliability)
  dir <- if ("direction" %in% names(es)) es$direction[o] else rep("maximize", nrow(es))
  col <- ifelse(grepl("max|incr", tolower(dir)), "#00583d", "#c0562f")
  par(mar = c(4, 8, 3, 1))
  barplot(es$marker_effect_reliability[o], names.arg = es$trait[o], horiz = TRUE, las = 1,
          col = col, border = NA, xlab = "Cross-validation reliability", main = "Trait model reliability")
}
ngcd_fig_scatter <- function(res) {
  cc <- res$candidate_crosses; sc <- res$selected_crosses
  plot(cc$pair_kinship, cc$multi_trait_score, pch = 19, cex = 0.4, col = "#9aa5a0aa",
       xlab = "Pair kinship", ylab = "Multi-trait score",
       main = "Selected priority tiers versus all candidate crosses")
  if (is.data.frame(sc) && all(c("multi_trait_score","pair_kinship","priority_tier") %in% names(sc))) {
    for (t in NGCD_TIER_ORD) {
      s <- sc[sc$priority_tier == t, , drop = FALSE]
      if (nrow(s)) points(s$pair_kinship, s$multi_trait_score, pch = 19, cex = 1.1, col = NGCD_TIER_COL[t])
    }
    legend("topright", legend = c("All candidate crosses", gsub("_", " ", NGCD_TIER_ORD)),
           pch = 19, col = c("#9aa5a0", NGCD_TIER_COL[NGCD_TIER_ORD]), bty = "n", pt.cex = c(0.6, rep(1.1, 4)))
  }
}
ngcd_fig_trait_heatmap <- function(res) {
  sc <- res$selected_crosses
  vcols <- grep("_value$", names(sc), value = TRUE)
  m <- as.matrix(sc[, vcols, drop = FALSE]); storage.mode(m) <- "double"
  m <- base::scale(m); m[!is.finite(m)] <- 0
  labs <- if ("parent1" %in% names(sc)) paste0(seq_len(nrow(sc)), ":", sc$parent1, ":", sc$parent2) else seq_len(nrow(sc))
  par(mar = c(7, 8, 3, 1))
  cols <- colorRampPalette(c("#3a6ea5", "#f7f7f7", "#b3261e"))(41)
  image(seq_len(ncol(m)), seq_len(nrow(m)), t(m[nrow(m):1, , drop = FALSE]), col = cols, axes = FALSE,
        xlab = "", ylab = "", main = "Selected crosses by direction-aware trait rank")
  axis(1, at = seq_len(ncol(m)), labels = sub("_value$", "", vcols), las = 2, cex.axis = 0.7)
  axis(2, at = seq_len(nrow(m)), labels = rev(labs), las = 1, cex.axis = 0.5)
}
ngcd_fig_dup_heatmap <- function(res) {
  pairs <- ngcd_dup_pairs(res)
  cols_ab <- names(pairs)[1:2]; simcol <- grep("ibs|sim|identity", tolower(names(pairs)))
  ids <- unique(c(pairs[[cols_ab[1]]], pairs[[cols_ab[2]]]))
  M <- matrix(NA_real_, length(ids), length(ids), dimnames = list(ids, ids)); diag(M) <- 1
  sc <- if (length(simcol)) pairs[[simcol[1]]] else rep(1, nrow(pairs))
  for (i in seq_len(nrow(pairs))) { a <- pairs[[cols_ab[1]]][i]; b <- pairs[[cols_ab[2]]][i]; M[a,b] <- sc[i]; M[b,a] <- sc[i] }
  par(mar = c(6, 6, 3, 1))
  cols <- colorRampPalette(c("#e69a6b", "#c0392b"))(21)
  image(seq_along(ids), seq_along(ids), M[, length(ids):1], col = cols, axes = FALSE, xlab = "", ylab = "",
        main = "Putative duplicate genotype similarity")
  axis(1, at = seq_along(ids), labels = ids, las = 2, cex.axis = 0.7)
  axis(2, at = seq_along(ids), labels = rev(ids), las = 1, cex.axis = 0.7)
}
ngcd_fig_frontier <- function(res) {
  fr <- res$plan_summary$frontier; fr <- fr[order(fr$group_coancestry), , drop = FALSE]
  op_x <- res$plan_summary$group_coancestry; op_y <- res$plan_summary$mean_gain
  plot(fr$group_coancestry, fr$mean_gain, type = "b", pch = 19, col = "#00583d",
       xlab = "Group coancestry  (diversity <- ... -> gain)", ylab = "Mean gain",
       main = "Gain-diversity frontier", cex = 1.1)
  if (!is.null(op_x) && !is.null(op_y) && is.finite(op_x) && is.finite(op_y))
    graphics::points(op_x, op_y, pch = 21, bg = "#FFC425", col = "#003524", cex = 2.4, lwd = 2)
  graphics::legend("bottomright", legend = c("frontier","selected plan"), pch = c(19,21),
                   col = c("#00583d","#003524"), pt.bg = c(NA,"#FFC425"), bty = "n")
}
ngcd_fig_cross_number <- function(res) {
  cv <- ngcd_sweep_curve(res); rk <- res$cross_number_sweep$recommended_k
  plot(cv$K, cv$total_gain, type = "b", pch = 19, col = "#00583d",
       xlab = "Number of crosses (K)", ylab = "Total plan gain",
       main = "Cross-number: diminishing returns", cex = 1.0)
  if (!is.null(rk) && is.finite(rk)) {
    yk <- cv$total_gain[match(as.integer(rk), cv$K)]
    if (length(yk) && is.finite(yk)) {
      graphics::points(rk, yk, pch = 21, bg = "#FFC425", col = "#003524", cex = 2.4, lwd = 2)
      graphics::abline(v = rk, col = "#c0562f", lty = 2)
    }
  }
  graphics::legend("bottomright", legend = c("gain vs K", "recommended K"), pch = c(19, 21),
                   col = c("#00583d", "#003524"), pt.bg = c(NA, "#FFC425"), bty = "n")
}

# ===========================================================================
# plotly figures (for the interactive HTML report and the in-app Report tab)
# ===========================================================================
ngcd_ply_priority_tiers <- function(res) {
  sc <- res$selected_crosses
  tb <- table(factor(sc$priority_tier, NGCD_TIER_ORD)); tb <- tb[tb > 0]
  plotly::layout(plotly::plot_ly(x = gsub("_"," ",names(tb)), y = as.integer(tb), type = "bar",
    marker = list(color = unname(NGCD_TIER_COL[names(tb)])),
    text = as.integer(tb), textposition = "outside", hoverinfo = "y+x"),
    title = "Priority tiers", yaxis = list(title = "Selected crosses"), xaxis = list(title = ""))
}
ngcd_ply_score_dist <- function(res) {
  cc <- res$candidate_crosses
  plotly::layout(plotly::plot_ly(x = cc$multi_trait_score, type = "histogram",
    marker = list(color = "#8fb3a3")),
    title = "Multi-trait score distribution", xaxis = list(title = "Multi-trait score"),
    yaxis = list(title = "Count"))
}
# Per-cross hover text tying a selected cross to its real parent values: the
# multi-trait score plus each trait's mid-parent GEBV (<trait>_mean_gebv).
ngcd_cross_hover <- function(df, gebv_cols) {
  txt <- paste0(df$parent1, " x ", df$parent2)
  if ("multi_trait_score" %in% names(df))
    txt <- paste0(txt, "<br>score: ", formatC(suppressWarnings(as.numeric(df$multi_trait_score)), format = "f", digits = 2))
  for (gc in gebv_cols) {
    tr <- sub("_mean_gebv$", "", gc)
    v <- suppressWarnings(as.numeric(df[[gc]]))
    txt <- paste0(txt, "<br>", tr, " mid-parent GEBV: ", ifelse(is.finite(v), formatC(v, format = "f", digits = 2), "--"))
  }
  txt
}
ngcd_ply_scatter <- function(res) {
  cc <- res$candidate_crosses; sc <- res$selected_crosses
  gebv_cols <- if (is.data.frame(sc)) grep("_mean_gebv$", names(sc), value = TRUE) else character(0)
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = cc$pair_kinship, y = cc$multi_trait_score, type = "scattergl", mode = "markers",
    marker = list(color = "rgba(154,165,160,0.35)", size = 4), name = "candidates", hoverinfo = "none")
  if (is.data.frame(sc) && "priority_tier" %in% names(sc)) for (t in NGCD_TIER_ORD) {
    s <- sc[sc$priority_tier == t, , drop = FALSE]; if (!nrow(s)) next
    p <- plotly::add_trace(p, x = s$pair_kinship, y = s$multi_trait_score, type = "scatter", mode = "markers",
      marker = list(color = unname(NGCD_TIER_COL[t]), size = 9), name = gsub("_"," ",t),
      text = ngcd_cross_hover(s, gebv_cols), hoverinfo = "text")
  }
  plotly::layout(p, title = "Selected vs all candidates",
    xaxis = list(title = "Pair kinship"), yaxis = list(title = "Multi-trait score"))
}
# Portfolio scatter: level (y) x upside (x), colour = risk_bin, with iso-genetic-usefulness
# contours (UC-VPM = mu + i*sigma; lines mu = U - i*sigma). Returns NULL if columns absent.
ngcd_portfolio_plotly <- function(res) {
  sc <- res$selected_crosses
  need <- c("cross_level", "cross_upside", "risk_bin")
  if (!is.data.frame(sc) || !all(need %in% names(sc)) || !nrow(sc)) return(NULL)
  p <- suppressWarnings(as.numeric(res$settings$selection_prop %||% 0.1))
  if (!is.finite(p) || p <= 0 || p >= 1) p <- 0.1
  i <- stats::dnorm(stats::qnorm(1 - p)) / p                 # selection intensity
  cols <- c(low = "#1f7a4d", med = "#8a6d1a", high = "#a3341f")
  lab <- if ("cross" %in% names(sc)) sc$cross else seq_len(nrow(sc))
  conf_num <- suppressWarnings(as.numeric(sc$cross_confidence %||% NA))
  conf <- ifelse(is.finite(conf_num), formatC(conf_num, format = "f", digits = 2), "--")
  hov <- sprintf("%s<br>tier: %s<br>profile: %s<br>confidence: %s",
                 lab, as.character(sc$priority_tier %||% NA),
                 as.character(sc$portfolio_profile %||% NA),
                 conf)
  fig <- plotly::plot_ly()
  # iso-usefulness contour lines across the plotted range
  xr <- range(sc$cross_upside, na.rm = TRUE); yr <- range(sc$cross_level, na.rm = TRUE)
  if (all(is.finite(xr)) && all(is.finite(yr))) {
    Us <- seq(min(yr) + i * min(xr), max(yr) + i * max(xr), length.out = 4)
    for (U in Us) fig <- plotly::add_lines(fig, x = xr, y = U - i * xr,
        line = list(dash = "dot", color = "#bbbbbb"), showlegend = FALSE, hoverinfo = "skip")
  }
  for (rb in c("low","med","high")) {
    idx <- !is.na(sc$risk_bin) & as.character(sc$risk_bin) == rb
    if (any(idx)) fig <- plotly::add_markers(fig, x = sc$cross_upside[idx], y = sc$cross_level[idx],
      name = paste0(rb, " risk"), text = hov[idx], hoverinfo = "text",
      marker = list(color = cols[[rb]], size = 9))
  }
  plotly::layout(fig, title = "Portfolio: genetic level x upside (colour = estimation risk)",
    xaxis = list(title = "within-family genetic SD (upside)"),
    yaxis = list(title = "expected mean (level)"),
    annotations = list(list(text = "dotted = iso-genetic-usefulness", showarrow = FALSE,
      xref = "paper", yref = "paper", x = 1, y = 1.05)))
}
ngcd_ply_parent_use <- function(res) {
  sc <- res$selected_crosses
  pu <- sort(table(c(sc$parent1, sc$parent2)))
  plotly::layout(plotly::plot_ly(y = names(pu), x = as.integer(pu), type = "bar", orientation = "h",
    marker = list(color = "#5b83a8"), hoverinfo = "x+y"),
    title = "Parent use in recommended plan", xaxis = list(title = "Selected crosses"),
    yaxis = list(title = "", categoryorder = "array", categoryarray = names(pu)))
}
ngcd_ply_reliability <- function(res) {
  es <- res$effect_summary
  o <- order(es$marker_effect_reliability); dir <- if ("direction" %in% names(es)) es$direction[o] else "maximize"
  col <- ifelse(grepl("max|incr", tolower(dir)), "#00583d", "#c0562f")
  plotly::layout(plotly::plot_ly(y = es$trait[o], x = es$marker_effect_reliability[o], type = "bar", orientation = "h",
    marker = list(color = col), hoverinfo = "x+y"),
    title = "Trait model reliability", xaxis = list(title = "Reliability"),
    yaxis = list(title = "", categoryorder = "array", categoryarray = es$trait[o]))
}
ngcd_ply_trait_heatmap <- function(res) {
  sc <- res$selected_crosses
  vcols <- grep("_value$", names(sc), value = TRUE)
  m <- as.matrix(sc[, vcols, drop = FALSE]); storage.mode(m) <- "double"
  m <- base::scale(m); m[!is.finite(m)] <- 0
  labs <- if ("parent1" %in% names(sc)) paste0(sc$parent1, " x ", sc$parent2) else paste0("cross ", seq_len(nrow(sc)))
  # Tie each z-scored cell back to its real mid-parent GEBV (<trait>_mean_gebv), so
  # the hover shows both the rank (colour) and the value it was ranked on. Carried
  # as a formatted character matrix in customdata (shape matches z); traits without
  # a mid-parent GEBV column show "--".
  traits <- sub("_value$", "", vcols)
  gtxt <- ngcd_gebv_text_matrix(sc, traits, digits = 2)
  # A per-cell hover string (plotly R drops 2D customdata on heatmaps, so carry the
  # full label in `text` and render it verbatim). Matrix shape matches z.
  htext <- matrix("", nrow = nrow(m), ncol = ncol(m))
  for (j in seq_len(ncol(m))) for (ii in seq_len(nrow(m)))
    htext[ii, j] <- sprintf("%s<br>%s<br>rank (z-score): %.2f<br>mid-parent GEBV: %s",
                            labs[ii], traits[j], m[ii, j], gtxt[ii, j])
  plotly::layout(plotly::plot_ly(z = m, x = traits, y = labs, type = "heatmap",
    text = htext, hoverinfo = "text",
    colorscale = list(c(0,"#3a6ea5"), c(0.5,"#f7f7f7"), c(1,"#b3261e")),
    colorbar = list(title = "z-score")),
    title = "Selected crosses by direction-aware trait rank",
    xaxis = list(title = "", tickangle = -40), yaxis = list(title = ""))
}
ngcd_ply_dup_heatmap <- function(res) {
  pairs <- ngcd_dup_pairs(res)
  cols_ab <- names(pairs)[1:2]; simcol <- grep("ibs|sim|identity", tolower(names(pairs)))
  ids <- unique(c(pairs[[cols_ab[1]]], pairs[[cols_ab[2]]]))
  M <- matrix(NA_real_, length(ids), length(ids), dimnames = list(ids, ids)); diag(M) <- 1
  sv <- if (length(simcol)) pairs[[simcol[1]]] else rep(1, nrow(pairs))
  for (i in seq_len(nrow(pairs))) { a <- pairs[[cols_ab[1]]][i]; b <- pairs[[cols_ab[2]]][i]; M[a,b] <- sv[i]; M[b,a] <- sv[i] }
  plotly::layout(plotly::plot_ly(z = M, x = ids, y = ids, type = "heatmap",
    colorscale = list(c(0,"#f6e2d3"), c(1,"#c0392b")), colorbar = list(title = "Similarity"),
    hovertemplate = "%{y} vs %{x}<br>similarity %{z:.3f}<extra></extra>"),
    title = "Putative duplicate genotype similarity",
    xaxis = list(title = "", tickangle = -40), yaxis = list(title = ""))
}
ngcd_ply_frontier <- function(res) {
  fr <- res$plan_summary$frontier
  ngcd_frontier_plotly(fr, res$plan_summary$group_coancestry, res$plan_summary$mean_gain)
}
ngcd_ply_cross_number <- function(res) {
  cv <- ngcd_sweep_curve(res); rk <- res$cross_number_sweep$recommended_k
  htext <- sprintf(paste0("K = %d<br>Total gain: %.4g<br>Mean gain: %.4g<br>",
                          "Group coancestry: %.4g<br>Ne: %s"),
                   cv$K, cv$total_gain, cv$mean_gain, cv$group_coancestry,
                   ifelse(is.finite(cv$Ne_estimate), sprintf("%.1f", cv$Ne_estimate), "Inf"))
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = cv$K, y = cv$total_gain, type = "scatter", mode = "lines+markers",
    name = "gain vs K", line = list(color = "#00583d", width = 2),
    marker = list(color = "#00583d", size = 7), text = htext, hoverinfo = "text")
  if (!is.null(rk) && is.finite(rk)) {
    yk <- cv$total_gain[match(as.integer(rk), cv$K)]
    if (length(yk) && is.finite(yk))
      p <- plotly::add_trace(p, x = rk, y = yk, type = "scatter", mode = "markers",
        name = "recommended K",
        marker = list(color = "#FFC425", size = 15, line = list(color = "#003524", width = 2)),
        text = sprintf("<b>Recommended K = %d</b>", as.integer(rk)), hoverinfo = "text")
  }
  plotly::layout(p, title = "Cross-number: diminishing returns",
    xaxis = list(title = "Number of crosses (K)"), yaxis = list(title = "Total plan gain"),
    hovermode = "closest", legend = list(orientation = "h", x = 0, y = -0.2), margin = list(t = 40))
}

# ===========================================================================
# staged-pipeline figures: a NEW pre-run trait-distribution preview plus
# adapters that turn each staged-run stage's JSON output into a plotly by
# reusing the builders above. Every one of these is a pure function that must
# NEVER error - a stage JSON that is NULL, empty, or missing an expected
# field yields a small "empty" plot with an explanatory annotation instead.
# ===========================================================================

# A minimal, always-valid plotly with a centered annotation - the fallback
# shown wherever a stage/preview figure has nothing to draw.
ngcd_empty_plot <- function(msg) {
  plotly::layout(plotly::plot_ly(type = "scatter", mode = "markers", x = numeric(0), y = numeric(0)),
    xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
    yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
    annotations = list(list(text = msg, showarrow = FALSE, xref = "paper", yref = "paper",
      x = 0.5, y = 0.5, font = list(size = 14, color = "#666666"))))
}

# Pre-run preview: histogram of a single numeric phenotype column, styled to
# match ngcd_ply_score_dist. Guards every way `col` can fail to resolve to a
# usable numeric column so the preview can be wired to a live column-picker
# without ever erroring mid-selection.
ngcd_ply_trait_distribution <- function(phenotype_df, col, direction = "increase") {
  msg <- "Load a phenotype and pick a numeric trait to preview"
  if (is.null(phenotype_df) || !is.data.frame(phenotype_df) || nrow(phenotype_df) == 0 ||
      is.null(col) || length(col) != 1L || is.na(col) || !nzchar(col) ||
      !(col %in% names(phenotype_df)))
    return(ngcd_empty_plot(msg))
  x <- suppressWarnings(as.numeric(phenotype_df[[col]]))
  x <- x[is.finite(x)]
  if (!length(x)) return(ngcd_empty_plot(msg))
  plotly::layout(plotly::plot_ly(x = x, type = "histogram", marker = list(color = "#8fb3a3")),
    title = paste0("Distribution: ", col), xaxis = list(title = col), yaxis = list(title = "Count"))
}

# Horizontal bar of pre-aggregated parent use (allocate.json's `parent_use`
# shape: one row per parent with a `crosses` count), styled like
# ngcd_ply_parent_use (which instead tallies parent1/parent2 from selected
# crosses - not applicable here since allocate.json ships the counts directly).
ngcd_ply_parent_use_counts <- function(parent_use_df) {
  if (!is.data.frame(parent_use_df) || nrow(parent_use_df) == 0 ||
      !all(c("parent", "crosses") %in% names(parent_use_df)))
    return(ngcd_empty_plot("No parent-use data available."))
  n <- suppressWarnings(as.numeric(parent_use_df$crosses))
  o <- order(n)
  nm <- as.character(parent_use_df$parent)[o]; n <- n[o]
  plotly::layout(plotly::plot_ly(y = nm, x = n, type = "bar", orientation = "h",
    marker = list(color = "#5b83a8"), hoverinfo = "x+y"),
    title = "Parent use in recommended plan", xaxis = list(title = "Selected crosses"),
    yaxis = list(title = "", categoryorder = "array", categoryarray = nm))
}

# Dispatcher: the figure(s) for one completed staged-pipeline stage, built
# from that stage's parsed JSON by reusing the post-run builders above via
# minimal res-shaped lists. `allocate` returns a list of two sub-figures
# (frontier, parents) since its JSON carries both a frontier and parent-use
# summary; every other stage returns a single plotly.
ngcd_stage_figure <- function(stage, stage_json) {
  stage_json <- stage_json %||% list()
  switch(stage,
    qc = {
      res <- list(qc = stage_json)
      if (!ngcd_has_dups(res)) return(ngcd_empty_plot("No putative duplicates flagged."))
      tryCatch(ngcd_ply_dup_heatmap(res),
               error = function(e) ngcd_empty_plot("No putative duplicates flagged."))
    },
    predict = {
      res <- list(effect_summary = stage_json$effect_summary)
      if (!ngcd_has_reliab(res)) return(ngcd_empty_plot("No trait reliability data available."))
      tryCatch(ngcd_ply_reliability(res),
               error = function(e) ngcd_empty_plot("No trait reliability data available."))
    },
    index = {
      sc <- stage_json$multi_trait_score
      if (is.null(sc) || !length(sc) || !any(is.finite(suppressWarnings(as.numeric(sc)))))
        return(ngcd_empty_plot("No index scores available."))
      p <- tryCatch(
        ngcd_ply_score_dist(list(candidate_crosses = data.frame(multi_trait_score = sc))),
        error = function(e) NULL)
      if (is.null(p)) return(ngcd_empty_plot("No index scores available."))
      plotly::layout(p, title = "Index distribution (computed multi-trait score)")
    },
    allocate = list(
      frontier = {
        res <- list(plan_summary = stage_json$plan_summary)
        if (!ngcd_has_frontier(res)) ngcd_empty_plot("No gain-diversity frontier available.")
        else tryCatch(ngcd_ply_frontier(res),
                      error = function(e) ngcd_empty_plot("No gain-diversity frontier available."))
      },
      parents = tryCatch(ngcd_ply_parent_use_counts(stage_json$parent_use),
                          error = function(e) ngcd_empty_plot("No parent-use data available."))
    ),
    ngcd_empty_plot("No figure available for this stage.")
  )
}

# ===========================================================================
# figure registry: id, title, one-line description, applies(), base(), ply()
# Order defines the report / TOC order.
# ===========================================================================
ngcd_fig_registry <- function() list(
  list(id = "tiers",    title = "Priority tiers",
       desc = "How many recommended crosses fall in each priority tier.",
       applies = ngcd_has_tiers,    base = ngcd_fig_priority_tiers, ply = ngcd_ply_priority_tiers),
  list(id = "scoredist", title = "Multi-trait score distribution",
       desc = "Distribution of direction-aware multi-trait scores across all candidates, with the selected plan marked.",
       applies = ngcd_has_scores,   base = ngcd_fig_score_dist,     ply = ngcd_ply_score_dist),
  list(id = "scatter",  title = "Selected vs all candidates",
       desc = "Merit versus relatedness for every candidate cross; selected crosses coloured by tier.",
       applies = ngcd_has_scatter,  base = ngcd_fig_scatter,        ply = ngcd_ply_scatter),
  list(id = "frontier", title = "Gain-diversity frontier",
       desc = "The trade-off between genetic gain and diversity, with your selected plan marked.",
       applies = ngcd_has_frontier, base = ngcd_fig_frontier,       ply = ngcd_ply_frontier),
  list(id = "crossnum", title = "Cross-number (diminishing returns)",
       desc = "Total plan gain as the number of crosses grows, with the recommended K marked where extra crosses stop paying off.",
       applies = ngcd_has_sweep,    base = ngcd_fig_cross_number,   ply = ngcd_ply_cross_number),
  list(id = "parents",  title = "Parent use",
       desc = "How often each parent is used across the recommended plan.",
       applies = ngcd_has_parents,  base = ngcd_fig_parent_use,     ply = ngcd_ply_parent_use),
  list(id = "reliab",   title = "Trait model reliability",
       desc = "Cross-validation reliability of the marker-effect model for each trait.",
       applies = ngcd_has_reliab,   base = ngcd_fig_reliability,    ply = ngcd_ply_reliability),
  list(id = "traitmap", title = "Selected crosses by trait rank",
       desc = "Direction-aware, per-trait z-scores for each selected cross.",
       applies = ngcd_has_traitmap, base = ngcd_fig_trait_heatmap,  ply = ngcd_ply_trait_heatmap),
  list(id = "dups",     title = "Putative duplicate similarity",
       desc = "Genotype similarity among lines flagged as possible duplicates.",
       applies = ngcd_has_dups,     base = ngcd_fig_dup_heatmap,    ply = ngcd_ply_dup_heatmap))

# active (applicable) figures for a given result
ngcd_report_active <- function(res) {
  reg <- ngcd_fig_registry()
  keep <- vapply(reg, function(f) isTRUE(tryCatch(f$applies(res), error = function(e) FALSE)), logical(1))
  reg[keep]
}

# ---- helpers for the interactive HTML ------------------------------------
# Path to the bundled plotly.js (so the report can be self-contained/offline).
# Returns "" if plotly is not installed or the library file is not found, in
# which case the caller renders static PNGs. NOTE: the library must be inlined
# as *raw bytes* (readBin/writeBin) - reading it with readLines() in a non-UTF-8
# locale corrupts its non-ASCII bytes and breaks the whole script.
ngcd_plotlyjs_path <- function() {
  if (!requireNamespace("plotly", quietly = TRUE)) return("")
  d <- system.file("htmlwidgets/lib/plotlyjs", package = "plotly")
  if (!nzchar(d)) return("")
  cand <- list.files(d, pattern = "plotly.*\\.min\\.js$", full.names = TRUE)
  if (!length(cand)) return("")
  cand[1]
}
# render-ready {data, layout} JSON for a built plotly figure.
# plotly's internal to_JSON() gives byte-perfect output that plotly.js expects
# (correct unboxing of length-1 vectors, NULL handling). We reach it without a
# hard-coded ':::' so R CMD check stays clean, and fall back to jsonlite with
# matching options if that internal helper ever disappears.
ngcd_fig_json <- function(p) {
  b <- plotly::plotly_build(p)
  payload <- b$x[c("data", "layout")]
  to_json <- tryCatch(get("to_JSON", envir = asNamespace("plotly")),
                      error = function(e) NULL)
  if (is.function(to_json)) return(as.character(to_json(payload)))
  as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null",
                                na = "null", digits = 10))
}

# ===========================================================================
# self-contained HTML report: PulseSmartLab header, exec summary, cross-linked
# TOC, and one interactive figure per section (static-PNG fallback).
# ===========================================================================
ngcd_report_html <- function(res, path) {
  figs <- ngcd_report_active(res)
  use_plotly <- requireNamespace("plotly", quietly = TRUE)
  plotly_path <- if (use_plotly) ngcd_plotlyjs_path() else ""
  interactive <- use_plotly && nzchar(plotly_path) && file.exists(plotly_path)

  # table of contents (cross-links to each figure section)
  toc <- paste0(
    "<nav class='toc'><b>Contents</b><ol>",
    "<li><a href='#summary'>Executive summary</a></li>",
    "<li><a href='#diagnostics'>Diagnostics &amp; tuning</a></li>",
    paste(vapply(figs, function(f) sprintf("<li><a href='#fig-%s'>%s</a></li>", f$id, f$title), character(1)),
          collapse = ""),
    "</ol></nav>")

  # figure sections
  secs <- character(0); scripts <- character(0)
  for (f in figs) {
    body <- ""
    if (interactive) {
      j <- tryCatch(ngcd_fig_json(f$ply(res)), error = function(e) NULL)
      if (!is.null(j)) {
        divid <- paste0("plot_", f$id)
        body <- sprintf("<div class='ngcd-plot' id='%s'></div>", divid)
        scripts <- c(scripts, sprintf(
          "Plotly.newPlot('%s', %s.data, %s.layout, {responsive:true, displaylogo:false, modeBarButtonsToRemove:['lasso2d','select2d']});",
          divid, j, j))
      }
    }
    if (!nzchar(body)) {  # fallback: static PNG
      tmp <- tempfile(fileext = ".png")
      grDevices::png(tmp, width = 1000, height = 640, res = 110)
      op <- graphics::par(no.readonly = TRUE)
      tryCatch(f$base(res), error = function(e) { plot.new(); graphics::text(.5,.5,"figure error") })
      graphics::par(op); grDevices::dev.off()
      body <- sprintf("<img alt='%s' src='data:image/png;base64,%s'/>", f$title, base64enc::base64encode(tmp))
    }
    secs <- c(secs, sprintf(
      "<section id='fig-%s'><h3>%s</h3><p class='cap'>%s</p>%s<p class='top'><a href='#top'>&uarr; back to top</a></p></section>",
      f$id, f$title, f$desc, body))
  }

  render_js <- if (interactive)
    paste0("<script type='text/javascript'>document.addEventListener('DOMContentLoaded',function(){",
           paste(scripts, collapse = "\n"), "});</script>") else ""

  head_html <- paste0(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>PulseSmartLab - Cross Design Run Report</title>",
    "<style>",
    "body{font-family:'Segoe UI',Arial,sans-serif;color:#1c211f;max-width:1080px;margin:0 auto;padding:0 0 40px 0}",
    "header{display:flex;align-items:center;gap:22px;background:#ffffff;padding:20px 26px;border-bottom:5px solid #00583d}",
    "header .logo{width:340px;max-width:46vw}header .rpt{font-family:Georgia,serif;color:#16315B;font-size:22px;font-weight:700;",
    "border-left:2px solid #d7ded9;padding-left:20px;line-height:1.2}header .rpt small{display:block;font-family:'Segoe UI',Arial,sans-serif;",
    "font-size:12px;font-weight:600;letter-spacing:.06em;text-transform:uppercase;color:#2E8B2E;margin-top:3px}",
    ".body{padding:8px 26px}h2{color:#16315B;border-bottom:2px solid #FFC425;padding-bottom:5px}",
    "h3{color:#00583d;margin:6px 0 2px}p{line-height:1.55}.cap{color:#5c6b64;font-size:13px;margin-top:0}",
    "nav.toc{background:#f4f6f5;border:1px solid #d7ded9;border-left:4px solid #00583d;border-radius:6px;padding:10px 18px;margin:18px 0}",
    "nav.toc ol{margin:6px 0 2px 0;padding-left:22px}nav.toc a{color:#16315B;text-decoration:none}nav.toc a:hover{text-decoration:underline}",
    "section{border-top:1px solid #eef1ef;margin-top:26px;padding-top:8px;scroll-margin-top:14px}",
    ".ngcd-plot{width:100%;height:440px}img{max-width:100%;border:1px solid #d7ded9;border-radius:6px}",
    ".top{font-size:12px;margin-top:6px}.top a{color:#5c6b64;text-decoration:none}",
    ".foot{color:#5c6b64;font-size:12px;border-top:1px solid #d7ded9;margin-top:30px;padding-top:10px}",
    "</style>")
  body_html <- paste0(
    "</head><body><a id='top'></a>",
    "<header><div class='logo'>", ngcd_logo_svg(), "</div>",
    "<div class='rpt'>Cross Design Run Report<small>Genomic cross prediction &amp; mate allocation</small></div></header>",
    "<div class='body'>",
    ngcd_exec_summary_html(res, figs), toc,
    "<h2 id='diagnostics'>Diagnostics &amp; tuning</h2>",
    "<p class='cap'>Why each procedure produced this result, and which parameter to change to steer it.</p>",
    ngcd_diagnostics_html(res),
    "<h2>Figures</h2>",
    if (!interactive) "<p class='cap'>Static figures shown (install the R package <b>plotly</b> for interactive, zoomable charts).</p>" else
      "<p class='cap'>Charts are interactive: hover for values, drag to zoom, double-click to reset.</p>",
    paste(secs, collapse = "\n"),
    "<div class='foot'>Generated ", format(Sys.time(), "%Y-%m-%d %H:%M"),
    " &middot; nextgenCrossDesign backend v", res$package_version %||% "?",
    " &middot; PulseSmartLab &middot; Backend: Dr. Sikiru Atanda &middot; Front-end: Mario Morales.</div>",
    "</div>", render_js, "</body></html>")

  # Write as raw bytes. plotly.js is streamed straight from disk with
  # readBin/writeBin so its bytes are never re-encoded by the (possibly
  # non-UTF-8) locale - that re-encoding is what corrupted the library and
  # blanked every chart. enc2utf8() keeps our own ASCII/UTF-8 markup intact.
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(head_html)), con)
  if (interactive) {
    writeBin(charToRaw("<script type='text/javascript'>"), con)
    writeBin(readBin(plotly_path, "raw", n = file.info(plotly_path)$size), con)
    writeBin(charToRaw("</script>"), con)
  }
  writeBin(charToRaw(enc2utf8(body_html)), con)
  invisible(path)
}

# ===========================================================================
# PDF report: title/summary page + one static figure per applicable page.
# ===========================================================================
ngcd_report_pdf <- function(res, path) {
  grDevices::pdf(path, width = 9, height = 6.5, onefile = TRUE)
  on.exit(grDevices::dev.off())
  figs <- ngcd_report_active(res)
  plot.new()
  txt <- gsub("<[^>]+>", "", ngcd_exec_summary_html(res, figs))
  txt <- gsub("&ndash;", "-", txt); txt <- gsub("&nbsp;", " ", txt)
  txt <- gsub("Executive summary", "", txt, fixed = TRUE)
  graphics::text(0, 1, "PulseSmartLab - Cross Design Run Report", adj = c(0, 1), cex = 1.4, font = 2, col = "#16315B")
  wrapped <- strwrap(txt, width = 105)
  graphics::text(0, 0.9, paste(wrapped, collapse = "\n"), adj = c(0, 1), cex = 0.8)
  for (f in figs) {
    op <- graphics::par(no.readonly = TRUE)
    tryCatch(f$base(res), error = function(e) { plot.new(); graphics::text(.5,.5, paste("figure error:", f$title)) })
    graphics::par(op)
  }
  invisible(path)
}
