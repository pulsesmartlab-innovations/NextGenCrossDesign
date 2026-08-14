# ---------------------------------------------------------------------------
# Guided-redesign modelling charts (Task 8)
#
# Self-contained plotly builders for the results/diagnostics views. Each takes a
# tidy data frame of per-candidate predictions and returns a plotly widget; each
# is empty-safe (returns a labelled placeholder when data/columns are missing).
# No existing code path is touched; these are wired into the results screens in
# Task 9.
#
# Expected columns (subset tolerated): Name, Trait, Train_Test_Label,
# Predicted_value, Observed_value, Standard_error, lower_bound, upper_bound,
# Reliability. Performance chart expects: trait, model, <metric value>.
# ---------------------------------------------------------------------------

# Accent-aligned, colourblind-safe palette (NDSU green + warm/neutral).
ngcd_chart_palette <- function() {
  list(primary = "#00583d", predicted = "#00583d", observed = "#d9822b",
       high = "#00583d", moderate = "#5aa17f", low = "#c2452d",
       grid = "#eef1f0", axis = "#5c6b64")
}

# Labelled empty placeholder so a missing/short data frame never errors a panel.
ngcd_chart_empty <- function(msg = "No data to plot yet") {
  plotly::layout(
    plotly::plot_ly(type = "scatter", mode = "markers",
                    x = numeric(0), y = numeric(0)),
    annotations = list(list(text = msg, showarrow = FALSE, x = 0.5, y = 0.5,
      xref = "paper", yref = "paper",
      font = list(size = 14, color = ngcd_chart_palette()$axis))),
    xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
}

# Does `df` have all of `cols` with at least one non-NA row?
ngcd_has_cols <- function(df, cols) {
  is.data.frame(df) && nrow(df) > 0 && all(cols %in% names(df))
}

# Optional trait filter helper.
ngcd_chart_filter_trait <- function(df, trait = NULL) {
  if (!is.null(trait) && "Trait" %in% names(df)) df[df$Trait %in% trait, , drop = FALSE] else df
}

# Classify reliability into high / moderate / low bands.
ngcd_reliability_band <- function(rel, high = 0.8, moderate = 0.6) {
  factor(ifelse(rel >= high, "high", ifelse(rel >= moderate, "moderate", "low")),
         levels = c("high", "moderate", "low"))
}

# --- 1. Predicted vs Observed distribution (overlaid histogram) --------------
ngcd_chart_pred_obs_hist <- function(df, trait = NULL, nbins = 30) {
  if (!ngcd_has_cols(df, "Predicted_value")) return(ngcd_chart_empty())
  df <- ngcd_chart_filter_trait(df, trait)
  if (nrow(df) == 0) return(ngcd_chart_empty("No rows for this selection"))
  pal <- ngcd_chart_palette()
  p <- plotly::plot_ly(alpha = 0.6, nbinsx = nbins)
  p <- plotly::add_histogram(p, x = df$Predicted_value, name = "Predicted",
                             marker = list(color = pal$predicted))
  if ("Observed_value" %in% names(df) && any(!is.na(df$Observed_value)))
    p <- plotly::add_histogram(p, x = df$Observed_value, name = "Observed",
                               marker = list(color = pal$observed))
  plotly::layout(p, barmode = "overlay", bargap = 0.02,
    xaxis = list(title = "Value", gridcolor = pal$grid),
    yaxis = list(title = "Frequency", gridcolor = pal$grid),
    legend = list(orientation = "h"))
}

# --- 2. Ridgeline: predicted vs observed density -----------------------------
# Two stacked filled density curves (approximation of a ridgeline plot).
ngcd_chart_ridgeline <- function(df, trait = NULL) {
  if (!ngcd_has_cols(df, "Predicted_value")) return(ngcd_chart_empty())
  df <- ngcd_chart_filter_trait(df, trait)
  if (nrow(df) == 0) return(ngcd_chart_empty("No rows for this selection"))
  pal <- ngcd_chart_palette()
  dens <- function(x) { x <- x[is.finite(x)]; if (length(x) < 2) NULL else stats::density(x) }
  dp <- dens(df$Predicted_value)
  do <- if ("Observed_value" %in% names(df)) dens(df$Observed_value) else NULL
  if (is.null(dp) && is.null(do)) return(ngcd_chart_empty("Not enough data for a density"))
  scale <- function(d) if (is.null(d)) 0 else max(d$y)
  offset <- max(scale(dp), scale(do)) * 1.15
  p <- plotly::plot_ly()
  if (!is.null(dp))
    p <- plotly::add_lines(p, x = dp$x, y = dp$y + offset, name = "Predicted",
      line = list(color = pal$predicted), fill = "tozeroy",
      fillcolor = "rgba(0,88,61,0.35)")
  if (!is.null(do))
    p <- plotly::add_lines(p, x = do$x, y = do$y, name = "Observed",
      line = list(color = pal$observed), fill = "tozeroy",
      fillcolor = "rgba(217,130,43,0.35)")
  plotly::layout(p,
    xaxis = list(title = "Value", gridcolor = pal$grid),
    yaxis = list(title = "", showticklabels = FALSE, gridcolor = pal$grid),
    legend = list(orientation = "h"))
}

# --- 3. Prediction confidence: predicted vs reliability ----------------------
ngcd_chart_pred_confidence <- function(df, trait = NULL) {
  if (!ngcd_has_cols(df, c("Predicted_value", "Reliability"))) return(ngcd_chart_empty())
  df <- ngcd_chart_filter_trait(df, trait)
  if (nrow(df) == 0) return(ngcd_chart_empty("No rows for this selection"))
  pal <- ngcd_chart_palette()
  band <- ngcd_reliability_band(df$Reliability)
  cols <- c(high = pal$high, moderate = pal$moderate, low = pal$low)
  p <- plotly::plot_ly(x = df$Reliability, y = df$Predicted_value, color = band,
    colors = cols, type = "scatter", mode = "markers",
    text = if ("Name" %in% names(df)) df$Name else NULL,
    marker = list(size = 8, opacity = 0.8))
  plotly::layout(p,
    xaxis = list(title = "Reliability", gridcolor = pal$grid),
    yaxis = list(title = "Predicted value", gridcolor = pal$grid),
    legend = list(orientation = "h"))
}

# --- 4. Prediction intervals: sorted points with error bars ------------------
ngcd_chart_pred_intervals <- function(df, trait = NULL) {
  need <- c("Predicted_value", "lower_bound", "upper_bound")
  if (!ngcd_has_cols(df, need)) return(ngcd_chart_empty())
  df <- ngcd_chart_filter_trait(df, trait)
  if (nrow(df) == 0) return(ngcd_chart_empty("No rows for this selection"))
  df <- df[order(df$Predicted_value), , drop = FALSE]
  df$.x <- seq_len(nrow(df))
  pal <- ngcd_chart_palette()
  band <- if ("Reliability" %in% names(df)) ngcd_reliability_band(df$Reliability)
          else factor(rep("moderate", nrow(df)), levels = c("high","moderate","low"))
  cols <- c(high = pal$high, moderate = pal$moderate, low = pal$low)
  p <- plotly::plot_ly(x = df$.x, y = df$Predicted_value, color = band, colors = cols,
    type = "scatter", mode = "markers",
    error_y = list(type = "data", symmetric = FALSE,
      array = df$upper_bound - df$Predicted_value,
      arrayminus = df$Predicted_value - df$lower_bound,
      color = "rgba(60,60,60,0.5)"),
    text = if ("Name" %in% names(df)) df$Name else NULL,
    marker = list(size = 6))
  plotly::layout(p,
    xaxis = list(title = "Candidates (sorted by predicted value)", gridcolor = pal$grid),
    yaxis = list(title = "Predicted value", gridcolor = pal$grid),
    legend = list(orientation = "h"))
}

# --- 5. Performance by model (capability-gated; see design spec section 7.1) ---------
# `df`: columns trait, model, and a metric column named by `metric`.
ngcd_chart_perf_by_model <- function(df, metric = "root_mean_squared_error") {
  if (!ngcd_has_cols(df, c("model", metric))) return(ngcd_chart_empty("No model-comparison metrics"))
  pal <- ngcd_chart_palette()
  traits <- if ("trait" %in% names(df)) unique(df$trait) else "trait"
  p <- plotly::plot_ly(type = "scatter", mode = "lines+markers")
  for (t in traits) {
    sub <- if ("trait" %in% names(df)) df[df$trait == t, , drop = FALSE] else df
    sub <- sub[order(sub$model), , drop = FALSE]
    p <- plotly::add_trace(p, x = sub$model, y = sub[[metric]], name = as.character(t),
                           mode = "lines+markers")
  }
  plotly::layout(p,
    xaxis = list(title = "Model", gridcolor = pal$grid),
    yaxis = list(title = metric, gridcolor = pal$grid),
    legend = list(orientation = "h"))
}

# ===========================================================================
# Workbench-native modelling charts (wired into Results in Task 9).
# These read the REAL result schema:
#   candidate_crosses / selected_crosses: parent1, parent2, multi_trait_score,
#     pair_kinship, per-trait <trait>_value; selected also: cross_confidence,
#     cross_upside, risk_bin, priority_tier.
#   effect_summary: trait, marker_effect_reliability, direction.
# The workbench scores UNOBSERVED crosses, so there is no predicted-vs-observed
# / interval / RMSE-by-model view here (those need a CV capability; see spec 7.1).
# ===========================================================================

# Per-trait value columns and the best single "cross score" column.
ngcd_trait_value_cols <- function(df) if (is.data.frame(df)) grep("_value$", names(df), value = TRUE) else character(0)
ngcd_cross_score_col <- function(df) {
  if ("multi_trait_score" %in% names(df)) return("multi_trait_score")
  vc <- ngcd_trait_value_cols(df); if (length(vc)) return(vc[1])
  if ("value" %in% names(df)) return("value")
  NA_character_
}

# 1. Distribution of predicted cross scores (all candidate crosses).
ngcd_chart_cross_scores <- function(cc, trait = NULL, nbins = 30) {
  if (!is.data.frame(cc) || !nrow(cc)) return(ngcd_chart_empty("No candidate crosses"))
  col <- if (!is.null(trait) && paste0(trait, "_value") %in% names(cc)) paste0(trait, "_value")
         else ngcd_cross_score_col(cc)
  if (is.na(col)) return(ngcd_chart_empty("No score column"))
  x <- suppressWarnings(as.numeric(cc[[col]])); x <- x[is.finite(x)]
  if (!length(x)) return(ngcd_chart_empty("No finite scores"))
  pal <- ngcd_chart_palette()
  plotly::layout(
    plotly::add_histogram(plotly::plot_ly(nbinsx = nbins), x = x,
      marker = list(color = pal$predicted), name = "Candidate crosses"),
    xaxis = list(title = paste0("Predicted cross score (", col, ")"), gridcolor = pal$grid),
    yaxis = list(title = "Number of crosses", gridcolor = pal$grid), bargap = 0.03)
}

# 2. Ridgeline of predicted value distributions, one row per trait.
ngcd_chart_cross_scores_ridge <- function(cc) {
  vc <- ngcd_trait_value_cols(cc)
  if (!is.data.frame(cc) || !nrow(cc) || !length(vc)) return(ngcd_chart_empty("No per-trait scores"))
  pal <- ngcd_chart_palette()
  dens <- lapply(vc, function(c) { x <- suppressWarnings(as.numeric(cc[[c]])); x <- x[is.finite(x)]
    if (length(x) < 2) NULL else stats::density(x) })
  keep <- !vapply(dens, is.null, logical(1))
  vc <- vc[keep]; dens <- dens[keep]
  if (!length(vc)) return(ngcd_chart_empty("Not enough data for a density"))
  maxy <- max(vapply(dens, function(d) max(d$y), numeric(1)))
  p <- plotly::plot_ly()
  for (i in seq_along(vc)) {
    d <- dens[[i]]; off <- (i - 1) * maxy * 1.25
    p <- plotly::add_lines(p, x = d$x, y = d$y + off, name = sub("_value$", "", vc[i]),
      fill = "tozeroy", line = list(width = 1))
  }
  plotly::layout(p, xaxis = list(title = "Predicted trait value", gridcolor = pal$grid),
    yaxis = list(title = "", showticklabels = FALSE, gridcolor = pal$grid),
    legend = list(orientation = "h"))
}

# 3. Score x confidence, coloured by risk bin (selected crosses).
ngcd_chart_cross_confidence <- function(sc) {
  if (!is.data.frame(sc) || !nrow(sc) || !"multi_trait_score" %in% names(sc))
    return(ngcd_chart_empty("No selected crosses"))
  if (!"cross_confidence" %in% names(sc)) return(ngcd_chart_empty("No cross_confidence column"))
  pal <- ngcd_chart_palette()
  color <- if ("risk_bin" %in% names(sc)) as.factor(sc$risk_bin) else NULL
  lab <- if (all(c("parent1", "parent2") %in% names(sc))) paste(sc$parent1, "x", sc$parent2) else NULL
  p <- plotly::plot_ly(x = sc$cross_confidence, y = sc$multi_trait_score, color = color,
    type = "scatter", mode = "markers", text = lab, marker = list(size = 9, opacity = 0.85))
  plotly::layout(p, xaxis = list(title = "Cross confidence", gridcolor = pal$grid),
    yaxis = list(title = "Predicted cross score", gridcolor = pal$grid),
    legend = list(orientation = "h"))
}

# 4. Score vs diversity (kinship): all candidates + selected highlighted.
ngcd_chart_cross_diversity <- function(cc, sc = NULL) {
  if (!is.data.frame(cc) || !nrow(cc) || !all(c("multi_trait_score", "pair_kinship") %in% names(cc)))
    return(ngcd_chart_empty("No candidate crosses"))
  pal <- ngcd_chart_palette()
  p <- plotly::add_markers(plotly::plot_ly(), x = cc$pair_kinship, y = cc$multi_trait_score,
    name = "All candidates", marker = list(color = "rgba(120,130,125,0.45)", size = 6))
  if (is.data.frame(sc) && nrow(sc) && all(c("multi_trait_score", "pair_kinship") %in% names(sc)))
    p <- plotly::add_markers(p, x = sc$pair_kinship, y = sc$multi_trait_score, name = "Selected",
      marker = list(color = pal$primary, size = 9),
      text = if (all(c("parent1", "parent2") %in% names(sc))) paste(sc$parent1, "x", sc$parent2) else NULL)
  plotly::layout(p, xaxis = list(title = "Pairwise kinship (lower = more diverse)", gridcolor = pal$grid),
    yaxis = list(title = "Predicted cross score", gridcolor = pal$grid),
    legend = list(orientation = "h"))
}
