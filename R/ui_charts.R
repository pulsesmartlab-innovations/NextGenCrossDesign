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

# 5. Trait-model reliability: per-trait cross-validation reliability (bar).
#    Reads effect_summary (trait, marker_effect_reliability, [direction]).
ngcd_chart_trait_reliability <- function(es) {
  if (!is.data.frame(es) || !nrow(es) || !all(c("trait", "marker_effect_reliability") %in% names(es)))
    return(ngcd_chart_empty("No trait reliability yet"))
  pal <- ngcd_chart_palette()
  rel <- suppressWarnings(as.numeric(es$marker_effect_reliability))
  o <- order(rel)
  dir <- if ("direction" %in% names(es)) as.character(es$direction)[o] else rep("maximize", nrow(es))
  col <- ifelse(grepl("max|incr", tolower(dir)), pal$primary, pal$observed)
  plotly::layout(
    plotly::plot_ly(y = es$trait[o], x = rel[o], type = "bar", orientation = "h",
      marker = list(color = col),
      text = sprintf("%.2f", rel[o]), textposition = "auto"),
    xaxis = list(title = "Cross-validation reliability", gridcolor = pal$grid, range = c(0, 1)),
    yaxis = list(title = "", categoryorder = "array", categoryarray = es$trait[o]))
}

# ===========================================================================
# Guided, sequential presentation of the modelling graphics.
#
# Instead of showing all five charts on one board, the Results "Modelling
# graphics" panel walks the breeder through them one figure at a time, each with
# a short plain-language explanation - mirroring how the run report presents its
# figures (title + one-line description + plot). ngcd_mg_steps() is the ordered
# figure list; ngcd_mg_guided_panel() renders the card for the current step with
# Previous / Next controls and clickable step dots; the server owns the current
# step (a reactiveVal driven by inputs mg_prev / mg_next / mg_goto).
# ===========================================================================

# Ordered figure list: id, title, explanation (desc), the plotly outputId the
# server already renders, and the figure height. Keep desc to one or two
# sentences, report-style.
ngcd_mg_steps <- function() list(
  list(id = "dist", title = "Predicted cross-score distribution",
       outputId = "mg_dist", height = "440px",
       desc = paste0("How the predicted merit score is spread across every candidate cross. ",
                     "A long right tail means a few standout crosses; a tight cluster means the ",
                     "candidates are hard to separate on score alone. Use the trait selector to view ",
                     "a single trait instead of the overall score.")),
  list(id = "ridge", title = "Per-trait score ridgeline",
       outputId = "mg_ridge", height = "440px",
       desc = paste0("Where each trait's predicted values sit, one density curve per trait. ",
                     "Compare the location and spread of the traits to see which ones separate ",
                     "crosses most and where the population centres.")),
  list(id = "conf", title = "Score x confidence (selected, by risk)",
       outputId = "mg_conf", height = "440px",
       desc = paste0("The selected crosses plotted by predicted score against how confident the ",
                     "prediction is, coloured by estimation-risk bin. Crosses toward the top-right ",
                     "are high-scoring and well estimated; high-score, low-confidence crosses are ",
                     "the riskier bets.")),
  list(id = "div", title = "Score vs diversity (kinship)",
       outputId = "mg_div", height = "440px",
       desc = paste0("Every candidate by predicted score against pairwise kinship (lower kinship = ",
                     "more diverse), with the selected plan highlighted over all candidates. It shows ",
                     "the gain-versus-diversity trade-off cross by cross - a good plan favours the ",
                     "high-score, lower-kinship region.")),
  list(id = "reliab", title = "Trait-model reliability (cross-validation)",
       outputId = "mg_reliab", height = "360px",
       desc = paste0("How dependably the marker-effect model predicts each trait, from ",
                     "cross-validation, coloured by selection direction. A trait with low reliability ",
                     "adds more noise to the score - consider down-weighting or dropping it."))
)

# Small, self-contained styling for the guided panel (injected once in the UI).
ngcd_mg_css <- function() shiny::tags$style(shiny::HTML("
.ngcd-mg-head{display:flex;align-items:center;flex-wrap:wrap;gap:12px;margin:2px 0 10px}
.ngcd-mg-count{font-weight:700;color:#00583d}
.ngcd-mg-dots{display:flex;gap:6px;flex:1 1 auto}
.ngcd-mg-dot{width:26px;height:26px;border-radius:50%;border:1px solid #cfe0d8;background:#fff;
  color:#5c6b64;font-size:12px;font-weight:600;cursor:pointer;line-height:1}
.ngcd-mg-dot:hover{border-color:#00583d;color:#00583d}
.ngcd-mg-dot.is-current{background:#00583d;border-color:#00583d;color:#fff}
.ngcd-mg-navbtns{display:flex;gap:8px}
.ngcd-mg-desc{color:#3c4a44;margin:0 0 12px;max-width:70ch}
"))

# Build the card for one figure step (1-based). Emits the step's title, its
# explanation, the trait selector on the distribution step, and the single
# plotly output the server renders. Navigation is done with Shiny inputs
# mg_prev / mg_next / mg_goto.
ngcd_mg_guided_panel <- function(step = 1L) {
  steps <- ngcd_mg_steps(); n <- length(steps)
  step <- suppressWarnings(as.integer(step)); if (length(step) != 1L || is.na(step)) step <- 1L
  step <- max(1L, min(n, step))
  d <- steps[[step]]
  dots <- lapply(seq_len(n), function(i) shiny::tags$button(
    type = "button", class = if (i == step) "ngcd-mg-dot is-current" else "ngcd-mg-dot",
    title = steps[[i]]$title, onclick = sprintf(
      "Shiny.setInputValue('mg_goto', %d, {priority:'event'})", i), as.character(i)))
  nav <- shiny::div(class = "ngcd-mg-navbtns",
    shiny::actionButton("mg_prev", "< Previous", class = "btn btn-sm btn-outline-secondary"),
    shiny::actionButton("mg_next", "Next >", class = "btn btn-sm btn-ndsu"))
  header <- shiny::div(class = "ngcd-mg-head",
    shiny::span(class = "ngcd-mg-count", sprintf("Figure %d of %d", step, n)),
    shiny::div(class = "ngcd-mg-dots", dots), nav)
  extra <- if (identical(d$id, "dist")) shiny::uiOutput("mg_trait_ui") else NULL
  bslib::card(
    bslib::card_header(d$title),
    header,
    shiny::p(class = "ngcd-mg-desc", d$desc),
    extra,
    plotly::plotlyOutput(d$outputId, height = d$height))
}
