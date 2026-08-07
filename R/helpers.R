# ===========================================================================
# helpers.R  -  UI + formatting helpers
# ===========================================================================

ngcd_badge <- function(label, kind = c("info","ok","warn","error","exp","guard")) {
  kind <- match.arg(kind)
  shiny::span(class = paste0("ndsu-badge b-", kind), label)
}

ngcd_callout <- function(..., kind = c("info","warn","error")) {
  kind <- match.arg(kind)
  cls <- if (kind == "info") "ndsu-callout" else paste0("ndsu-callout ", kind)
  shiny::div(class = cls, ...)
}

# Turn a raw backend advisory/error string into a breeder-friendly {title, body}.
# Internal/developer notices (deprecations) are flagged hide = TRUE so they are
# never shown to breeders; unknown messages fall back to the raw text under a
# neutral title. Keeps the wall-of-text technical wording out of the UI while
# preserving it in the debug panel.
ngcd_humanize_message <- function(msg) {
  m <- tolower(msg %||% "")
  if (grepl("deprecat", m))
    return(list(title = NA_character_, body = msg, hide = TRUE))
  if (grepl("residual[ -]hetero", m) && grepl("ril", m))
    return(list(
      title = "Conservative variance estimate",
      body = paste0("One or more parents are RILs carrying residual heterozygosity, so the ",
                    "reported family variance is slightly conservative (biased low). Supply ",
                    "phased haplotypes for the exact value; the ranking of crosses is unaffected."),
      hide = FALSE))
  if (grepl("heterozygous loci|not fully inbred|residual[ -]hetero", m))
    return(list(
      title = "Heterozygous parents detected",
      body = paste0("Some parents carry heterozygous loci. A doubled-haploid or fully-inbred ",
                    "line should be homozygous - fix or exclude those parents, or set Parent ",
                    "type to 'RIL' if they are recombinant inbred lines."),
      hide = FALSE))
  list(title = "Notice", body = msg, hide = FALSE)
}

# Render a list of raw advisory strings as one professional, titled callout
# (skips developer-only notices). Returns NULL when nothing is worth showing.
ngcd_advisory_ui <- function(msgs) {
  msgs <- as.character(msgs)
  items <- Filter(function(h) !isTRUE(h$hide), lapply(msgs, ngcd_humanize_message))
  if (!length(items)) return(NULL)
  ngcd_callout(
    kind = "warn",
    shiny::tags$div(class = "ndsu-advisory-head",
      shiny::tags$b(sprintf("%d advisor%s from this run", length(items),
                            if (length(items) == 1) "y" else "ies"))),
    shiny::tags$ul(class = "ndsu-advisory-list",
      lapply(items, function(h) shiny::tags$li(
        shiny::tags$b(h$title), shiny::tags$span(": "), h$body))))
}

# One concise, auto-dismissing toast pointing to the advisory banner -- instead
# of a wall of long per-warning toasts. No-op when there is nothing to show.
ngcd_notify_advisories <- function(msgs) {
  items <- Filter(function(h) !isTRUE(h$hide),
                  lapply(as.character(msgs), ngcd_humanize_message))
  if (length(items))
    shiny::showNotification(
      sprintf("Completed with %d advisor%s - see the banner on the Results screen.",
              length(items), if (length(items) == 1) "y" else "ies"),
      type = "warning", duration = 8)
  invisible(NULL)
}

ngcd_kpi <- function(value, label)
  shiny::div(class = "ndsu-kpi", shiny::div(class = "val", value), shiny::div(class = "lab", label))

ngcd_section <- function(title, subtitle = NULL)
  shiny::tagList(shiny::div(class = "ndsu-section-title", title),
                 if (!is.null(subtitle)) shiny::div(class = "ndsu-section-sub", subtitle))

# A collapsible "how to use this section" panel shown atop each screen, labeled
# by its stage in the Data | Configure | Run | Results navbar (not a flat step
# count - the app has stages with sub-sections, not one linear sequence).
# `body` is a tagList of guidance; `next_hint` points to the following section.
# Collapsed by default; the "Show guided tour" toggle (see workbench_ui) expands
# every guide box client-side.
ngcd_guide <- function(stage, title, body, next_hint = NULL, open = FALSE) {
  args <- list(class = "ndsu-guide",
    shiny::tags$summary(shiny::span(class = "g-step", stage),
                        title, " - how to use this section"),
    shiny::div(class = "g-body", body,
      if (!is.null(next_hint)) shiny::div(class = "g-next", shiny::tags$b("Next -> "), next_hint)))
  if (isTRUE(open)) args$open <- "open"
  do.call(shiny::tags$details, args)
}

# A collapsible "Figure" disclosure shown at an activity screen, collapsed by
# default. Sibling of ngcd_guide() above, but styled/classed distinctly
# (ndsu-figtag, NOT ndsu-guide) so the "Show guided tour" toggle
# (ngcdToggleGuides(), which targets details.ndsu-guide) leaves it alone.
# Body is always a uiOutput(output_id) - the server decides plot-vs-note
# based on the stage's run status, so this helper never has to know whether
# the stage has actually run yet.
ngcd_figure_tag <- function(output_id, label = "Figure", height = "340px", desc = NULL, open = FALSE) {
  args <- list(class = "ndsu-figtag",
    shiny::tags$summary(shiny::span(class = "figtag-chip", "Figure"), " ", label),
    shiny::div(class = "figtag-body",
      if (!is.null(desc)) shiny::div(class = "help-hint", desc),
      shiny::uiOutput(output_id)))
  if (isTRUE(open)) args$open <- "open"
  do.call(shiny::tags$details, args)
}

ngcd_guess_col <- function(cols, candidates) {
  hit <- which(tolower(cols) %in% tolower(candidates))
  if (length(hit)) cols[hit[1]] else NULL
}

# Full-table read for editable tables. Robust to real-world exports:
#   * strips a UTF-8 BOM (Excel adds one; it corrupts the first column name),
#   * auto-detects the delimiter (comma / semicolon / tab / pipe) so European
#     or tab exports load as a table instead of one mashed-together column,
#   * falls back to Latin-1 if the file is not valid UTF-8.
# Returns NULL on failure.
ngcd_read_full <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  # byte-safe BOM strip/detect (works in any locale, incl. C)
  bom3 <- as.raw(c(0xEF, 0xBB, 0xBF))
  strip_bom <- function(x) {
    if (length(x) != 1L || is.na(x) || !nzchar(x)) return(x)
    r <- charToRaw(x)
    if (length(r) >= 3L && identical(r[1:3], bom3)) return(rawToChar(r[-(1:3)]))
    x
  }
  has_bom <- tryCatch({
    con <- file(path, "rb"); on.exit(close(con))
    identical(readBin(con, "raw", 3L), bom3)
  }, error = function(e) FALSE)

  # sniff the header line to pick the delimiter
  hdr <- tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) "")
  hdr <- if (length(hdr)) strip_bom(hdr[1]) else ""
  seps <- c(",", ";", "\t", "|")
  counts <- vapply(seps, function(s) {
    m <- gregexpr(s, hdr, fixed = TRUE)[[1]]; sum(m > 0)
  }, integer(1))
  sep <- if (all(counts == 0)) "," else seps[which.max(counts)]

  read_try <- function(enc) tryCatch(
    utils::read.table(path, header = TRUE, sep = sep, quote = "\"",
                      check.names = FALSE, stringsAsFactors = FALSE,
                      fill = TRUE, comment.char = "", fileEncoding = enc),
    error = function(e) NULL)
  # UTF-8-BOM strips the BOM when present; otherwise plain read, then Latin-1.
  df <- read_try(if (has_bom) "UTF-8-BOM" else "")
  if (is.null(df) || !ncol(df)) df <- read_try("UTF-8-BOM")
  if (is.null(df) || !ncol(df)) df <- read_try("latin1")
  if (is.null(df)) return(NULL)
  if (length(names(df))) names(df)[1] <- strip_bom(names(df)[1])
  names(df) <- trimws(names(df))
  df
}

ngcd_num <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return("--")
  formatC(as.numeric(x[1]), format = "f", digits = digits, big.mark = ",")
}

# Backfill plan-summary fields the polyploid / disomic-subgenome backend paths do
# not export (their plan_summary carries only n_crosses + mean_gain), so the KPI
# row and report show real numbers instead of blanks. Everything here is derived
# from the selected-crosses table, never invented: unique parents and maximum
# parent use are counts, and mean pairwise kinship is a diversity read for paths
# that don't return a group-coancestry scalar. Existing values are left untouched.
ngcd_enrich_result <- function(res) {
  if (is.null(res) || !is.list(res)) return(res)
  ps <- res$plan_summary %||% list()
  sc <- res$selected_crosses
  if (is.data.frame(sc) && all(c("parent1", "parent2") %in% names(sc)) && nrow(sc) > 0) {
    par <- c(as.character(sc$parent1), as.character(sc$parent2))
    if (is.null(ps$unique_parents)) ps$unique_parents <- length(unique(par))
    if (is.null(ps$max_parent_use)) ps$max_parent_use <- max(as.integer(table(par)))
    if (is.null(ps$mean_pair_kinship) && "pair_kinship" %in% names(sc)) {
      mpk <- mean(suppressWarnings(as.numeric(sc$pair_kinship)), na.rm = TRUE)
      if (is.finite(mpk)) ps$mean_pair_kinship <- mpk
    }
  }
  res$plan_summary <- ps
  res
}

# Read-only DT with NDSU styling.
ngcd_dt <- function(df, ..., page = 10, priority_col = NULL) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df))
    return(DT::datatable(data.frame(Message = "No rows to display."),
                         rownames = FALSE, options = list(dom = "t")))
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], function(x) round(x, 4))
  dt <- DT::datatable(df, rownames = FALSE, filter = "top", ...,
    options = list(pageLength = page, scrollX = TRUE,
                   lengthMenu = c(10, 25, 50, 100), dom = "lftip"))
  if (!is.null(priority_col) && priority_col %in% names(df))
    dt <- DT::formatStyle(dt, priority_col, target = "row",
      backgroundColor = DT::styleEqual(
        c("highly_priority","priority","medium_priority","low_priority"),
        c("#eef6f1","#f4faf6","#fffdf3","#fbfcfb")))
  dt
}

# Editable DT for input-data tables.
ngcd_dt_editable <- function(df, page = 10) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df))
    return(DT::datatable(data.frame(Message = "No data loaded."),
                         rownames = FALSE, options = list(dom = "t")))
  DT::datatable(df, rownames = FALSE, editable = list(target = "cell"),
    selection = "none", class = "cell-border stripe",
    options = list(pageLength = page, scrollX = TRUE,
                   lengthMenu = c(10, 25, 50, 100), dom = "lftip"))
}

# Replicate the backend's inbred-dosage audit: a parent violates when more than
# `fraction_tolerance` of its markers are heterozygous (dosage ~ ploidy/2).
ngcd_het_violators <- function(geno_df, id_col, marker_cols, ploidy = 2,
                               tolerance = 0.05, fraction_tolerance = 0.02) {
  if (is.null(geno_df) || !length(marker_cols)) return(list(ids = character(0), n = 0, max_frac = 0))
  m <- suppressWarnings(vapply(geno_df[marker_cols], as.numeric, numeric(nrow(geno_df))))
  if (is.null(dim(m))) m <- matrix(m, nrow = nrow(geno_df))
  het_dist <- pmin(abs(m), abs(ploidy - m))
  het_dist[!is.finite(het_dist)] <- 0
  frac <- rowMeans(het_dist > tolerance, na.rm = TRUE)
  frac[!is.finite(frac)] <- 0
  ids <- trimws(as.character(geno_df[[id_col]]))
  bad <- which(frac > fraction_tolerance)
  list(ids = ids[bad], n = length(bad), max_frac = if (length(frac)) max(frac) else 0,
       frac_tol = fraction_tolerance)
}

# Registry of restorable UI controls: input id -> widget type. Used to snapshot
# ("save settings") and restore ("load settings") the whole configuration.
ngcd_settings_registry <- function() {
  c(
    data_source = "radio", objective_mode = "radio", diversity_mode = "radio",
    selection_prop = "slider", diversity_emphasis = "slider",
    traits_to_use = "checkboxgroup", crop = "selectize",
    trait_weights = "textarea", committed_crosses = "textarea", parent_group = "textarea",
    group_quota = "textarea", group_disallow = "textarea", marker_target_spec = "textarea",
    lethal_spec = "textarea",
    alphamate_executable = "text", alphamate_runtime_path = "text", alphamate_workdir = "text",
    alphamate_lambda_grid = "text", training_genotype_id_col = "text", training_phenotype_id_col = "text",
    priority_breaks = "text", priority_labels = "text", output_file = "text",
    assume_inbred = "checkbox", use_ocs = "checkbox", threshold_penalty_autoscale = "checkbox",
    ld_pruning = "checkbox", run_posterior_prediction = "checkbox", use_parallel = "checkbox",
    drop_lethal_carrier_crosses = "checkbox", write_outputs = "checkbox", write_figures = "checkbox",
    restrict_shared_markers = "checkbox", restrict_shared_ids = "checkbox",
    drop_noninbred_parents = "checkbox", alphamate_keep_files = "checkbox",
    pos_unit = "select", trait_value_metric = "select", uc_variance_source = "select",
    method_varPMV = "select", multi_trait_method = "select", threshold_policy = "select",
    progeny = "select", ril_mode = "select", recomb_model = "select", grm_method = "select",
    duplicate_action = "select", ld_backend = "select", optimizer = "select",
    allocation_method = "select", lambda_parent_use_mode = "select", strategy = "select",
    alphamate_mode = "select", posterior_method = "select", ploidy = "select",
    genotype_id_col = "select", phenotype_id_col = "select", map_marker_col = "select",
    map_chr_col = "select", map_pos_bp_col = "select", map_pos_cm_col = "select",
    direction_trait_col = "select", direction_column_col = "select", direction_direction_col = "select",
    index_col = "select", cost_col = "select", logistic_col = "select", single_trait = "select",
    bp_per_cm = "num", map_pos_cm_divisor = "num", threshold_penalty_weight = "num",
    min_effect_reliability = "num", duplicate_threshold = "num", duplicate_maf_min = "num",
    duplicate_max_missing_prop = "num", duplicate_min_compared_markers = "num", ld_window = "num",
    ld_r2_threshold = "num", ld_maf_threshold = "num", n_crosses = "num", max_crosses_per_parent = "num",
    min_unique_parents = "num", max_pair_kinship = "num", lambda_group = "num", lambda_mating = "num",
    lambda_parent_use = "num", local_iter = "num", ocs_iter = "num", evol_solutions = "num",
    evol_iterations = "num", evol_stop = "num", evol_seed = "num", target_coancestry = "num",
    alphamate_target_degree = "num", alphamate_max_contributions = "num", alphamate_n_threads = "num",
    alphamate_number_of_parents = "num", alphamate_lambda_group = "num", alphamate_evol_solutions = "num",
    alphamate_evol_iterations = "num", alphamate_evol_stop = "num", lambda_progeny_inbreeding = "num",
    nselfing = "num",
    cross_number_mode = "radio", cross_sweep_criterion = "select",
    cross_sweep_k_min = "num", cross_sweep_k_max = "num", cross_sweep_k_step = "num",
    cross_sweep_relative_threshold = "num", cross_sweep_ne_min = "num",
    cross_sweep_coancestry_max = "num",
    robust_allocation = "checkbox", robust_objective = "select",
    robustness_quantile = "slider", robust_top_n_target = "num",
    family_size_total_progeny = "num", family_size_min = "num", family_size_max = "num",
    multitrait_joint_prob = "checkbox", multitrait_targets = "textarea", pareto_explore = "checkbox", pareto_lambdas = "text",
    workflow = "radio", poly_trait_col = "select", subgenome_col = "select", poly_gain = "select",
    poly_grm_method = "select", poly_dominance = "checkbox", poly_run_qc = "checkbox",
    poly_double_reduction = "num",
    min_crosses_per_parent = "num", lambda_marker = "num", budget = "num", lambda_cost = "num",
    lambda_logistic = "num", n_iter = "num", burn_in = "num", n_threads = "num",
    priority_score_weight = "num", priority_kinship_weight = "num", priority_threshold_weight = "num",
    seed = "num")
}

# Snapshot the current values of all registered inputs into a named list.
ngcd_collect_settings <- function(input) {
  reg <- ngcd_settings_registry()
  vals <- list()
  for (id in names(reg)) {
    v <- input[[id]]
    if (!is.null(v)) vals[[id]] <- v
  }
  vals
}

# Apply a saved settings list back onto the inputs (best-effort, type-aware).
ngcd_apply_settings <- function(session, values) {
  reg <- ngcd_settings_registry()
  for (id in intersect(names(values), names(reg))) {
    v <- values[[id]]; t <- reg[[id]]
    switch(t,
      radio         = shiny::updateRadioButtons(session, id, selected = v),
      slider        = shiny::updateSliderInput(session, id, value = as.numeric(v)),
      checkboxgroup = shiny::updateCheckboxGroupInput(session, id, selected = v),
      selectize     = shiny::updateSelectizeInput(session, id, selected = v),
      textarea      = shiny::updateTextAreaInput(session, id, value = v),
      text          = shiny::updateTextInput(session, id, value = v),
      checkbox      = shiny::updateCheckboxInput(session, id, value = isTRUE(v)),
      select        = shiny::updateSelectInput(session, id, selected = v),
      num           = shiny::updateNumericInput(session, id,
                        value = if (length(v) == 1 && (is.null(v) || is.na(v))) NA else as.numeric(v)))
  }
}

# Migrate a saved settings list's metric-related values from the legacy raw
# backend vocabulary (var_complex/pmv/vpm/mean) to the current
# breeder-intuitive friendly vocabulary (mid_parent_mean/family_variance/
# reliable_family_variance/usefulness/parent_distance), so a settings profile
# saved before the metric-names rework restores to a valid dropdown selection
# instead of leaving the control blank (updateSelectInput silently ignores an
# unrecognized value).
#
# A legacy `var_complex` or bare `pmv` restores as usefulness +
# reliable_family_variance - NOT the new pure-variance metric - because that
# preserves what the user actually configured: native usefulness scoring on
# the (then-only) PMV variance path. Legacy `vpm` restores as usefulness +
# family_variance for the same reason. `mean` maps to the friendly
# `mid_parent_mean`. Already-friendly values (and any other setting) pass
# through unchanged. Pure - no shiny, no I/O.
ngcd_migrate_metric_settings <- function(s) {
  if (is.null(s) || !length(s)) return(s)
  tvm <- s$trait_value_metric
  if (!is.null(tvm)) {
    if (tvm %in% c("var_complex", "pmv")) {
      s$trait_value_metric <- "usefulness"
      s$uc_variance_source <- "reliable_family_variance"
    } else if (identical(tvm, "vpm")) {
      s$trait_value_metric <- "usefulness"
      s$uc_variance_source <- "family_variance"
    } else if (identical(tvm, "mean")) {
      s$trait_value_metric <- "mid_parent_mean"
    }
  }
  ucs <- s$uc_variance_source
  if (!is.null(ucs)) {
    if (identical(ucs, "pmv")) s$uc_variance_source <- "reliable_family_variance"
    else if (identical(ucs, "vpm")) s$uc_variance_source <- "family_variance"
  }
  s
}

# Friendly display label for a trait_value_metric token, for report/help
# text that shows the metric to a user. Understands both the current friendly
# tokens and the legacy raw backend tokens a pre-migration saved result might
# still carry, so old runs still display sensibly. Falls back to the raw
# token for anything unrecognized.
ngcd_metric_label <- function(metric) {
  labels <- c(
    mid_parent_mean          = "Mid-parent mean",
    family_variance          = "Family variance",
    reliable_family_variance = "Reliable family variance",
    usefulness                = "Usefulness",
    parent_distance            = "Parent distance",
    # legacy raw backend tokens (pre metric-names rework)
    var_complex = "Usefulness",
    pmv         = "Reliable family variance",
    vpm         = "Family variance",
    mean        = "Mid-parent mean")
  if (is.null(metric) || !length(metric) || is.na(metric) || !nzchar(metric)) return("?")
  hit <- unname(labels[metric])
  if (is.na(hit)) metric else hit
}

# Interactive gain-diversity frontier via plotly (hover tooltips, zoom, pan).
# `fr` is the plan_summary$frontier data.frame; op_x/op_y mark the selected plan.
ngcd_frontier_plotly <- function(fr, op_x = NULL, op_y = NULL) {
  fr <- fr[order(fr$group_coancestry), , drop = FALSE]
  col <- function(nm) if (nm %in% names(fr)) fr[[nm]] else rep(NA, nrow(fr))
  lg <- col("lambda_group"); up <- col("unique_parents"); mu <- col("max_parent_use")
  mpi <- col("mean_progeny_inbreeding"); de <- col("diversity_emphasis")
  htext <- sprintf(paste0(
    "<b>Frontier point</b><br>",
    "Mean gain: %.4g<br>Group coancestry: %.4g<br>",
    "lambda_group: %.4g<br>%s%sUnique parents: %s<br>Max parent use: %s"),
    fr$mean_gain, fr$group_coancestry, lg,
    ifelse(is.na(de), "", sprintf("Diversity emphasis: %s<br>", de)),
    ifelse(is.na(mpi), "", sprintf("Mean progeny F: %.4g<br>", mpi)),
    ifelse(is.na(up), "-", up), ifelse(is.na(mu), "-", mu))

  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = fr$group_coancestry, y = fr$mean_gain,
    type = "scatter", mode = "lines+markers", name = "frontier",
    line = list(color = "#00583d", width = 2),
    marker = list(color = "#00583d", size = 8),
    text = htext, hoverinfo = "text")
  if (!is.null(op_x) && !is.null(op_y) && is.finite(op_x) && is.finite(op_y))
    p <- plotly::add_trace(p, x = op_x, y = op_y, type = "scatter", mode = "markers",
      name = "selected plan",
      marker = list(color = "#FFC425", size = 16, line = list(color = "#003524", width = 2)),
      text = sprintf("<b>Selected plan</b><br>Mean gain: %.4g<br>Group coancestry: %.4g", op_y, op_x),
      hoverinfo = "text")
  plotly::layout(p,
    title = list(text = "Gain-diversity frontier", font = list(color = "#003524")),
    xaxis = list(title = "Group coancestry  (lower = more diverse)", zeroline = FALSE),
    yaxis = list(title = "Mean gain", zeroline = FALSE),
    hovermode = "closest",
    legend = list(orientation = "h", x = 0, y = -0.22),
    margin = list(t = 40))
}

# Diminishing-returns curve for the auto cross-number sweep. x = K (number of crosses),
# y = mean gain; the recommended K (elbow) is highlighted. For the Ne / coancestry criteria
# a secondary trace shows the constrained quantity vs K so the user sees where it binds.
ngcd_diminishing_returns_plotly <- function(curve, recommended_k = NULL,
                                            criterion = "elbow_relative") {
  curve <- as.data.frame(curve, stringsAsFactors = FALSE)
  curve <- curve[order(curve$K), , drop = FALSE]
  col <- function(nm) if (nm %in% names(curve)) curve[[nm]] else rep(NA_real_, nrow(curve))
  htext <- sprintf(paste0("<b>K = %s</b><br>Mean gain: %.4g<br>",
                          "Marginal gain: %.4g<br>Ne: %.3g<br>Group coancestry: %.4g"),
    curve$K, curve$mean_gain, col("marginal_gain"),
    col("Ne_estimate"), col("group_coancestry"))
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = curve$K, y = curve$mean_gain,
    type = "scatter", mode = "lines+markers", name = "mean gain",
    line = list(color = "#00583d", width = 2), marker = list(color = "#00583d", size = 7),
    text = htext, hoverinfo = "text")
  if (!is.null(recommended_k) && is.finite(recommended_k)) {
    yk <- curve$mean_gain[match(as.integer(recommended_k), curve$K)]
    if (length(yk) && is.finite(yk))
      p <- plotly::add_trace(p, x = as.integer(recommended_k), y = yk,
        type = "scatter", mode = "markers", name = "recommended K",
        marker = list(color = "#FFC425", size = 16, line = list(color = "#003524", width = 2)),
        text = sprintf("<b>Recommended K = %d</b><br>Mean gain: %.4g", as.integer(recommended_k), yk),
        hoverinfo = "text")
  }
  sec <- if (identical(criterion, "ne_target")) list(col = "Ne_estimate", lab = "Effective size (Ne)")
         else if (identical(criterion, "coancestry_budget")) list(col = "group_coancestry", lab = "Group coancestry")
         else NULL
  if (!is.null(sec) && sec$col %in% names(curve)) {
    p <- plotly::add_trace(p, x = curve$K, y = curve[[sec$col]], yaxis = "y2",
      type = "scatter", mode = "lines", name = sec$lab,
      line = list(color = "#8a6d00", width = 1.5, dash = "dot"), hoverinfo = "skip")
  }
  plotly::layout(p,
    title = list(text = "Diminishing returns: gain vs number of crosses", font = list(color = "#003524")),
    xaxis = list(title = "Number of crosses (K)", zeroline = FALSE),
    yaxis = list(title = "Mean gain", zeroline = FALSE),
    yaxis2 = if (!is.null(sec)) list(title = sec$lab, overlaying = "y", side = "right", zeroline = FALSE) else NULL,
    hovermode = "closest", legend = list(orientation = "h", x = 0, y = -0.22),
    margin = list(t = 40))
}

# Choices for a dropdown, merging the backend registry `controls` list with the given
# hardcoded fallback. To avoid any visual regression, existing values KEEP the frontend's
# fallback labels; only registry values not already present are appended (with the registry
# label), so new/renamed backend methods surface without a UI edit and no existing label
# changes. Returns a shiny-style named vector (names = labels, values = values). If the
# registry is unavailable, returns the fallback unchanged.
ngcd_control_choices <- function(registry, id, fallback = NULL) {
  ctls <- registry$controls
  if (is.null(ctls) || !length(ctls)) return(fallback)
  hit <- Filter(function(c) identical(c$id, id), ctls)
  if (!length(hit) || !length(hit[[1]]$choices)) return(fallback)
  ch  <- hit[[1]]$choices
  reg <- stats::setNames(vapply(ch, function(x) x$value %||% "", ""),
                         vapply(ch, function(x) x$label %||% x$value %||% "", ""))
  if (is.null(fallback) || !length(fallback)) return(reg)
  extra <- reg[!(unname(reg) %in% unname(fallback))]  # registry values the fallback lacks
  c(fallback, extra)
}

# Registry-declared default for a control, or the given fallback.
ngcd_control_default <- function(registry, id, fallback = NULL) {
  ctls <- registry$controls
  if (is.null(ctls) || !length(ctls)) return(fallback)
  hit <- Filter(function(c) identical(c$id, id), ctls)
  if (!length(hit) || is.null(hit[[1]]$default)) return(fallback)
  hit[[1]]$default
}

ngcd_demo_files <- function(cfg) {
  d <- cfg$demo_data_dir
  list(genotype = file.path(d, "genotype.csv"), phenotype = file.path(d, "phenotype.csv"),
       map = file.path(d, "marker_map.csv"), direction = file.path(d, "trait_direction.csv"))
}

# Tetraploid demo (dosage 0..4 + single-trait phenotype) for the polyploid mode.
ngcd_poly_demo_files <- function(cfg) {
  d <- ngcd_res("data", "demo_poly")
  list(genotype = file.path(d, "dosage.csv"), phenotype = file.path(d, "phenotype.csv"),
       map = NULL, direction = NULL)
}

# Disomic-subgenome demo (diploid dosage 0..2 + single-trait phenotype + a marker
# map carrying a subgenome column and cM positions, so the within-family variance
# is recombination-aware) for the true-allopolyploid mode.
ngcd_subgenome_demo_files <- function(cfg) {
  d <- ngcd_res("data", "demo_subgenome")
  list(genotype = file.path(d, "dosage.csv"), phenotype = file.path(d, "phenotype.csv"),
       map = file.path(d, "marker_map.csv"), direction = NULL)
}

# Assemble the trait_checks spec data.frame from per-trait UI picks. Drops traits with no check
# chosen; "auto" direction -> unset (NA), letting the backend default from the breeding direction
# (see nextgenCrossDesign::ng_trait_check_spec, R/44_trait_checks.R).
ngcd_build_trait_checks <- function(traits, checks, directions, bases) {
  rows <- lapply(traits, function(t) {
    ck <- as.character(checks[[t]] %||% "")
    if (!nzchar(ck)) return(NULL)
    dir <- as.character(directions[[t]] %||% "auto")
    data.frame(trait = t, check = ck,
               direction = if (identical(dir, "auto")) NA_character_ else dir,
               basis = as.character(bases[[t]] %||% "gebv"), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

# Pure derivation from the breeder-facing 3-way "Selection objective" choice
# (objective_mode: single/multi/index) to the backend's prediction_mode +
# traits_to_use + whether the multi-trait combination method applies. Kept
# free of shiny so it can be unit-tested directly (see test-objective-mode.R).
#   single -> trait_by_trait, one trait, no combination method needed
#   multi  -> trait_by_trait, the trait set, combination method applies
#   index  -> index_as_trait, no trait set, no combination method
# objective_mode defaults to "single" (the radio's own default) when unset -
# e.g. the very first reactive tick, before the client has sent its initial
# input values.
ngcd_objective_backend <- function(objective_mode, single_trait, traits, index_col) {
  objective_mode <- objective_mode %||% "single"
  switch(objective_mode,
    single = list(prediction_mode = "trait_by_trait", traits_to_use = single_trait,
                  multi_trait_method_applies = FALSE),
    multi  = list(prediction_mode = "trait_by_trait", traits_to_use = traits,
                  multi_trait_method_applies = TRUE),
    index  = list(prediction_mode = "index_as_trait", traits_to_use = NULL,
                  multi_trait_method_applies = FALSE),
    stop("Unknown objective_mode: ", objective_mode))
}

# ===========================================================================
# Staged pipeline (Phase 2): pipeline state + config-subset invalidation.
#
# Deliberately dependency-light: NO hashing package (digest/rlang are not
# declared deps for this frontend, and adding one ripples into the Docker
# CRAN list). Staleness is decided two ways instead:
#   - per-stage config change: identical() on the stage's own named param
#     subset vs. the subset stored the last time that stage ran. identical()
#     is exact for the char/number/small-vector/NULL/data.frame values that
#     come out of build_params(), so no hash is needed.
#   - input-table change: an integer version counter (rv$data_version in
#     app.R) bumped whenever the loaded/edited tables mutate, compared to the
#     version stored when a stage last ran.
# ===========================================================================

ngcd_pipeline_stage_names <- c("qc", "predict", "index", "allocate", "rank")

# Stage -> vector of parameter-name patterns whose change should invalidate
# that stage. A pattern containing "*" is a glob (matched against the whole
# key, anchored); anything else is an exact key name. Deliberately more
# specific than a blanket prefix in a couple of spots so a key never silently
# lands in more than the ONE stage it actually belongs to:
#   - lambda_marker is index-only, so allocate spells out its own lambda_*
#     keys individually rather than using a "lambda_*" glob that would also
#     catch lambda_marker.
#   - min_effect_reliability is predict-only, so allocate spells out
#     min_unique_parents / min_crosses_per_parent individually rather than a
#     "min_*" glob that would also catch min_effect_reliability.
# Only keys actually present in `params` end up in a stage's subset (see
# ngcd_stage_cfg_subset()) - a pattern matching nothing simply contributes
# nothing.
ngcd_stage_key_patterns <- list(
  qc = c(
    "genotype_file", "phenotype_file", "map_file", "direction_file",
    "genotype_id_col", "phenotype_id_col",
    "map_marker_col", "map_chr_col", "map_pos_bp_col", "map_pos_cm_col",
    "map_pos_cm_divisor", "map_position_unit", "bp_per_cm",
    "direction_trait_col", "direction_column_col", "direction_direction_col",
    "prediction_mode", "traits_to_use", "index_col", "index_direction",
    "duplicate_*", "ld_*", "marker_ploidy", "ploidy"),
  predict = c(
    "training_*",
    "trait_value_metric", "uc_variance_source", "method_varPMV",
    "progeny", "recomb_model", "grm_method", "assume_inbred",
    "min_effect_reliability", "selection_prop", "seed",
    "run_posterior_prediction", "posterior_method", "n_iter", "burn_in",
    "ril_mode", "nselfing"),
  index = c(
    "multi_trait_method", "trait_weights",
    "threshold_policy", "threshold_penalty_*",
    "lethal_spec", "drop_lethal_carrier_crosses",
    "trait_checks", "check_basis", "exclude_threshold_violators",
    "marker_target_spec", "lambda_marker"),
  allocate = c(
    "n_crosses", "max_crosses_per_parent",
    "min_unique_parents", "min_crosses_per_parent", "max_pair_kinship",
    "optimizer", "allocation_method", "use_ocs",
    "lambda_group", "lambda_mating", "lambda_parent_use", "lambda_parent_use_mode",
    "lambda_cost", "lambda_logistic",
    "strategy", "diversity_emphasis", "target_coancestry",
    "committed_crosses", "parent_group", "group_*",
    "cross_cost", "cost_col", "budget", "logistic_*",
    "alphamate_*", "evol_*", "local_iter", "ocs_iter",
    "mate_relatedness", "mate_relatedness_weight"),
  # rank owns priority_* PLUS the post-run "meta" keys that
  # ngcd_coerce_backend_args() (inst/app/tools/run_cross_prediction_json.R)
  # strips out of the backend call entirely - they never reach qc/predict/
  # index/allocate, they only steer the cross-number sweep, robust-allocation
  # re-optimization, family-size allocation, multi-trait joint-prob add-on,
  # Pareto explorer, and crop-aware recommendation stamped onto the FINAL
  # assembled result. Changing one of these therefore invalidates ONLY rank.
  rank = c(
    "priority_breaks", "priority_labels", "priority_*_weight",
    "crop", "cross_number_mode",
    "cross_sweep_k_min", "cross_sweep_k_max", "cross_sweep_k_step",
    "cross_sweep_criterion", "cross_sweep_relative_threshold",
    "cross_sweep_ne_min", "cross_sweep_coancestry_max",
    "robust_allocation", "robust_objective", "robustness_quantile",
    "robust_top_n_target",
    "family_size_total_progeny", "family_size_min", "family_size_max",
    "pareto_explore", "pareto_lambdas",
    "multitrait_joint_prob", "multitrait_targets",
    # Terminal output side-effects: the workbook / figures / per-trait GEBV
    # column are written at emit time (the backend's rank stage,
    # ng_run_cp_output_files), so changing any of them invalidates ONLY rank.
    # (When write_outputs/write_figures is ON the Run button actually routes to
    # the one-shot full path -- see ngcd_run_uses_staged() -- because the staged
    # invocation never sets output_dir; but they still need a home in the
    # partition so toggling them is never a silent no-op.)
    "write_outputs", "write_figures", "output_file", "include_trait_gebv"))

# Match `keys` against a vector of glob patterns ("*" = any chars; anything
# without "*" must match exactly). Internal helper for ngcd_stage_cfg_subset().
ngcd_glob_match <- function(keys, patterns) {
  hit <- rep(FALSE, length(keys))
  for (p in patterns) {
    if (grepl("*", p, fixed = TRUE)) {
      rx <- paste0("^", gsub("*", ".*", p, fixed = TRUE), "$")
      hit <- hit | grepl(rx, keys)
    } else {
      hit <- hit | (keys == p)
    }
  }
  keys[hit]
}

# The named sub-list of `params` whose change should invalidate `stage`
# (qc/predict/index/allocate/rank). Pure - no shiny, no I/O.
ngcd_stage_cfg_subset <- function(params, stage) {
  patterns <- ngcd_stage_key_patterns[[stage]]
  if (is.null(patterns)) stop("Unknown pipeline stage: ", stage)
  keys <- ngcd_glob_match(names(params), patterns)
  params[keys]
}

# Initial rv$pipeline: nothing has run yet, every stage starts stale.
ngcd_pipeline_init <- function() {
  mk_stage <- function() list(status = "stale", cfg = NULL, json = NULL, ran_at = NULL)
  list(run_dir = NULL, input_version = NULL,
       stages = stats::setNames(lapply(ngcd_pipeline_stage_names, function(s) mk_stage()),
                                 ngcd_pipeline_stage_names))
}

# PURE: recompute staleness for every stage given the CURRENT params and
# input data_version, without ever upgrading a stage to "done" here - only an
# actual stage RUN (ngcd_pipeline_set(), driven by Task 3's run buttons) sets
# "done". A stage becomes (or stays) "stale" if any of:
#   - its own config subset changed since it was last recorded (identical()
#     against the stored `cfg`);
#   - the input tables changed (data_version != the pipeline's stored
#     input_version - this invalidates every stage, qc through rank);
#   - any upstream stage is not "done" (covers stale/blocked/error alike, so
#     a failure or edit anywhere upstream cascades downstream).
# A stage that is currently "done", whose own subset is unchanged, with every
# upstream stage "done", stays "done" untouched. A stage that is already
# "stale"/"blocked"/"error" and nothing changed simply keeps that status -
# this function never assigns "done".
ngcd_pipeline_mark <- function(pipeline, params, data_version,
                                stage_order = c("qc", "predict", "index", "allocate", "rank")) {
  input_changed <- !identical(data_version, pipeline$input_version)
  upstream_bad <- FALSE
  for (stage in stage_order) {
    st <- pipeline$stages[[stage]]
    if (is.null(st)) st <- list(status = "stale", cfg = NULL, json = NULL, ran_at = NULL)
    subset <- ngcd_stage_cfg_subset(params, stage)
    cfg_changed <- !identical(subset, st$cfg)
    if (cfg_changed || input_changed || upstream_bad) st$status <- "stale"
    st$cfg <- subset
    pipeline$stages[[stage]] <- st
    if (!identical(st$status, "done")) upstream_bad <- TRUE
  }
  pipeline$input_version <- data_version
  pipeline
}

# PURE stage-walk for do_run_pipeline() (Task 3's standard "Run" button). Given
# the current pipeline, return the ordered list of stages that must be (re)run:
# every stage from the FIRST one whose status != "done" through "rank". Stages
# already "done" upstream of that point are skipped (compute-once). If qc is
# "blocked", nothing may run - return list(blocked = TRUE, stages = character(0))
# so the caller can surface the blocker instead. If every stage is already
# "done", stages is character(0) (nothing to do). No shiny, no I/O - unit-tested
# in test-pipeline-run.R.
ngcd_next_stages <- function(pipeline, stage_order = ngcd_pipeline_stage_names) {
  stat <- function(s) (pipeline$stages[[s]]$status %||% "stale")
  if (identical(stat("qc"), "blocked"))
    return(list(blocked = TRUE, stages = character(0)))
  first <- NA_integer_
  for (i in seq_along(stage_order)) {
    if (!identical(stat(stage_order[i]), "done")) { first <- i; break }
  }
  if (is.na(first)) return(list(blocked = FALSE, stages = character(0)))
  list(blocked = FALSE, stages = stage_order[first:length(stage_order)])
}

# PURE routing predicate for the standard-workflow Run button: does this run go
# through the staged pipeline (do_run_pipeline), or fall back to the one-shot
# full run (do_run)? Two cases the staged path cannot reproduce force the full
# path (returns FALSE):
#   - cross_number_mode == "auto": the diminishing-returns cross-number sweep
#     (ng_optimize_mating_plan_curve -> elbow K + the whole curve + the Results
#     sweep chart) lives ONLY in the wrapper's non-stage full-run branch. The
#     staged path would silently pin n_crosses at cross_sweep_k_max with no
#     elbow and no sweep, so auto must use the full one-shot path.
#   - write_outputs / write_figures: the backend rank stage writes the
#     workbook/figures via ng_run_cp_output_files(), which ng_stop()s unless
#     output_dir is set. The staged invocation path (ngcd_run_stage ->
#     workflow="stage" wrapper branch) never sets output_dir, whereas the full
#     run path sets output_dir = run_dir. So any run that emits artifacts must
#     use the full path or it would error / write nothing.
# No shiny, so the routing decision is unit-testable directly (see
# test-pipeline-run.R). Returns TRUE for the ordinary fixed-K, no-artifact
# staged run; FALSE when the one-shot full path is required.
ngcd_run_uses_staged <- function(params) {
  if (identical(params$cross_number_mode %||% "fixed", "auto")) return(FALSE)
  if (isTRUE(params$write_outputs) || isTRUE(params$write_figures)) return(FALSE)
  TRUE
}

# Record the outcome of an actual stage run (Task 3 uses this after invoking
# ngcd_run_stage()). Not required by the pure invalidation contract above, but
# keeps the "how do I mark a stage done" logic in one place.
ngcd_pipeline_set <- function(pipeline, stage, status, json = NULL) {
  st <- pipeline$stages[[stage]]
  if (is.null(st)) st <- list(status = "stale", cfg = NULL, json = NULL, ran_at = NULL)
  st$status <- status
  st$json <- json
  st$ran_at <- Sys.time()
  pipeline$stages[[stage]] <- st
  pipeline
}
