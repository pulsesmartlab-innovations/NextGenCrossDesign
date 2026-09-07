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

# Per-file import status for the guided data-import cards. Pure. Returns
# list(state, label): "empty" (not uploaded), "error" (unreadable), "warn"
# (single column - likely a delimiter problem), or "ok" (N rows x M columns).
ngcd_import_state <- function(uploaded, df) {
  if (is.null(uploaded)) return(list(state = "empty", label = "not uploaded"))
  if (is.null(df) || !is.data.frame(df) || !ncol(df))
    return(list(state = "error", label = "could not read the file"))
  if (ncol(df) < 2)
    return(list(state = "warn", label = "only one column - check the delimiter"))
  list(state = "ok", label = sprintf("%d rows × %d columns", nrow(df), ncol(df)))
}

# The traits available for selection come from the PHENOTYPE file's own columns
# (every column except the parent-ID column) -- what the user uploaded and sees.
# The trait-direction file, if supplied, only annotates increase/decrease for
# these traits; it does NOT define the set. `id_col` is auto-detected when NULL.
# Pure: no shiny, no I/O.
ngcd_trait_columns <- function(pheno, id_col = NULL) {
  if (is.null(pheno) || !is.data.frame(pheno) || !ncol(pheno)) return(character(0))
  if (is.null(id_col) || !id_col %in% names(pheno))
    id_col <- ngcd_guess_col(names(pheno), c("NAME", "parent", "id", "line")) %||% names(pheno)[1]
  setdiff(names(pheno), id_col)
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
    use_ocs = "checkbox", threshold_penalty_autoscale = "checkbox",
    ld_pruning = "checkbox", run_posterior_prediction = "checkbox", use_parallel = "checkbox",
    drop_lethal_carrier_crosses = "checkbox", write_outputs = "checkbox", write_figures = "checkbox",
    restrict_shared_markers = "checkbox", restrict_shared_ids = "checkbox",
    drop_noninbred_parents = "checkbox", alphamate_keep_files = "checkbox",
    pos_unit = "select", trait_value_metric = "select", uc_variance_source = "select",
    method_varPMV = "select", multi_trait_method = "select", threshold_policy = "select",
    parent_type = "select", progeny = "select", ril_mode = "select", recomb_model = "select", grm_method = "select",
    duplicate_action = "select", ld_backend = "select", optimizer = "select",
    allocation_method = "select", lambda_parent_use_mode = "select", strategy = "select",
    alphamate_mode = "select", posterior_method = "select", ploidy = "select",
    genotype_id_col = "select", phenotype_id_col = "select", map_marker_col = "select",
    map_chr_col = "select", map_pos_bp_col = "select", map_pos_cm_col = "select",
    direction_trait_col = "select", direction_column_col = "select", direction_direction_col = "select",
    index_col = "select", cost_col = "select", logistic_col = "select", single_trait = "select",
    check_id_col = "select",
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

# Migrate a saved settings profile that predates the Parent-type selector: it
# stored the boolean `assume_inbred` (the deprecated backend flag) instead of
# `parent_type`. Map TRUE -> "inbred" (fully fixed lines; het blocked) and
# FALSE -> "ril" (residual het accepted), matching ng_reconcile_parent_type in
# the backend, then drop the stale key. A profile that already carries
# `parent_type` passes through unchanged. Pure - no shiny, no I/O.
ngcd_migrate_parent_type_settings <- function(s) {
  if (is.null(s) || !length(s)) return(s)
  if (is.null(s$parent_type) && !is.null(s$assume_inbred))
    s$parent_type <- if (isTRUE(s$assume_inbred)) "inbred" else "ril"
  s$assume_inbred <- NULL
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
#
# `drop` is the frontend's own retraction list: values the backend genuinely
# supports but that THIS app cannot drive, because it has no way to collect the
# extra inputs the backend then demands (see multi_trait_method in app.R). It is
# applied to the registry choices AND the fallback, exactly like the registry's
# own experimental/guarded status gate below, so a dropped value can never
# reappear via the registry-merge. Offering a choice that is a guaranteed hard
# error is worse than not offering it: use `drop` rather than deleting it from
# the fallback only.
ngcd_control_choices <- function(registry, id, fallback = NULL, drop = character()) {
  drop <- as.character(drop)
  if (length(drop)) fallback <- fallback[!(unname(fallback) %in% drop)]
  ctls <- registry$controls
  if (is.null(ctls) || !length(ctls)) return(fallback)
  hit <- Filter(function(c) identical(c$id, id), ctls)
  if (!length(hit) || !length(hit[[1]]$choices)) return(fallback)
  ch  <- hit[[1]]$choices
  # Status gate (belt-and-suspenders): the backend registry is the single source
  # of truth for what is exposed. Any choice the backend marks experimental/guarded
  # must never surface in the UI (VALIDATED_STATE frontend-surfacing governance).
  # We drop it from the registry choices AND from the hardcoded fallback, so the
  # backend can retract a capability without a frontend edit.
  blocked <- unique(c(drop, vapply(
    Filter(function(x) (x$status %||% "") %in% c("experimental", "guarded"), ch),
    function(x) x$value %||% "", "")))
  ch <- Filter(function(x) !((x$value %||% "") %in% blocked), ch)
  fallback <- fallback[!(unname(fallback) %in% blocked)]  # registry status overrides fallback
  if (!length(ch)) return(fallback)
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

# Assemble the backend's trait_checks data.frame from the per-trait pickers. There is no basis
# column: the backend puts the check on the run's own mean_source, which is what keeps the
# reference line on the same scale as the cross means it is drawn against.
ngcd_build_trait_checks <- function(traits, checks, directions) {
  rows <- lapply(traits, function(t) {
    ck <- as.character(checks[[t]] %||% "")
    if (!nzchar(ck)) return(NULL)
    dir <- as.character(directions[[t]] %||% "auto")
    data.frame(trait = t, check = ck,
               direction = if (identical(dir, "auto")) NA_character_ else dir,
               stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

# A check line's genotype file is a separate, optional table, but nothing
# stops a breeder from listing the SAME line in both the parent file and the
# check file (a released variety like CONLON is plausibly a candidate parent
# AND a benchmark check). The backend (R/39_cross_prediction_runner.R)
# intersects rownames(check_geno) with rownames(geno) and hard-errors on any
# overlap -- a line cannot be both an untouchable benchmark and a candidate
# parent -- but that error names neither offending ID. This is the pure
# comparison the frontend runs before the run ever starts, so the breeder gets
# a message that actually names the clashing lines (see check_id_clash_message()
# in app.R for the reactive wiring that calls this at every run entry point).
# Returns NULL when the two ID sets are disjoint (the common case).
ngcd_check_parent_clash <- function(check_ids, parent_ids, max_shown = 5L) {
  check_ids  <- trimws(as.character(check_ids  %||% character(0)))
  parent_ids <- trimws(as.character(parent_ids %||% character(0)))
  clash <- unique(intersect(check_ids[nzchar(check_ids)], parent_ids[nzchar(parent_ids)]))
  if (!length(clash)) return(NULL)
  shown <- utils::head(clash, max_shown)
  extra <- length(clash) - length(shown)
  ids_txt <- paste(shown, collapse = ", ")
  if (extra > 0L) ids_txt <- paste0(ids_txt, ", and ", extra, " more")
  paste0("A check line must not also be a candidate parent, but ", ids_txt,
         if (length(clash) == 1L) " is" else " are",
         " listed in both the parent genotype file and the check genotype file. ",
         "Decide which role that line plays and remove it from the other file before running.")
}

# A budget cap is meaningless without a per-cross cost: the backend
# (ng_optimize_mating_plan) hard-errors with "a finite budget requires cost_col"
# the moment a finite budget arrives with no cost column. The frontend already
# omits `budget` from the config in that state (build_params()), but silently
# dropping a number the breeder typed would hide their mistake, so this is the
# message the run gate shows instead. Returns NULL when there is nothing to
# report (no budget typed, or a cost column is chosen). lambda_cost /
# lambda_logistic deliberately do NOT gate here: the backend treats them as
# no-ops without a cost/logistic column rather than an error.
ngcd_budget_cost_message <- function(budget, cost_col) {
  b <- suppressWarnings(as.numeric(budget %||% NA_real_))
  if (length(b) != 1L || is.na(b) || !is.finite(b)) return(NULL)
  cc <- trimws(as.character(cost_col %||% "")[1])
  if (!is.na(cc) && nzchar(cc)) return(NULL)
  paste0("You set a budget cap (", format(b, scientific = FALSE),
         ") but no cost column, so there is no per-cross cost for it to spend against. ",
         "Upload a cost table under Configure > Mate allocation > Plan size & constraints ",
         "(Cost & logistics) and pick its Cost column, ",
         "or clear the budget cap, then run again.")
}

# Polyploid additive+dominance fitting is an explicitly experimental backend
# mode: ng_polyploid_fit_effects() refuses it unless
# allow_experimental_dominance = TRUE, because a single ridge penalty is shared
# by both variance components, so the additive/dominance split is not
# trustworthy. R/helpers.R's surfacing rule says an experimental capability must
# not surface as an ordinary control -- so the dominance checkbox is kept, but
# the run is refused until the breeder ticks the explicit acknowledgement
# (poly_allow_experimental_dominance), which is what forwards the backend flag.
# Refusing beats silently disabling dominance: the breeder asked for a
# genotypic-value model and must know they did not get one.
ngcd_experimental_dominance_message <- function(dominance, acknowledged) {
  if (!isTRUE(dominance) || isTRUE(acknowledged)) return(NULL)
  paste0("Dominance (heterosis) modelling is experimental and is switched off until you ",
         "confirm it. The additive and dominance variance components currently share a single ",
         "ridge penalty, so how much of the genetic variance is called additive versus dominance ",
         "is not reliable -- use it for research diagnostics, not for selection decisions. ",
         "Tick \"I understand ...\" under Model dominance on the Data screen to run it anyway, ",
         "or untick Model dominance to score on additive effects only.")
}

# ===========================================================================
# User-supplied phenotypic (P) and genetic (G) covariance matrices
# ===========================================================================
# Smith-Hazel (economic_index, b = P^{-1} G a) and Pesek-Baker
# (desired_gain, b = G^{-1} d) are the only two REAL selection indices the
# backend offers. "weighted", the app's other multi-trait method, is a rank
# sum: scale-invariant but magnitude-blind, with no P, no G, no heritabilities
# and no genetic correlations. Both index methods need quantitative-genetic
# covariance matrices that no part of a cross-prediction run can invent --
# ng_multitrait_index_covariance() refuses to substitute candidate-score
# covariance for them. Backend 0.27.0 takes them as ng_run_cross_prediction()
# formals (phenotypic_covariance / genetic_covariance) and is label-aware: a
# fully labelled matrix is reordered BY NAME, and a missing / extra /
# misspelled / one-sided label is a hard error.
#
# THE JSON-BRIDGE HAZARD, and why the payload looks the way it does
# ----------------------------------------------------------------
# The app reaches the backend by writing config JSON and shelling out to
# inst/app/tools/run_cross_prediction_json.R. jsonlite DROPS dimnames on a
# matrix round trip -- jsonlite::fromJSON(jsonlite::toJSON(M)) is an UNLABELLED
# matrix. The backend then reads it POSITIONALLY (its documented fallback for a
# genuinely unlabelled matrix) and cannot detect a reordering. The backend
# measured what that costs on a 3-trait permutation: Smith-Hazel coefficients
# moved by max |db| = 0.1708 and the emitted index re-ranked the candidate
# crosses at Spearman 0.9168 -- a coherent-looking index that is simply wrong,
# with nothing on screen to notice it.
#
# So a matrix is NEVER sent as a matrix. ngcd_cov_payload() emits LONG FORM:
#   { schema, traits = [...], cells = [ {trait_row, trait_col, value}, ... ] }
# Every scalar carries its own row AND column label, so no part of the payload
# can be read positionally even in principle, and the runner's decoder
# (ngcd_cov_from_payload(), defined in the runner script because that script is
# a standalone Rscript outside this namespace) rebuilds the matrix by a TILING
# ASSERT: the p^2 cells must exactly cover traits x traits, with no unknown
# label, no duplicate and no gap, or the run stops. A {traits, matrix} object
# would have been smaller, but if its `traits` field ever went missing what is
# left is still a plausible bare matrix that degrades silently back to a
# positional read -- precisely the failure this shape exists to make impossible.
NGCD_COV_SCHEMA <- "ngcd_labelled_matrix.v1"

# The trait set the backend will actually build the index over, derived exactly
# the way ng_run_cp_trait_spec() (nextgenCrossDesign R/39) derives it: the rows
# of the TRAIT-DIRECTION file (not the phenotype file's columns), filtered by
# traits_to_use matched against either the trait label or the phenotype column.
# P and G must be subset to THIS set -- backend 0.27.0 errors on an extra label,
# so a program-wide covariance matrix has to be narrowed by the caller. Pure.
ngcd_index_trait_set <- function(direction, trait_col = NULL, column_col = NULL,
                                 traits_to_use = NULL) {
  if (!is.data.frame(direction) || !nrow(direction) || !ncol(direction)) return(character(0))
  pick <- function(col, guesses) {
    if (!is.null(col) && length(col) == 1L && nzchar(col) && col %in% names(direction)) return(col)
    ngcd_guess_col(names(direction), guesses) %||% names(direction)[[1L]]
  }
  tcol <- pick(trait_col,  c("Trait", "trait", "trait_name", "TraitName", "name"))
  ccol <- pick(column_col, c("Trait", "trait", "column"))
  traits  <- trimws(as.character(direction[[tcol]]))
  columns <- trimws(as.character(direction[[ccol]]))
  keep <- nzchar(traits)
  if (!is.null(traits_to_use) && length(traits_to_use)) {
    sel <- trimws(as.character(traits_to_use))
    keep <- keep & (traits %in% sel | columns %in% sel)
  }
  unique(traits[keep])
}

# Read an uploaded covariance CSV into a LABELLED square numeric matrix.
# Contract: trait names in the first column AND as the remaining headers.
# Returns list(ok, matrix, message) -- `message` is breeder-facing and names
# the offending labels/cells; it is never a bare R condition.
ngcd_cov_from_table <- function(df, name = "covariance matrix") {
  bad <- function(...) list(ok = FALSE, matrix = NULL, message = paste0(name, ": ", ...))
  if (is.null(df) || !is.data.frame(df) || !ncol(df) || !nrow(df))
    return(bad("the file could not be read as a table."))
  if (ncol(df) < 2L)
    return(bad("only one column was read - check the file's delimiter (comma / semicolon / tab)."))
  rows <- trimws(as.character(df[[1L]]))
  cols <- trimws(as.character(names(df)[-1L]))
  if (any(!nzchar(rows)))
    return(bad("every row must be labelled with a trait name in the first column; row(s) ",
               paste(which(!nzchar(rows)), collapse = ", "), " are blank."))
  if (any(!nzchar(cols)))
    return(bad("every value column must be headed by a trait name; column header(s) ",
               paste(which(!nzchar(cols)) + 1L, collapse = ", "), " are blank."))
  dup_r <- unique(rows[duplicated(rows)]); dup_c <- unique(cols[duplicated(cols)])
  if (length(dup_r) || length(dup_c))
    return(bad("a trait may appear only once. Duplicated row label(s): ",
               if (length(dup_r)) paste(dup_r, collapse = ", ") else "<none>",
               "; duplicated column header(s): ",
               if (length(dup_c)) paste(dup_c, collapse = ", ") else "<none>", "."))
  if (length(rows) != length(cols))
    return(bad("a covariance matrix must be square, but the file has ", length(rows),
               " row(s) and ", length(cols), " value column(s)."))
  if (!setequal(rows, cols))
    return(bad("the row labels and the column headers must name the SAME traits. Only in rows: ",
               if (length(setdiff(rows, cols))) paste(setdiff(rows, cols), collapse = ", ") else "<none>",
               "; only in headers: ",
               if (length(setdiff(cols, rows))) paste(setdiff(cols, rows), collapse = ", ") else "<none>", "."))
  vals <- df[, -1L, drop = FALSE]
  m <- suppressWarnings(matrix(as.numeric(as.character(unlist(vals, use.names = FALSE))),
                               nrow = nrow(df), ncol = ncol(vals)))
  dimnames(m) <- list(rows, cols)
  if (any(!is.finite(m))) {
    hit <- which(!is.finite(m), arr.ind = TRUE)
    shown <- utils::head(sprintf("(%s, %s)", rows[hit[, 1]], cols[hit[, 2]]), 5L)
    return(bad("every entry must be a finite number. Non-numeric or missing at ",
               paste(shown, collapse = ", "),
               if (nrow(hit) > 5L) paste0(", and ", nrow(hit) - 5L, " more") else "", "."))
  }
  m <- m[rows, rows, drop = FALSE]   # put the columns in row order
  list(ok = TRUE, matrix = m, message = NULL)
}

# Validate a labelled covariance matrix against the traits this run will index,
# and SUBSET it to exactly those traits (backend 0.27.0 errors on an extra
# label, so the narrowing is the caller's job -- i.e. ours).
#
# Refuses, with a breeder-facing message, when: a trait of the run is absent
# from the matrix; the matrix is not symmetric within the backend's own 1e-8
# tolerance (refusing here rather than letting the same rule fail 20 minutes
# into a run); a variance on the diagonal is not positive; the matrix is not
# positive semidefinite; or -- when the chosen index has to invert it -- it is
# singular or so ill-conditioned that the solve returns numerical noise. The
# backend would NOT error on that last one: it ridges (1e-6) and pseudo-inverts,
# so a near-singular P or G yields plausible-looking coefficients built out of
# rounding error. The condition number is reported so the breeder can see why.
# cond_max = 1e6: with IEEE doubles carrying ~16 significant digits, a solve
# against a matrix of condition number k keeps roughly 16 - log10(k) of them, so
# by 1e6 a third of the precision of every coefficient is already gone and the
# ratio is climbing fast. It is also comfortably above anything a genuinely
# estimated covariance matrix over a handful of traits produces, and comfortably
# BELOW the 1e8 ceiling implied by the positive-semidefinite tolerance above
# (which already rejects min_eigen < 1e-8 * max_eigen as singular).
ngcd_validate_cov <- function(M, traits, name = "covariance matrix",
                              require_invertible = FALSE, cond_max = 1e6) {
  bad <- function(...) list(ok = FALSE, matrix = NULL, message = paste0(name, ": ", ...), notes = character(0))
  notes <- character(0)
  if (is.null(M) || !is.matrix(M)) return(bad("no matrix was loaded."))
  traits <- unique(trimws(as.character(traits %||% character(0))))
  if (!length(traits)) return(bad("the run has no traits to index yet - load a trait-direction file first."))
  rn <- rownames(M); cn <- colnames(M)
  if (is.null(rn) || is.null(cn)) return(bad("the matrix lost its trait labels."))
  missing <- setdiff(traits, rn)
  if (length(missing))
    return(bad("every trait in the run needs a row and a column. Missing: ",
               paste(missing, collapse = ", "), ". The matrix covers: ",
               paste(rn, collapse = ", "), "."))
  extra <- setdiff(rn, traits)
  if (length(extra)) {
    notes <- c(notes, paste0("Using the ", length(traits), " trait(s) in this run (",
                             paste(traits, collapse = ", "), "); ignoring ",
                             paste(extra, collapse = ", "), "."))
  }
  M <- M[traits, traits, drop = FALSE]
  if (any(!is.finite(M))) return(bad("contains non-numeric or missing entries for the traits in this run."))
  asym <- max(abs(M - t(M)))
  if (!is.finite(asym) || asym > 1e-8) {
    d <- abs(M - t(M)); hit <- which(d == max(d), arr.ind = TRUE)[1, ]
    return(bad("a covariance matrix must be symmetric, but the cell (",
               traits[hit[[1]]], ", ", traits[hit[[2]]], ") = ",
               format(M[hit[[1]], hit[[2]]], digits = 8), " and its mirror (",
               traits[hit[[2]]], ", ", traits[hit[[1]]], ") = ",
               format(M[hit[[2]], hit[[1]]], digits = 8), " differ by ",
               format(asym, digits = 3), ". Make the two mirrored cells exactly equal."))
  }
  if (any(diag(M) <= 0)) {
    z <- traits[diag(M) <= 0]
    return(bad("the diagonal holds each trait's variance, which must be positive. Not positive for: ",
               paste(z, collapse = ", "), "."))
  }
  Ms <- (M + t(M)) / 2
  ev <- eigen(Ms, symmetric = TRUE, only.values = TRUE)$values
  tol <- 1e-8 * max(1, max(abs(ev)))
  if (min(ev) < -tol)
    return(bad("the matrix is not a valid covariance matrix (not positive semidefinite): its smallest ",
               "eigenvalue is ", format(min(ev), digits = 3),
               ". Some pair of traits is given a correlation stronger than the variances allow."))
  if (isTRUE(require_invertible)) {
    if (min(ev) <= tol)
      return(bad("this index has to invert the matrix, but it is singular (smallest eigenvalue ",
                 format(min(ev), digits = 3), ") - at least one trait is an exact linear combination ",
                 "of the others. Drop a redundant trait, or supply a matrix estimated with more data."))
    cond <- max(ev) / min(ev)
    if (cond > cond_max)
      return(bad("this index has to invert the matrix, but it is too ill-conditioned to invert ",
                 "reliably: condition number ", format(cond, digits = 3), " (the limit is ",
                 format(cond_max, digits = 3), "). The coefficients would be dominated by rounding ",
                 "error rather than by your data. Two traits are very nearly redundant - drop one, ",
                 "or supply a better-estimated matrix."))
    notes <- c(notes, paste0("Condition number ", format(cond, digits = 4), " (invertible)."))
  }
  list(ok = TRUE, matrix = M, message = NULL, notes = notes)
}

# Labelled matrix -> the JSON-safe long-form payload described at the top of
# this block. Errors (rather than emitting something the bridge would read
# positionally) if the matrix is not square or is not labelled on BOTH
# dimensions. Pure.
ngcd_cov_payload <- function(M, name = "covariance matrix") {
  if (is.null(M)) return(NULL)
  M <- as.matrix(M)
  rn <- rownames(M); cn <- colnames(M)
  if (is.null(rn) || is.null(cn))
    stop(name, " must carry trait names on BOTH rownames and colnames before it is serialised; ",
         "an unlabelled matrix is read positionally by the backend.", call. = FALSE)
  if (nrow(M) != ncol(M) || !setequal(rn, cn))
    stop(name, " must be square and labelled with the same traits on rows and columns.", call. = FALSE)
  M <- M[rn, rn, drop = FALSE]
  cells <- vector("list", length(rn) * length(rn))
  k <- 0L
  for (i in seq_along(rn)) for (j in seq_along(rn)) {
    k <- k + 1L
    cells[[k]] <- list(trait_row = rn[[i]], trait_col = rn[[j]], value = as.numeric(M[i, j]))
  }
  list(schema = NGCD_COV_SCHEMA, traits = as.character(rn), cells = cells)
}

# The multi-trait method the backend will ACTUALLY solve with. An explicit pick
# is used as picked; "auto" is promoted by ng_breeder_selection_objective() on
# the mere presence of a positive desired_change (-> desired_gain) or
# economic_weight (-> economic_index) column in the trait-direction file, with
# desired_change winning. Returns "auto" when nothing promotes it, or NA when
# the run is not combining traits at all. Pure.
ngcd_effective_index_method <- function(direction, objective_mode, multi_trait_method) {
  if (!identical(objective_mode %||% "single", "multi")) return(NA_character_)
  m <- as.character(multi_trait_method %||% "auto")[1]
  if (!identical(m, "auto")) return(m)
  if (!is.data.frame(direction) || !nrow(direction)) return("auto")
  finite_positive <- function(col) {
    if (!(col %in% names(direction))) return(FALSE)
    x <- suppressWarnings(as.numeric(direction[[col]]))
    isTRUE(any(is.finite(x) & x > 0))
  }
  if (finite_positive("desired_change")) return("desired_gain")
  if (finite_positive("economic_weight")) return("economic_index")
  "auto"
}

# Which selection-index methods this run can actually offer, given what is
# loaded. desired_gain (Pesek-Baker) needs G alone -- backend 0.27.0 made P
# optional there, and without it the index is fully valid while only the
# REPORTED predicted response and index SD come back NA. economic_index
# (Smith-Hazel) still needs both. Pure; drives both the dropdown note and the
# run gate so the two can never disagree.
ngcd_index_method_message <- function(method, direction = NULL, has_phenotypic = FALSE,
                                      has_genetic = FALSE) {
  method <- as.character(method %||% "")[1]
  if (!(method %in% c("economic_index", "desired_gain"))) return(NULL)
  label <- if (identical(method, "economic_index")) "Economic index (Smith-Hazel, b = P^-1 G a)"
           else "Desired gains (Pesek-Baker, b = G^-1 d)"
  need <- if (identical(method, "economic_index"))
    c(if (!isTRUE(has_phenotypic)) "phenotypic covariance (P)",
      if (!isTRUE(has_genetic)) "genetic covariance (G)")
  else if (!isTRUE(has_genetic)) "genetic covariance (G)" else character(0)
  reasons <- character(0)
  if (length(need))
    reasons <- c(reasons, paste0(label, " needs the ", paste(need, collapse = " and the "),
      ". Upload it on the Data screen under ", sQuote("Trait covariance matrices"),
      " (a square traits x traits CSV with the trait names in the first column and as the headers)."))
  # The target vector: `a` for Smith-Hazel, `d` for Pesek-Baker. Both come from
  # the trait-direction file, and the backend refuses without them.
  col <- if (identical(method, "economic_index")) "economic_weight" else "desired_change"
  if (is.data.frame(direction) && nrow(direction)) {
    vals <- if (col %in% names(direction)) suppressWarnings(as.numeric(direction[[col]])) else numeric(0)
    if (!isTRUE(any(is.finite(vals) & vals > 0)))
      reasons <- c(reasons, paste0(label, " also needs a ", col,
        " column in your trait-direction file, with a positive value for at least one trait. ",
        "Without it the run stops in the backend."))
  }
  if (!length(reasons)) return(NULL)
  paste(reasons, collapse = " ")
}

# desired_gain solves b = G^{-1} d without ever touching P; P only standardises
# the REPORTED predicted response by the index SD sqrt(b' P b). Backend 0.27.0
# therefore lets a G-only run through and returns NA for those two reported
# quantities, naming them in the plan summary. Blank cells with no explanation
# are worse than no cells, so this turns that stamp into one sentence for the
# results screen. Returns NULL when nothing degraded. Pure.
ngcd_desired_gain_unavailable_message <- function(plan_summary) {
  if (!is.list(plan_summary)) return(NULL)
  un <- plan_summary$multitrait_desired_gain_unavailable
  un <- as.character(unlist(un, use.names = FALSE))
  un <- un[!is.na(un) & nzchar(un)]
  if (!length(un)) return(NULL)
  pretty <- c(predicted_response = "predicted response", index_sd = "index standard deviation")
  shown <- unname(ifelse(un %in% names(pretty), pretty[un], un))
  paste0("Desired gains ran on the genetic covariance (G) alone, which is all the ",
         "Pesek-Baker coefficients b = G^-1 d need - the index and the cross ranking are ",
         "complete and unaffected. Only the reported ", paste(shown, collapse = " and "),
         " is blank, because reporting it needs the phenotypic covariance (P) for the index ",
         "scale sqrt(b' P b). Upload P on the Data screen if you want that number too.")
}

# multi_trait_method = "auto" SELF-PROMOTES on the mere presence of a positive
# value in the trait-direction file's desired_change or economic_weight column:
# ng_breeder_selection_objective() (backend R/19) promotes to "desired_gain" or
# "economic_index" respectively, ahead of the plain "weight" column. Both are
# genuine selection indices; desired_gain needs the genetic covariance (G) and
# economic_index needs both P and G, and ng_multitrait_index_covariance() refuses
# to substitute candidate-score covariance for either.
#
# Until backend 0.27.0 the app could not supply P or G at all, so this helper
# refused every promoting file outright. It no longer does: the Data screen now
# collects both matrices, so when the promotion is BACKED by the matrices it
# needs, the run is simply allowed to proceed and this returns NULL. It refuses
# only when the promotion would reach the backend without them -- and then it
# says which matrix is missing and where to put it, instead of letting the run
# fail part-way through naming arguments the breeder never typed.
#
# ng_run_cp_trait_spec() carries EVERY column of the direction file through
# untouched, so adding one column to a spreadsheet is all it takes.
#
# The columns are deliberately NOT stripped and the method is deliberately NOT
# forced: the breeder put those numbers there on purpose.
# Returns NULL when there is nothing to report. Pure (no shiny) so the run gate and
# a unit test can both call it. `direction` is the loaded trait-direction table.
ngcd_auto_index_promotion_message <- function(direction, objective_mode,
                                              multi_trait_method,
                                              has_phenotypic = FALSE,
                                              has_genetic = FALSE) {
  if (!identical(objective_mode %||% "single", "multi")) return(NULL)
  # Only "auto" promotes; an explicitly chosen method is used as chosen (and is
  # gated by ngcd_index_method_message() instead).
  if (!identical(multi_trait_method %||% "auto", "auto")) return(NULL)
  if (!is.data.frame(direction) || !nrow(direction)) return(NULL)
  # Same test the backend applies: any finite POSITIVE value in the column.
  finite_positive <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    isTRUE(any(is.finite(x) & x > 0))
  }
  hits <- c("desired_change", "economic_weight")
  hits <- hits[hits %in% names(direction)]
  hits <- hits[vapply(hits, function(nm) finite_positive(direction[[nm]]), logical(1))]
  if (!length(hits)) return(NULL)
  # desired_change wins over economic_weight, exactly as the backend orders them.
  promoted <- if ("desired_change" %in% hits) "desired_gain" else "economic_index"
  promoted_label <- if (identical(promoted, "desired_gain")) "Desired gains (desired_gain)" else
    "Economic weights (economic_index)"
  # Supplied and valid? Then the promotion is legitimate: let it run.
  need <- if (identical(promoted, "economic_index"))
    c(if (!isTRUE(has_phenotypic)) "phenotypic covariance (P)",
      if (!isTRUE(has_genetic)) "genetic covariance (G)")
  else if (!isTRUE(has_genetic)) "genetic covariance (G)" else character(0)
  if (!length(need)) return(NULL)
  paste0("Your trait-direction file has a ", paste(hits, collapse = " and a "),
         " column with positive values, and the multi-trait method is set to Automatic. ",
         "The backend reads that as a request for ", promoted_label,
         " - a true selection index, which needs the ", paste(need, collapse = " and the "),
         ". Upload it on the Data screen under ", sQuote("Trait covariance matrices"),
         " (a square traits x traits CSV, trait names in the first column and as the headers), ",
         "or remove the ", paste(hits, collapse = " / "),
         " column from your trait-direction file, or pick Relative weights explicitly and give ",
         "the weights in the Trait weights box. Your numbers are left exactly as you entered ",
         "them - nothing has been dropped or silently rewritten.")
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
    "duplicate_*", "ld_*", "marker_ploidy", "ploidy", "run_qc", "poly_min_maf",
    "poly_max_missing_marker", "poly_max_missing_sample", "poly_run_qc",
    # disomic-subgenome: the map column that splits markers into subgenomes
    # changes how each diploid subgenome is QC'd, so it invalidates from qc.
    "subgenome_col"),
  predict = c(
    "training_*",
    "trait_value_metric", "uc_variance_source", "method_varPMV",
    "progeny", "recomb_model", "grm_method", "parent_type",
    "min_effect_reliability", "selection_prop", "seed",
    "run_posterior_prediction", "posterior_method", "n_iter", "burn_in",
    "ril_mode", "nselfing",
    # robustness_quantile is a predict key, NOT a rank one, even though the
    # robust plan it steers is assembled at rank. From backend 0.25.0 it is a
    # formal of ng_run_cross_prediction() that makes the POSTERIOR stage cache
    # that exact empirical tail of the ranked value; the post-run allocator can
    # only be served a tail the draws actually cached. Changing the slider must
    # therefore re-run predict (and everything downstream), or the allocator
    # asks for a tail the cached posterior does not have and the breeder
    # silently gets no robust plan again. Its siblings (robust_allocation,
    # robust_objective, robust_top_n_target) stay in rank -- they only steer the
    # post-run add-on. (Turning robust_allocation on/off makes this key appear
    # or disappear from the config, which invalidates predict too; that is
    # correct, since it also flips run_posterior_prediction.)
    "robustness_quantile",
    # polyploid predict (fit + score) keys
    "dominance", "poly_dominance", "gain", "poly_gain", "double_reduction",
    "poly_double_reduction", "poly_trait_col", "poly_grm_method",
    # disomic-subgenome: DH vs RIL sets the recombination-aware variance target
    # in the per-subgenome score step, so it invalidates from predict.
    "subgenome_progeny"),
  index = c(
    "multi_trait_method", "trait_weights",
    "threshold_policy", "threshold_penalty_*",
    "lethal_spec", "drop_lethal_carrier_crosses",
    # trait_checks / check_progeny_size: consumed inside the backend's own
    # ng_cp__stage_index (nextgenCrossDesign R/39_cross_prediction_runner.R)
    # -- check_progeny_size is the k in P(beat check) = 1 - Phi((tau-mu)/sigma)^k,
    # and the probability columns are attached there via
    # ng_attach_check_reference()/ng_attach_joint_check_probability(). Both
    # belong to index, NOT rank: raising/lowering progeny size must invalidate
    # the compute-once index stage (and everything downstream of it) so the
    # P(beat check) numbers actually recompute, rather than silently keeping a
    # stale k.
    # check_geno / check_pheno: the check-line reference data itself, consumed
    # inside the same ng_cp__stage_index -- changing the check genotype (or
    # phenotype, for a phenotype-mean-sourced trait) must invalidate index (and
    # everything downstream), the same as trait_checks/check_progeny_size above.
    # check_id_col: which check_geno column keys the matrix the runner builds. Changing
    # it re-keys the check lines, so it invalidates index for exactly the same reason
    # check_geno itself does.
    "trait_checks", "check_progeny_size", "check_geno", "check_pheno", "check_id_col",
    # phenotypic_covariance / genetic_covariance: the user-supplied P and G that
    # the Smith-Hazel (economic_index) and Pesek-Baker (desired_gain) solves are
    # built from. They enter at ng_cp__stage_index (through
    # ng_add_multitrait_score), exactly where multi_trait_method and
    # trait_weights do, so they belong to index -- swapping in a different G must
    # invalidate the compute-once index stage and everything downstream, or the
    # breeder silently keeps an index solved from the previous matrix.
    "phenotypic_covariance", "genetic_covariance",
    "marker_target_spec", "lambda_marker"),
  allocate = c(
    "n_crosses", "max_crosses_per_parent",
    "min_unique_parents", "min_crosses_per_parent", "max_pair_kinship",
    "optimizer", "method", "allocation_method", "use_ocs",
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
    # "priority_*_weight" already matches priority_check_weight (Task 5) - the
    # weight that lets a failing check drop a cross's priority tier, forwarded
    # as check_weight to the backend's ng_rank_cross_priority() - confirmed by
    # test-pipeline-state.R's "changing priority_check_weight..." test rather
    # than adding a redundant explicit entry.
    "priority_breaks", "priority_labels", "priority_*_weight",
    "crop", "cross_number_mode",
    "cross_sweep_k_min", "cross_sweep_k_max", "cross_sweep_k_step",
    "cross_sweep_criterion", "cross_sweep_relative_threshold",
    "cross_sweep_ne_min", "cross_sweep_coancestry_max",
    # robustness_quantile is deliberately NOT here -- it moved to `predict` when
    # backend 0.25.0 made it a real ng_run_cross_prediction() formal that steers
    # the posterior cache. See the note there.
    "robust_allocation", "robust_objective",
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

# One-line, human summary of a stage's outcome from its stored stage JSON
# (rv$pipeline$stages[[s]]$json), so each Run card can show its own result.
# Pure. NULL / empty json -> "Not run yet.".
ngcd_stage_summary <- function(stage, json) {
  if (is.null(json) || !length(json)) return("Not run yet.")
  n_issue <- function(sev) {
    iss <- json$issues
    if (is.data.frame(iss)) return(sum(iss$severity %in% sev))
    if (is.list(iss) && length(iss))
      return(sum(vapply(iss, function(x) isTRUE(x$severity %in% sev), logical(1))))
    0L
  }
  plural <- function(n) if (identical(as.integer(n), 1L)) "" else "s"
  switch(stage,
    qc = {
      if (!is.null(json$marker_report$kept)) {       # polyploid QC cleans the dosage
        sprintf("%d markers kept, %d dropped", json$marker_report$kept,
                json$marker_report$dropped %||% 0L)
      } else {
        nb <- n_issue("blocker"); nw <- n_issue("warning")
        sprintf("%d blocker%s, %d warning%s", nb, plural(nb), nw, plural(nw))
      }
    },
    predict = {
      if (!is.null(json$poly_metric)) {              # polyploid fit + score
        nc <- json$n_candidates %||% NA
        sprintf("Scored%s (metric %s)",
                if (is.na(nc)) "" else sprintf(" %d candidate crosses", nc), json$poly_metric)
      } else {
        nt <- if (is.data.frame(json$effect_summary)) nrow(json$effect_summary)
              else length(json$effect_summary)
        nc <- json$n_candidates %||% NA
        sprintf("%d trait%s scored%s", nt, plural(nt),
                if (is.na(nc)) "" else sprintf(" · %d candidate crosses", nc))
      }
    },
    index = sprintf("Selection index (%s)", (json$objective$method %||% "index")),
    allocate = {
      ps <- json$plan_summary %||% list()
      k <- ps$n_crosses %||% NA; g <- ps$mean_gain %||% NA; co <- ps$group_coancestry %||% NA
      out <- paste(c(if (!is.na(k)) sprintf("%d crosses", k),
                     if (!is.na(g)) sprintf("mean gain %.3g", g),
                     if (!is.na(co)) sprintf("coancestry %.3g", co)), collapse = " · ")
      if (nzchar(out)) out else "Allocation complete."
    },
    "")
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
