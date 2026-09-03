# ===========================================================================
# ui_explore.R  -  Results > Explore: four linked figures sharing one selection.
#
# WHY A SEPARATE VIEW
# The guided "Modelling graphics" stepper shows one figure at a time, which is
# right for a walkthrough but wrong for brushing: a selection made in one figure
# is invisible until you page to another. Explore puts the linkable figures on
# one screen so the link is observable, and leaves the stepper alone.
#
# TWO UNITS, ONE SELECTION
# The figures are not in the same unit of analysis:
#
#   scatter   score vs kinship            one point   per CROSS
#   traitmap  selected crosses by trait   one row     per CROSS
#   parents   parent use                  one bar     per LINE
#   dups      putative duplicates         one row/col per LINE
#
# So the shared state is a pair of sets, and every interaction writes both:
#
#   pick crosses -> those crosses, plus the lines that parent them
#   pick lines   -> those lines, plus the PLAN crosses that use them
#
# The line -> cross direction is restricted to the recommended plan: expanding
# across every scored candidate lit up most of the scatter for a commonly-used
# parent, which is technically correct and practically unreadable.
#
# WHY THE HIGHLIGHTING IS DONE IN JAVASCRIPT
# The first version re-rendered every figure from R whenever the selection
# changed. Server-side that worked - the render ran with the right selection -
# but the new value never reached the rendered widget, so the figures stayed
# frozen while the tables below updated correctly. Rather than keep fighting
# that, the figures are rendered exactly ONCE and never re-rendered: the browser
# applies the highlight itself with Plotly.restyle / Plotly.relayout. The
# selection is pushed back to Shiny (input$xp_sel) only so the tables can filter.
#
# Consequences worth knowing:
#   * highlighting is instant, and cannot desynchronise from what you clicked
#   * R never needs to know any figure's point order - the JS reads the keys off
#     the figures themselves (customdata, or the categorical axis)
#   * a new run re-renders the figures and clears the selection
#   * the selection is SESSION-ONLY and deliberately not persisted. It was
#     briefly stored in localStorage so a refresh kept it; restoring a
#     selection into a fresh session made recovering from a crash worse, not
#     better, so the selection now always starts empty.
#
# CONSISTENCY RULES
#   1. One visual language: selection always DIMS what is not selected, in every
#      figure. Never a highlight box in one place and dimming in another.
#   2. Three states, visibly distinct: nothing selected (nothing dimmed);
#      selected and matched here (the rest dimmed); selected but nothing here
#      matches (everything dimmed) - so "no match" never looks like "no
#      selection", which is what made the earlier attempt feel broken.
#   3. One clearing gesture: the Clear button. Plotly's double-click keeps its
#      usual meaning (reset zoom) rather than being overloaded.
# ===========================================================================

NGCD_EXPLORE_DIM <- 0.12                          # opacity of non-selected marks
NGCD_EXPLORE_SCRIM <- "rgba(255,255,255,0.72)"    # dims non-selected heatmap rows

# Stable per-cross key, shared by the figures, the JS layer and the tables.
# ASCII only, deliberately: a multiplication sign here triggers "strings not
# representable in native encoding" on a non-UTF-8 Windows locale, and this id
# is compared against table contents where a re-encoded copy would stop matching.
ngcd_xp_cross_id <- function(df) {
  if (!is.data.frame(df) || !nrow(df) || !all(c("parent1", "parent2") %in% names(df)))
    return(character(0))
  paste(df$parent1, df$parent2, sep = " x ")
}

# Per-trace selected/unselected styling. Set once at render; the JS only ever
# changes which points are selected, never how selection looks.
ngcd_xp_marker_states <- function() list(
  selected   = list(marker = list(opacity = 1)),
  unselected = list(marker = list(opacity = NGCD_EXPLORE_DIM)))

# --- figures (rendered once; the browser does the highlighting) ------------

ngcd_xp_scatter <- function(cc, sc) {
  if (!is.data.frame(cc) || !nrow(cc) || !all(c("multi_trait_score", "pair_kinship") %in% names(cc)))
    return(ngcd_chart_empty("No candidate crosses"))
  pal <- ngcd_chart_palette(); st <- ngcd_xp_marker_states()
  cid <- ngcd_xp_cross_id(cc); sid <- ngcd_xp_cross_id(sc)
  p <- plotly::plot_ly()
  p <- plotly::add_markers(p, x = cc$pair_kinship, y = cc$multi_trait_score,
    customdata = cid, text = cid, hoverinfo = "text", name = "All candidates",
    marker = list(color = "rgba(120,130,125,0.45)", size = 6),
    selected = st$selected, unselected = st$unselected)
  if (is.data.frame(sc) && nrow(sc) && all(c("multi_trait_score", "pair_kinship") %in% names(sc)))
    p <- plotly::add_markers(p, x = sc$pair_kinship, y = sc$multi_trait_score,
      customdata = sid, text = sid, hoverinfo = "text", name = "Selected",
      marker = list(color = pal$primary, size = 10),
      selected = st$selected, unselected = st$unselected)
  plotly::layout(p, margin = list(t = 30),
    xaxis = list(title = "Pairwise kinship (lower = more diverse)", gridcolor = pal$grid),
    yaxis = list(title = "Predicted cross score", gridcolor = pal$grid),
    legend = list(orientation = "h"))
}

ngcd_xp_parent_use <- function(sc) {
  if (!is.data.frame(sc) || !nrow(sc) || !all(c("parent1", "parent2") %in% names(sc)))
    return(ngcd_chart_empty("No selected crosses"))
  pal <- ngcd_chart_palette(); st <- ngcd_xp_marker_states()
  pu <- sort(table(c(sc$parent1, sc$parent2)))
  ids <- names(pu)
  plotly::layout(
    plotly::plot_ly(y = ids, x = as.integer(pu), customdata = ids,
      type = "bar", orientation = "h", hoverinfo = "x+y",
      marker = list(color = "#5b83a8"),
      selected = st$selected, unselected = st$unselected),
    margin = list(t = 30),
    xaxis = list(title = "Crosses in plan", gridcolor = pal$grid),
    yaxis = list(title = "", categoryorder = "array", categoryarray = ids))
}

# Row labels ARE the cross ids, so the JS reads them straight off the y axis -
# plotly drops 2D customdata on heatmaps, so there is nowhere else to put them.
# The y order is also the plan-cross list the JS uses to expand a line.
ngcd_xp_traitmap <- function(sc) {
  vcols <- if (is.data.frame(sc)) grep("_value$", names(sc), value = TRUE) else character(0)
  if (!length(vcols) || !nrow(sc)) return(ngcd_chart_empty("No per-trait values"))
  m <- as.matrix(sc[, vcols, drop = FALSE]); storage.mode(m) <- "double"
  m <- base::scale(m); m[!is.finite(m)] <- 0
  labs <- ngcd_xp_cross_id(sc); traits <- sub("_value$", "", vcols)
  plotly::layout(
    plotly::plot_ly(z = m, x = traits, y = labs, type = "heatmap",
      colorscale = "RdBu", reversescale = TRUE, colorbar = list(title = "z"),
      hovertemplate = "%{y}<br>%{x}<br>z %{z:.2f}<extra></extra>"),
    margin = list(t = 30), xaxis = list(title = ""), yaxis = list(title = ""))
}

ngcd_xp_dups <- function(pairs) {
  if (!is.data.frame(pairs) || !nrow(pairs)) return(ngcd_chart_empty("No putative duplicates flagged"))
  ab <- names(pairs)[1:2]
  simcol <- grep("ibs|sim|identity", tolower(names(pairs)))
  ids <- unique(c(pairs[[ab[1]]], pairs[[ab[2]]]))
  M <- matrix(NA_real_, length(ids), length(ids), dimnames = list(ids, ids)); diag(M) <- 1
  sv <- if (length(simcol)) pairs[[simcol[1]]] else rep(1, nrow(pairs))
  for (i in seq_len(nrow(pairs))) {
    a <- pairs[[ab[1]]][i]; b <- pairs[[ab[2]]][i]; M[a, b] <- sv[i]; M[b, a] <- sv[i]
  }
  plotly::layout(
    plotly::plot_ly(z = M, x = ids, y = ids, type = "heatmap",
      colorscale = list(c(0, "#f6e2d3"), c(1, "#c0392b")), colorbar = list(title = "Similarity"),
      hovertemplate = "%{y} vs %{x}<br>similarity %{z:.3f}<extra></extra>"),
    margin = list(t = 30), xaxis = list(title = "", tickangle = -40),
    yaxis = list(title = ""))
}

# --- the linking layer (browser side) --------------------------------------
# Binds to the four figures once they exist, translates any pick into the cross
# and line spaces, applies the dimming, updates the status bar, and reports the
# selection to Shiny as input$xp_sel so the tables can filter.
ngcd_xp_js <- function() sprintf('
(function(){
  var IDS = ["xp_scatter","xp_parents","xp_traitmap","xp_dups"];
  var SPACE = {xp_scatter:"cross", xp_traitmap:"cross", xp_parents:"line", xp_dups:"line"};
  var DIMSCRIM = "%s";
  var gd = {}, bound = false, sel = null;   // sel = null means "no selection"
  function get(id){ var d = document.getElementById(id); return (d && d.data && d.on) ? d : null; }
  function isHeat(id){ return id === "xp_traitmap" || id === "xp_dups"; }

  // Keys in figure order. Points carry customdata; heatmap rows are the y axis.
  function keys(id){
    var d = gd[id]; if (!d) return [];
    if (isHeat(id)) return (d.data[0] && d.data[0].y ? d.data[0].y.slice() : []);
    return d.data.map(function(t){ return (t.customdata || []).slice(); });
  }
  // Crosses in the recommended plan: the trait map row order, or failing that
  // the scatter\'s "Selected" trace. Used to expand a line without dragging in
  // every scored candidate.
  function planCrosses(){
    if (gd.xp_traitmap) { var y = keys("xp_traitmap"); if (y.length) return y; }
    if (gd.xp_scatter) { var k = keys("xp_scatter"); if (k.length > 1) return k[1] || []; }
    return [];
  }
  function parentsOf(cid){ var i = cid.lastIndexOf(" x "); return i < 0 ? [cid] : [cid.slice(0,i), cid.slice(i+3)]; }

  function derive(space, picked){
    var C = {}, L = {};
    if (space === "cross") {
      picked.forEach(function(cid){ C[cid] = 1; parentsOf(cid).forEach(function(p){ if(p) L[p]=1; }); });
    } else {
      picked.forEach(function(l){ L[l] = 1; });
      planCrosses().forEach(function(cid){
        if (parentsOf(cid).some(function(p){ return L[p]; })) C[cid] = 1; });
    }
    return { crosses: Object.keys(C), lines: Object.keys(L) };
  }

  function scrim(n, keepIdx, axis){
    var keep = {}; keepIdx.forEach(function(i){ keep[i] = 1; });
    var out = [];
    for (var i = 0; i < n; i++) { if (keep[i]) continue;
      var s = {type:"rect", line:{width:0}, fillcolor:DIMSCRIM, layer:"above"};
      if (axis === "y") { s.xref="paper"; s.x0=0; s.x1=1; s.yref="y"; s.y0=i-0.5; s.y1=i+0.5; }
      else              { s.yref="paper"; s.y0=0; s.y1=1; s.xref="x"; s.x0=i-0.5; s.x1=i+0.5; }
      out.push(s); }
    return out;
  }

  function apply(){
    IDS.forEach(function(id){
      var d = gd[id]; if (!d) return;
      var want = sel ? (SPACE[id] === "cross" ? sel.crosses : sel.lines) : null;
      var has = {}; (want || []).forEach(function(v){ has[v] = 1; });
      if (isHeat(id)) {
        var y = keys(id), hit = [];
        y.forEach(function(v,i){ if (has[v]) hit.push(i); });
        // no selection -> no scrim; selection with no match here -> scrim all
        var shp = !sel ? [] : (id === "xp_dups"
          ? scrim(y.length, hit, "y").concat(scrim(y.length, hit, "x"))
          : scrim(y.length, hit, "y"));
        Plotly.relayout(d, {shapes: shp});
      } else {
        var k = keys(id);
        var pts = k.map(function(arr){
          if (!sel) return null;
          var out = []; (arr||[]).forEach(function(v,i){ if (has[v]) out.push(i); });
          return out.length ? out : [arr ? arr.length : 0];   // out-of-range -> dim all
        });
        Plotly.restyle(d, {selectedpoints: pts}, d.data.map(function(_,i){ return i; }));
      }
    });
    var bar = document.getElementById("xp-bar");
    if (bar) bar.innerHTML = sel
      ? "<strong>" + sel.crosses.length + " cross(es), " + sel.lines.length +
        " line(s) selected</strong><span class=\\"muted\\">Tables below are filtered to this selection.</span>" +
        "<button type=\\"button\\" id=\\"xp-clear\\">Clear</button>"
      : "<span>No selection</span><span class=\\"muted\\">Click in any figure - the other three, and the tables below, follow.</span>";
    if (window.Shiny && Shiny.setInputValue)
      Shiny.setInputValue("xp_sel", sel ? sel : {crosses:[], lines:[]}, {priority:"event"});
  }

  function setSel(space, picked){
    picked = picked.filter(function(v,i,a){ return v != null && v !== "" && a.indexOf(v) === i; });
    sel = picked.length ? derive(space, picked) : null;
    apply();
  }

  function pick(id, pts){
    var out = [];
    (pts || []).forEach(function(p){
      if (isHeat(id)) { if (id === "xp_dups") { out.push(p.y); out.push(p.x); } else out.push(p.y); }
      else if (p.customdata != null) out.push(p.customdata);
    });
    return out;
  }

  function bind(){
    var ready = 0;
    IDS.forEach(function(id){ var d = get(id); if (d) { gd[id] = d; ready++; } });
    if (!ready) return false;
    IDS.forEach(function(id){
      var d = gd[id]; if (!d || d.__ngcdBound) return; d.__ngcdBound = true;
      d.on("plotly_click",    function(e){ setSel(SPACE[id], pick(id, e && e.points)); });
      d.on("plotly_selected", function(e){ setSel(SPACE[id], pick(id, e && e.points)); });
      d.on("plotly_deselect", function(){ sel = null; apply(); });
    });
    sel = null;
    bound = true; apply(); return true;
  }

  document.addEventListener("click", function(e){
    if (e.target && e.target.id === "xp-clear") { sel = null; apply(); }
  });

  // The figures arrive asynchronously (and again after every new run), so poll
  // until they exist, and re-bind whenever a figure is replaced.
  setInterval(function(){
    var stale = IDS.some(function(id){ var d = get(id); return d && d !== gd[id]; });
    if (!bound || stale) { gd = {}; IDS.forEach(function(id){ var d=get(id); if(d){ d.__ngcdBound=false; } }); bind(); }
  }, 1500);
})();
', NGCD_EXPLORE_SCRIM)

# --- UI --------------------------------------------------------------------
ngcd_explore_ui <- function() {
  card <- function(title, hint, out, h = "340px")
    shiny::div(class = "xp-card",
      shiny::h5(title, class = "xp-title"),
      shiny::div(class = "xp-hint", hint),
      plotly::plotlyOutput(out, height = h))
  shiny::tagList(
    shiny::tags$style(shiny::HTML(paste0(
      ".xp-card{width:49%;display:inline-block;vertical-align:top;border:1px solid #d7ded9;",
      "border-radius:6px;margin:0 0.5% 12px 0;padding:6px 4px;background:#fff}",
      ".xp-title{margin:6px 12px 0;font-size:15px;color:#00583d}",
      ".xp-hint{color:#5c6b66;font-size:12px;padding:2px 12px 6px}",
      "#xp-bar{position:sticky;top:0;z-index:30;display:flex;align-items:center;gap:14px;",
      "background:#00583d;color:#fff;padding:8px 14px;border-radius:6px;margin-bottom:12px}",
      "#xp-bar .muted{opacity:.85;font-size:13px}",
      "#xp-bar button{background:#fff;color:#00583d;border:0;border-radius:3px;",
      "padding:4px 10px;font:inherit;cursor:pointer}"))),
    shiny::div(id = "xp-bar"),
    shiny::div(
      card("Score vs diversity (kinship)",
           "Click a cross to select it, or use the Box / Lasso buttons in the chart toolbar to select several.", "xp_scatter"),
      card("Parent use in plan",
           "Click a parent to select it, or use the Box / Lasso buttons to select several.", "xp_parents"),
      card("Selected crosses by trait rank",
           "Click any cell to select that cross.", "xp_traitmap"),
      card("Putative duplicate similarity",
           "Click any cell to select both of those lines.", "xp_dups")),
    bslib::navset_tab(
      bslib::nav_panel("Selected crosses", DT::DTOutput("xp_tbl_selected")),
      bslib::nav_panel("Candidates",       DT::DTOutput("xp_tbl_candidates")),
      bslib::nav_panel("Parent use",       DT::DTOutput("xp_tbl_parents"))),
    shiny::tags$script(shiny::HTML(ngcd_xp_js())))
}

# --- server ----------------------------------------------------------------
# Figures render ONCE per run. Only the tables react to the selection, which
# arrives from the browser as input$xp_sel.
ngcd_explore_server <- function(input, output, session, res) {
  sc_r <- shiny::reactive({ r <- res(); shiny::req(r); r$selected_crosses })
  cc_r <- shiny::reactive({ r <- res(); shiny::req(r); r$candidate_crosses })
  dp_r <- shiny::reactive({
    r <- res(); shiny::req(r); qc <- r$qc %||% list()
    pd <- qc$putative_duplicates %||% qc$cleaning$putative_duplicates
    if (is.data.frame(pd$pairs) && nrow(pd$pairs)) pd$pairs else NULL
  })

  output$xp_scatter  <- plotly::renderPlotly({ ngcd_xp_scatter(cc_r(), sc_r()) })
  output$xp_parents  <- plotly::renderPlotly({ ngcd_xp_parent_use(sc_r()) })
  output$xp_traitmap <- plotly::renderPlotly({ ngcd_xp_traitmap(sc_r()) })
  output$xp_dups     <- plotly::renderPlotly({ ngcd_xp_dups(dp_r()) })

  # Debounced: a click, and especially a box-select, can arrive in bursts, and
  # each one re-renders a DT table. 250 ms collapses a burst into one render
  # without being perceptible.
  sel <- shiny::debounce(shiny::reactive({
    s <- input$xp_sel
    list(crosses = as.character(s$crosses %||% character(0)),
         lines   = as.character(s$lines   %||% character(0)))
  }), 250)

  filt <- function(df) {
    s <- sel(); if (!length(s$crosses) || !is.data.frame(df)) return(df)
    df[ngcd_xp_cross_id(df) %in% s$crosses, , drop = FALSE]
  }
  output$xp_tbl_selected   <- DT::renderDT({ ngcd_dt(filt(sc_r()), page = 10) })
  output$xp_tbl_candidates <- DT::renderDT({ ngcd_dt(filt(cc_r()), page = 10) })
  output$xp_tbl_parents    <- DT::renderDT({
    sc <- sc_r(); s <- sel()
    pu <- table(c(sc$parent1, sc$parent2))
    d <- data.frame(parent = names(pu), uses = as.integer(pu), row.names = NULL)
    d <- d[order(-d$uses), , drop = FALSE]
    if (length(s$lines)) d <- d[d$parent %in% s$lines, , drop = FALSE]
    ngcd_dt(d, page = 10)
  })

  # Explore is a hidden sub-tab: after a run the app lands on Results, whose
  # default sub-tab is Report, so these first render while display:none.
  # The FIGURES need this: Explore is a hidden sub-tab, so they first render
  # while display:none and would otherwise be suspended and never sent.
  # The TABLES deliberately do NOT: they sit in their own nav tabs, so leaving
  # suspension on means only the table you are looking at re-renders on a
  # selection change instead of all three (~0.4 s each).
  for (o in c("xp_scatter", "xp_parents", "xp_traitmap", "xp_dups"))
    shiny::outputOptions(output, o, suspendWhenHidden = FALSE)
}
