# ===========================================================================
# report_link.R  -  cross-figure linked selection for the interactive HTML
#   report (brushing / "one-to-one" click selection across figures).
#
# The four linkable figures do not share a unit of analysis:
#
#   scatter   "Selected vs all candidates"      one point  per CROSS
#   traitmap  "Selected crosses by trait rank"  one row    per CROSS
#   parents   "Parent use"                      one bar    per LINE
#   dups      "Putative duplicate similarity"   one row/col per LINE
#
# So linking is not a single shared key: it is a translation between the CROSS
# space and the LINE space, in both directions.
#
#   select crosses -> highlight those crosses, and the lines that parent them
#   select lines   -> highlight those lines, and every cross that uses one
#
# Rather than re-deriving each figure's point order in R (which would silently
# drift the moment a figure changes), the keys ride along inside the figure
# itself: the scatter carries "<parent1>US<parent2>" (US = ASCII 31) per point in
# customdata, and the two bar/heatmap axes already hold line ids or cross
# labels as categories. Only the trait-heatmap row ids have to be emitted
# separately, because plotly drops 2D customdata on heatmaps.
# ===========================================================================

# Unit separator: joins the two parents into one cross id. Chosen because it
# cannot occur in a genotype name, so JS can split the id back apart safely.
NGCD_LINK_SEP <- intToUtf8(31L)  # ASCII unit separator

# The ids the JS layer needs that it cannot read off the rendered figures.
# `rows` is the trait-heatmap's row order, which must match the `labs` vector
# built in ngcd_ply_trait_heatmap (both are selected_crosses in row order).
ngcd_link_model <- function(res, ids) {
  sc <- res$selected_crosses
  rows <- if (is.data.frame(sc) && all(c("parent1", "parent2") %in% names(sc)))
    paste(sc$parent1, sc$parent2, sep = NGCD_LINK_SEP) else character(0)
  jsonlite::toJSON(list(sep = NGCD_LINK_SEP, figs = as.list(ids),
                        traitmapRows = rows),
                   auto_unbox = TRUE, null = "null")
}

# Which figures participate, and in which space. `source` tells the JS layer
# where a point's key lives: customdata (scatter), or the categorical axis.
NGCD_LINK_FIGS <- list(
  scatter  = list(space = "cross", source = "customdata"),
  traitmap = list(space = "cross", source = "rows"),
  parents  = list(space = "line",  source = "y"),
  dups     = list(space = "line",  source = "xy"))

# Figures where box/lasso brushing is meaningful. Heatmaps are click-only:
# plotly.js does not emit plotly_selected for heatmap traces, so a lasso over
# one would silently do nothing - worse than not offering the tool.
NGCD_LINK_BRUSHABLE <- c("scatter", "parents")

ngcd_link_toolbar_html <- function() paste0(
  "<div id='ngcd-link-bar' class='ngcd-linkbar' hidden>",
  "<span id='ngcd-link-msg'></span>",
  "<button type='button' id='ngcd-link-clear'>Clear selection</button>",
  "</div>")

ngcd_link_css <- function() paste0(
  ".ngcd-linkbar{position:sticky;top:0;z-index:20;display:flex;gap:12px;",
  "align-items:center;background:#00583d;color:#fff;padding:8px 14px;",
  "border-radius:4px;margin:12px 0;font-size:14px}",
  ".ngcd-linkbar button{background:#fff;color:#00583d;border:0;border-radius:3px;",
  "padding:4px 10px;font:inherit;cursor:pointer}",
  ".ngcd-linkhint{color:#5c6b66;font-size:13px;margin:2px 0 8px 0}")

# The linking layer itself. Kept as one string so the report stays a single
# self-contained file with no external assets.
ngcd_link_js <- function() '
(function(){
  var M = window.NGCD_LINK; if (!M) return;
  var SEP = M.sep, FIGS = M.figs;
  var gd = {}, keys = {}, sel = {crosses:null, lines:null}, applying = false;

  function el(id){ var d = document.getElementById("plot_"+id); return (d && d.on) ? d : null; }
  function parents(cid){ return cid.split(SEP); }

  // Per-figure key arrays: keys[id] is an array (per trace, for the scatter) or
  // a flat array of ids in point/category order.
  function readKeys(id, spec){
    var d = gd[id]; if (!d) return null;
    if (spec.source === "customdata")
      return d.data.map(function(t){ return (t.customdata || []).slice(); });
    if (spec.source === "rows") return M.traitmapRows.slice();
    if (spec.source === "y")    return (d.data[0].y || []).slice();
    if (spec.source === "xy")   return (d.data[0].y || []).slice();
    return null;
  }

  // Translate whatever was selected into both spaces. Selecting crosses lights
  // up their parent lines; selecting lines lights up every cross using them.
  function derive(space, picked){
    var crosses = new Set(), lines = new Set();
    if (space === "cross") {
      picked.forEach(function(cid){
        crosses.add(cid);
        parents(cid).forEach(function(p){ if (p) lines.add(p); });
      });
    } else {
      picked.forEach(function(l){ lines.add(l); });
      Object.keys(FIGS).forEach(function(id){
        if (FIGS[id].space !== "cross" || !keys[id]) return;
        var arrs = (FIGS[id].source === "customdata") ? keys[id] : [keys[id]];
        arrs.forEach(function(a){ (a||[]).forEach(function(cid){
          if (!cid) return;
          if (parents(cid).some(function(p){ return lines.has(p); })) crosses.add(cid);
        }); });
      });
    }
    return {crosses:crosses, lines:lines};
  }

  function bandShapes(idx, axis){
    return idx.map(function(i){
      var s = {type:"rect", line:{width:0}, fillcolor:"rgba(0,88,61,0.16)", layer:"above"};
      if (axis === "y") { s.xref="paper"; s.x0=0; s.x1=1; s.yref="y"; s.y0=i-0.5; s.y1=i+0.5; }
      else              { s.yref="paper"; s.y0=0; s.y1=1; s.xref="x"; s.x0=i-0.5; s.x1=i+0.5; }
      return s;
    });
  }

  function apply(){
    applying = true;
    Object.keys(FIGS).forEach(function(id){
      var d = gd[id], spec = FIGS[id], k = keys[id]; if (!d || !k) return;
      var want = (spec.space === "cross") ? sel.crosses : sel.lines;
      var isHeat = (spec.source === "rows" || spec.source === "xy");
      if (!want) {
        if (isHeat) Plotly.relayout(d, {shapes: []});
        else Plotly.restyle(d, {selectedpoints: [null]}, d.data.map(function(_,i){return i;}));
        return;
      }
      if (spec.source === "customdata") {
        var pts = k.map(function(arr){
          var out = []; (arr||[]).forEach(function(cid,i){ if (want.has(cid)) out.push(i); }); return out;
        });
        Plotly.restyle(d, {selectedpoints: pts}, d.data.map(function(_,i){return i;}));
      } else if (spec.source === "y") {
        var idx = []; k.forEach(function(v,i){ if (want.has(v)) idx.push(i); });
        Plotly.restyle(d, {selectedpoints: [idx]}, [0]);
      } else {
        var hit = []; k.forEach(function(v,i){ if (want.has(v)) hit.push(i); });
        var shp = (spec.source === "rows") ? bandShapes(hit, "y")
                                          : bandShapes(hit, "y").concat(bandShapes(hit, "x"));
        Plotly.relayout(d, {shapes: shp});
      }
    });
    var bar = document.getElementById("ngcd-link-bar");
    var msg = document.getElementById("ngcd-link-msg");
    if (bar && msg) {
      if (!sel.crosses && !sel.lines) { bar.hidden = true; }
      else {
        bar.hidden = false;
        msg.textContent = "Linked selection: " + (sel.crosses ? sel.crosses.size : 0) +
          " cross(es), " + (sel.lines ? sel.lines.size : 0) + " line(s)";
      }
    }
    applying = false;
  }

  function setFrom(space, picked){
    if (!picked.length) { sel = {crosses:null, lines:null}; apply(); return; }
    var r = derive(space, picked); sel = {crosses:r.crosses, lines:r.lines}; apply();
  }
  function clear(){ sel = {crosses:null, lines:null}; apply(); }

  // Pull the ids out of an event payload for a given figure.
  function picked(id, pts){
    var spec = FIGS[id], k = keys[id], out = [];
    (pts||[]).forEach(function(p){
      if (spec.source === "customdata") { if (p.customdata) out.push(p.customdata); }
      else if (spec.source === "rows")  { if (k && k[p.pointIndex[0]] !== undefined) out.push(k[p.pointIndex[0]]); }
      else if (spec.source === "y")     { out.push(p.y); }
      else                              { out.push(p.y); out.push(p.x); }
    });
    return out.filter(function(v,i,a){ return v != null && a.indexOf(v) === i; });
  }

  Object.keys(FIGS).forEach(function(id){
    var d = el(id); if (!d) return;
    gd[id] = d; keys[id] = readKeys(id, FIGS[id]);
    d.on("plotly_selected", function(e){ if (applying) return; setFrom(FIGS[id].space, picked(id, e && e.points)); });
    d.on("plotly_click",    function(e){ if (applying) return; setFrom(FIGS[id].space, picked(id, e && e.points)); });
    d.on("plotly_deselect",    function(){ if (!applying) clear(); });
    d.on("plotly_doubleclick", function(){ if (!applying) clear(); });
  });

  var btn = document.getElementById("ngcd-link-clear");
  if (btn) btn.addEventListener("click", clear);
})();
'
