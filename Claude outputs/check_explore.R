# ---------------------------------------------------------------------------
# Check the new Results > Explore (linked) tab.
#
# RESTART R FIRST (Ctrl+Shift+F10). Installing a package that is already
# loaded is what produced the "lazy-load database is corrupt" error earlier.
# ---------------------------------------------------------------------------

devtools::install("C:/Users/MM/Documents/GitHub/NextGenCrossDesign",
                  dependencies = FALSE, upgrade = FALSE)

# ===========================================================================
# OPTION A - quick check, no pipeline run (~10 seconds)
# Mounts just the Explore tab against your most recent completed run.
# ===========================================================================
check_explore_quick <- function() {
  library(shiny)
  ns <- asNamespace("nextgenCrossWorkbench")

  RUNS <- "C:/Users/MM/Documents/ngcd-workbench/ngcd-data/runs"
  hits <- list.files(RUNS, pattern = "^result\\.json$", recursive = TRUE, full.names = TRUE)
  stopifnot("no completed run found - use Option B below" = length(hits) > 0)
  res_path <- hits[order(file.mtime(hits), decreasing = TRUE)][1]
  message("using run: ", dirname(res_path))
  r <- jsonlite::fromJSON(res_path)

  message("selected crosses: ", nrow(r$selected_crosses),
          " | candidates: ",   nrow(r$candidate_crosses))
  qc <- r$qc
  pd <- if (!is.null(qc$putative_duplicates)) qc$putative_duplicates else qc$cleaning$putative_duplicates
  message("putative duplicates: ",
          if (is.data.frame(pd$pairs) && nrow(pd$pairs)) nrow(pd$pairs) else
            "none - that figure will show its empty state, which is correct")

  shinyApp(
    ui = bslib::page_fluid(ns$ngcd_explore_ui()),
    server = function(input, output, session)
      ns$ngcd_explore_server(input, output, session, reactive(r)))
}

# ===========================================================================
# OPTION B - the real thing: full app, fresh run
# Data tab defaults to the bundled demo (note: it is TETRAPLOID, so set
# Ploidy = 4 on Configure), then Run, then Results > Explore (linked).
# ===========================================================================
check_explore_full <- function() {
  wb <- file.path(path.expand("~"), "ngcd-workbench")
  dir.create(wb, showWarnings = FALSE, recursive = TRUE)
  nextgenCrossWorkbench::run_workbench(dir = wb)
}

# Run ONE of these:
check_explore_quick()
# check_explore_full()
