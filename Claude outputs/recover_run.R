# ---------------------------------------------------------------------------
# Recover after a browser crash / closed tab.
#
# Nothing you need is lost: every run writes its full result and its exact input
# CSVs to disk before the browser ever sees them. What a crash costs you is only
# the Shiny SESSION - the loaded tables, the settings you picked, and the current
# Explore selection. The run itself is recoverable from the run folder.
#
# NOTE ON FILENAMES: the staged pipeline (the normal path - QC, fit, score,
# rank as separate steps) writes `stage_rank.json`. The single-shot path writes
# `result.json`. Look for either, newest first.
# ---------------------------------------------------------------------------

RUNS <- "C:/Users/MM/Documents/ngcd-workbench/ngcd-data/runs"

ngcd_runs <- function(runs_dir = RUNS) {
  f <- list.files(runs_dir, pattern = "^(stage_rank|result)\\.json$",
                  recursive = TRUE, full.names = TRUE)
  if (!length(f)) return(data.frame())
  info <- data.frame(path = f, mtime = file.mtime(f), stringsAsFactors = FALSE)
  info <- info[order(info$mtime, decreasing = TRUE), , drop = FALSE]
  info$run <- basename(dirname(info$path))
  # a stage_rank.json can record a FAILED run; only completed ones are usable
  info$ok <- vapply(info$path, function(p)
    isTRUE(tryCatch(jsonlite::fromJSON(p)$ok, error = function(e) NA)) ||
    is.null(tryCatch(jsonlite::fromJSON(p)$ok, error = function(e) NULL)),
    logical(1))
  info[, c("run", "ok", "mtime", "path")]
}

# 1. What have I got?
runs <- ngcd_runs()
print(utils::head(runs, 10))

# 2. Load the newest usable run
stopifnot("no completed run on disk" = nrow(runs) > 0)
res <- jsonlite::fromJSON(runs$path[runs$ok][1])
cat("recovered:", runs$run[runs$ok][1],
    "|", nrow(res$selected_crosses), "selected crosses,",
    nrow(res$candidate_crosses), "candidates\n")

# 3a. Rebuild the HTML report from it (no pipeline re-run)
ns  <- asNamespace("nextgenCrossWorkbench")
out <- file.path(path.expand("~"), "ngcd-recovered-report.html")
ns$ngcd_report_html(res, out)
cat("report:", out, "\n"); utils::browseURL(out)

# 3b. …or bring the linked Explore view back up on that run
recover_explore <- function(r = res) {
  library(shiny)
  shinyApp(ui = bslib::page_fluid(ns$ngcd_explore_ui()),
           server = function(input, output, session)
             ns$ngcd_explore_server(input, output, session, reactive(r)))
}
# recover_explore()

# 4. The exact inputs that produced a run, if you need to reproduce it in the app
run_inputs <- function(run_name) list.files(file.path(RUNS, run_name, "inputs"), full.names = TRUE)
# run_inputs(runs$run[1])
