# ---------------------------------------------------------------------------
# Rebuild the HTML report from your most recent successful run and open it, so
# you can exercise the linked selection without re-running the pipeline.
#
# RESTART R FIRST (Ctrl+Shift+F10) - installing a package that is already
# loaded is what produced the "lazy-load database is corrupt" error earlier.
# ---------------------------------------------------------------------------

devtools::install("C:/Users/MM/Documents/GitHub/NextGenCrossDesign",
                  dependencies = FALSE, upgrade = FALSE)

RUNS <- "C:/Users/MM/Documents/ngcd-workbench/ngcd-data/runs"

# newest run that actually produced a result.json (failed runs have none)
hits <- list.files(RUNS, pattern = "^result\\.json$", recursive = TRUE, full.names = TRUE)
stopifnot("no completed run found - run the app once first" = length(hits) > 0)
res_path <- hits[order(file.mtime(hits), decreasing = TRUE)][1]
cat("using run:", dirname(res_path), "\n")

res <- jsonlite::fromJSON(res_path)

# Which of the four linked figures this run can actually show. A figure whose
# data is absent is omitted entirely, so a run with no flagged duplicates will
# legitimately have three linked figures rather than four.
ns  <- asNamespace("nextgenCrossWorkbench")
ids <- vapply(ns$ngcd_report_active(res), function(f) f$id, character(1))
cat("figures in this run :", paste(ids, collapse = ", "), "\n")
cat("linked figures      :", paste(intersect(ids, names(ns$NGCD_LINK_FIGS)), collapse = ", "), "\n")

out <- file.path(path.expand("~"), "ngcd-linked-report.html")
ns$ngcd_report_html(res, out)
cat("wrote:", out, sprintf("(%.1f MB)\n", file.size(out) / 1e6))
utils::browseURL(out)
