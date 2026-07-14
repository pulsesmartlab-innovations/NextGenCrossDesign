#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Standalone parameter-combination sweep.
#
#   Rscript run_combinations.R [work_dir] [level]
#     work_dir : folder holding config.yml (default: current directory)
#     level    : "full" (default) or "smoke"
#
# Runs many parameter combinations through the backend on the bundled demo
# data and prints which succeed / fail, writing combination_results.csv.
# ---------------------------------------------------------------------------
suppressMessages(library(nextgenCrossWorkbench))

args  <- commandArgs(trailingOnly = TRUE)
dir   <- if (length(args) >= 1) args[[1]] else getwd()
level <- if (length(args) >= 2) args[[2]] else "full"

res <- run_combination_tests(dir, level = level, verbose = TRUE)

# Non-zero exit only on UNEXPECTED failures. Infeasible-plan refusals (e.g.
# n_crosses beyond the parent pool's capacity) are expected and ignored.
failed <- res[isTRUE(res$unexpected_fail) | res$unexpected_fail, ]
if (nrow(failed)) {
  cat("\nUNEXPECTED FAILURES:\n")
  print(failed[, c("varied", "value", "message")])
  quit(status = 1)
}
cat(sprintf("\nNo unexpected failures (%d ok, %d expected infeasible).\n",
            sum(res$ok), sum(res$expected_fail)))
