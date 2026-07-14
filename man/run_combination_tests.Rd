\name{run_combination_tests}
\alias{run_combination_tests}
\title{Run a parameter-combination sweep against the backend}
\usage{
run_combination_tests(dir = getwd(), level = c("full", "smoke"),
  random = 0, workers = 1, seed = 1,
  verbose = TRUE, results_csv = file.path(dir, "combination_results.csv"))
}
\arguments{
\item{dir}{Working directory holding \code{config.yml} (default: current).}
\item{level}{\code{"full"} (one-at-a-time + factorial core) or \code{"smoke"}
(one-at-a-time only, faster).}
\item{random}{Number of additional random combinations spanning ALL parameters
to draw and run (0 = none).}
\item{workers}{Number of parallel worker processes (forking; Unix/macOS only,
ignored on Windows). Default 1.}
\item{seed}{Seed for the random combination sampler.}
\item{verbose}{Print progress and a summary.}
\item{results_csv}{Optional path to write the full results table.}
}
\value{A data.frame of results (varied, value, ok, message, n_selected,
seconds), invisibly.}
\description{
Executes many parameter combinations on the bundled demo data through the real
backend and reports which succeed or fail. A quick way to detect regressions
across the whole option surface.
}
\examples{
\dontrun{
res <- run_combination_tests("~/cross-workbench", level = "smoke")
subset(res, !ok)
}
}
