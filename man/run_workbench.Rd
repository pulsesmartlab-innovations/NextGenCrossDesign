\name{run_workbench}
\alias{run_workbench}
\title{Launch the NDSU Cross Design Workbench}
\usage{
run_workbench(dir = getwd(), launch.browser = interactive(),
  port = getOption("shiny.port"), host = "127.0.0.1")
}
\arguments{
\item{dir}{Writable working directory for \code{config.yml} and per-run outputs
(default: current directory).}
\item{launch.browser}{Open a browser automatically (default: interactive).}
\item{port}{Optional fixed port; \code{NULL} lets Shiny choose.}
\item{host}{Host to bind (default \code{"127.0.0.1"}).}
}
\value{Called for its side effect (runs the app).}
\description{
Starts the Shiny front-end. The backend (\pkg{nextgenCrossDesign}) runs in a
separate R process configured via \code{config.yml} in \code{dir}; if no config
file exists yet, a template is written for you to edit.
}
\examples{
\dontrun{
library(nextgenCrossWorkbench)
init_workbench_dir("~/cross-workbench")
run_workbench("~/cross-workbench")
}
}
