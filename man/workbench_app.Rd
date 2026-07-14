\name{workbench_app}
\alias{workbench_app}
\title{Build the workbench Shiny app object}
\usage{
workbench_app(dir = getwd())
}
\arguments{
\item{dir}{Writable working directory holding \code{config.yml} and
\code{runs/} (default: current directory).}
}
\value{A \code{shiny.appobj}.}
\description{
Returns the Shiny app object. Used by \code{\link{run_workbench}} and by the
packaged \code{inst/app/app.R} so the RStudio "Run App" button works.
}
