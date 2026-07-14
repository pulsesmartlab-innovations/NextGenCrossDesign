\name{init_workbench_dir}
\alias{init_workbench_dir}
\title{Initialise a working directory for the workbench}
\usage{
init_workbench_dir(dir = getwd())
}
\arguments{
\item{dir}{Writable directory to initialise (default: current directory).}
}
\value{The path to the config file, invisibly.}
\description{
Copies the packaged \code{config.yml} template into \code{dir} (if not already
present) and creates a \code{runs/} folder. Edit the copied \code{config.yml}
to point at your local R and backend.
}
