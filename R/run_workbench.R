# ===========================================================================
# run_workbench.R  -  public entry points
# ===========================================================================

# Ensure a writable working directory has a config.yml (copied from the
# packaged template) and a runs/ folder. Returns the config path invisibly.
#' Initialise a working directory for the workbench
#'
#' Copies the packaged \code{config.yml} template into \code{dir} (if not
#' already present) and creates a \code{runs/} folder. Edit the copied
#' \code{config.yml} to point at your local R and backend.
#'
#' @param dir Writable directory to initialise (default: current directory).
#' @return The path to the config file, invisibly.
#' @export
init_workbench_dir <- function(dir = getwd()) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  cfg_path <- file.path(dir, "config.yml")
  if (!file.exists(cfg_path)) {
    tmpl <- system.file("app", "config.template.yml", package = "nextgenCrossWorkbench")
    if (nzchar(tmpl) && file.exists(tmpl)) {
      file.copy(tmpl, cfg_path)
      message("Wrote ", cfg_path, " - edit rscript_path / package_library to match your R install.")
    }
  }
  runs <- file.path(dir, "runs")
  if (!dir.exists(runs)) dir.create(runs, recursive = TRUE, showWarnings = FALSE)
  invisible(cfg_path)
}

# Build the Shiny app object (used by run_workbench() and inst/app/app.R so the
# RStudio "Run App" button works).
#' Build the workbench Shiny app object
#'
#' @param dir Writable working directory holding \code{config.yml} and
#'   \code{runs/} (default: current directory).
#' @return A \code{shiny.appobj}.
#' @export
workbench_app <- function(dir = getwd()) {
  cfg <- ngcd_load_config(dir)
  shiny::addResourcePath("ngcd_www", cfg$www_dir)
  report_dir <- cfg$report_dir
  if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  shiny::addResourcePath("ngcd_report", report_dir)
  options(shiny.maxRequestSize = (cfg$max_upload_mb %||% 200) * 1024^2)
  # Developer mode comes from config (developer_mode) and can also be switched
  # on per-visit with a ?dev=1 URL query, so the UI is a function of the request.
  ui <- function(req) {
    qs  <- shiny::parseQueryString(req$QUERY_STRING %||% "")
    dev <- isTRUE(cfg$developer_mode) || identical(qs$dev, "1") || identical(qs$developer, "1")
    workbench_ui(cfg, dev = dev)
  }
  shiny::shinyApp(ui = ui, server = workbench_server(cfg))
}

#' Launch the NextGenCrossDesign app
#'
#' Starts the Shiny front-end. The backend (\pkg{nextgenCrossDesign}) is run in
#' a separate R process configured via \code{config.yml} in \code{dir}; if no
#' config file exists yet, a template is written for you to edit.
#'
#' @param dir Writable working directory for \code{config.yml} and per-run
#'   outputs (default: current directory).
#' @param launch.browser Open a browser automatically (default: interactive).
#' @param port Optional fixed port; \code{NULL} lets Shiny choose.
#' @param host Host to bind (default \code{"127.0.0.1"}).
#' @return Normally called for its side effect (runs the app). If it is called
#'   while a Shiny app is already running - e.g. RStudio's \strong{Run App}
#'   button sourced a file that calls \code{run_workbench()} - it does \emph{not}
#'   start a nested app; it returns the \code{shiny.appobj} so the outer runner
#'   can launch it, avoiding the "Can't call runApp() from within runApp()"
#'   error.
#' @examples
#' \dontrun{
#'   library(nextgenCrossWorkbench)
#'   init_workbench_dir("~/cross-workbench")  # first time: writes config.yml
#'   # edit ~/cross-workbench/config.yml, then, in the R Console:
#'   run_workbench("~/cross-workbench")
#' }
#' @export
run_workbench <- function(dir = getwd(), launch.browser = interactive(),
                          port = getOption("shiny.port"), host = "127.0.0.1") {
  init_workbench_dir(dir)
  app <- workbench_app(dir)
  # If a Shiny app is already running (RStudio "Run App" sources the file inside
  # its own runApp()), calling runApp() again errors with "Can't call runApp()
  # from within runApp()". Detect that and just hand back the app object - when
  # run_workbench() is the last expression of an app.R, RStudio launches it.
  already_running <- any(vapply(sys.calls(), function(cl)
    any(grepl("runApp", tryCatch(deparse(cl[[1]]), error = function(e) ""), fixed = TRUE)),
    logical(1)))
  if (already_running) return(app)
  shiny::runApp(app, launch.browser = launch.browser, port = port, host = host)
}
