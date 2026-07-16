# ===========================================================================
# config.R  -  load user configuration and resolve the R backend
# ===========================================================================
# In package form, the app's *resources* (runner script, demo data, CSS) live
# inside the installed package (system.file), while the *user* config.yml and
# per-run outputs live in a writable working directory the user chooses.

PKG <- "nextgenCrossWorkbench"

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L || (is.character(a) && !nzchar(a[1]))) b else a
}

# Path to a resource shipped inside the installed package.
ngcd_res <- function(...) system.file("app", ..., package = PKG)

# Load user config from <dir>/config.yml, applying NGCD_* env overrides and
# resolving package resource paths. `dir` is a writable folder for config +
# runs (defaults to the working directory).
ngcd_load_config <- function(dir = getwd()) {
  cfg_path <- file.path(dir, "config.yml")
  cfg <- list()
  if (file.exists(cfg_path) && requireNamespace("yaml", quietly = TRUE)) {
    parsed <- tryCatch(yaml::read_yaml(cfg_path), error = function(e) NULL)
    if (!is.null(parsed$default)) cfg <- parsed$default else cfg <- parsed %||% list()
  }

  defaults <- list(
    rscript_path             = "Rscript",
    package_library          = "",
    required_backend_version = "0.4.0",
    alphamate_executable     = "",
    run_timeout_seconds      = 1800,
    max_upload_mb            = 200,
    # Developer mode exposes the Setup screen and the Save/Load-settings
    # profile tools (and their .json import/export). Off by default so a
    # deployed app hides configuration and developer plumbing from end users.
    developer_mode           = FALSE
  )
  for (nm in names(defaults)) if (is.null(cfg[[nm]])) cfg[[nm]] <- defaults[[nm]]

  for (nm in names(cfg)) {
    ev <- Sys.getenv(paste0("NGCD_", toupper(nm)), unset = NA)
    if (!is.na(ev) && nzchar(ev)) cfg[[nm]] <- ev
  }

  # Coerce developer_mode to a single logical (yaml gives logical; an env var
  # or a "true"/"1"/"yes" string all count as TRUE).
  cfg$developer_mode <- isTRUE(cfg$developer_mode) ||
    (is.character(cfg$developer_mode) &&
       tolower(trimws(cfg$developer_mode)) %in% c("true", "1", "yes", "on"))

  cfg$work_dir      <- dir
  cfg$config_path   <- cfg_path
  cfg$runner_script <- ngcd_res("tools", "run_cross_prediction_json.R")
  cfg$demo_data_dir <- ngcd_res("data", "demo")
  cfg$www_dir       <- ngcd_res("www")
  cfg$runs_dir      <- file.path(dir, "runs")
  if (!dir.exists(cfg$runs_dir)) dir.create(cfg$runs_dir, recursive = TRUE, showWarnings = FALSE)
  cfg
}

ngcd_resolve_rscript <- function(cfg) {
  cand <- cfg$rscript_path %||% "Rscript"
  if (file.exists(cand)) return(normalizePath(cand))
  found <- Sys.which(cand)
  if (nzchar(found)) return(unname(found))
  NA_character_
}

# Probe the configured R + backend package in a child process.
ngcd_check_backend <- function(cfg) {
  rscript <- ngcd_resolve_rscript(cfg)
  out <- list(
    rscript_path = cfg$rscript_path, rscript_resolved = rscript,
    rscript_ok = !is.na(rscript), r_version = NA_character_,
    backend_installed = FALSE, backend_version = NA_character_, version_ok = FALSE,
    package_library = cfg$package_library, runner_exists = file.exists(cfg$runner_script),
    optional = list(), alphamate_executable = cfg$alphamate_executable %||% "",
    alphamate_ok = NA, supports_nselfing = FALSE, messages = character(0))
  if (is.na(rscript)) {
    out$messages <- c(out$messages, paste0(
      "Rscript not found at '", cfg$rscript_path,
      "'. Set rscript_path in config.yml to your R installation."))
    return(out)
  }
  if (!out$runner_exists)
    out$messages <- c(out$messages, paste0("Runner script missing: ", cfg$runner_script))

  libarg <- if (nzchar(cfg$package_library)) cfg$package_library else ""
  probe <- sprintf('lib <- "%s"; if (nzchar(lib)) .libPaths(c(lib, .libPaths())); ',
                   gsub("\\\\", "/", libarg))
  probe <- paste0(probe,
    'cat("RVER=", as.character(getRversion()), "\\n", sep=""); ',
    'ok <- requireNamespace("nextgenCrossDesign", quietly=TRUE); ',
    'cat("BACKEND=", ok, "\\n", sep=""); ',
    'if (ok) cat("BVER=", as.character(packageVersion("nextgenCrossDesign")), "\\n", sep=""); ',
    'for (p in c("lpSolve","PopVar","SimpleMating","genomicMateSelectR","openxlsx",',
    '"ggplot2","AlphaSimR","sommer","Matrix","mvtnorm","RhpcBLASctl")) ',
    'cat("OPT:", p, "=", requireNamespace(p, quietly=TRUE), "\\n", sep=""); ',
    'if (ok) cat("NSELFING=", "nselfing" %in% names(formals(nextgenCrossDesign::ng_run_cross_prediction)), "\\n", sep="")')

  res <- tryCatch(system2(rscript, c("-e", shQuote(probe)), stdout = TRUE, stderr = TRUE),
                  error = function(e) paste("ERR:", conditionMessage(e)))
  txt <- paste(res, collapse = "\n")

  if (grepl("RVER=", txt)) out$r_version <- sub(".*RVER=([0-9.]+).*", "\\1", txt)
  out$backend_installed <- grepl("BACKEND=TRUE", txt)
  if (grepl("BVER=", txt)) {
    out$backend_version <- sub(".*BVER=([0-9.]+).*", "\\1", txt)
    out$version_ok <- tryCatch(
      package_version(out$backend_version) >= package_version(cfg$required_backend_version),
      error = function(e) FALSE)
    if (!out$version_ok) out$messages <- c(out$messages, paste0(
      "Backend version ", out$backend_version, " is older than required ",
      cfg$required_backend_version, ". Upgrade with ",
      'remotes::install_github("pulsesmartlab-innovations/nextgenCrossDesignR@v',
      cfg$required_backend_version, '").'))
  }
  if (!out$backend_installed) out$messages <- c(out$messages,
    paste0("nextgenCrossDesign is not installed for the configured R. Install it with ",
           'remotes::install_github("pulsesmartlab-innovations/nextgenCrossDesignR@v',
           cfg$required_backend_version, '"), or set package_library in config.yml ',
           "to an R library that already has it."))

  # optional package availability
  opt_names <- regmatches(txt, gregexpr("OPT:[A-Za-z0-9]+=(TRUE|FALSE)", txt))[[1]]
  if (length(opt_names)) {
    for (kv in opt_names) {
      nm <- sub("OPT:([A-Za-z0-9]+)=.*", "\\1", kv)
      out$optional[[nm]] <- grepl("=TRUE$", kv)
    }
  }
  out$supports_nselfing <- grepl("NSELFING=TRUE", txt)
  # AlphaMate external executable (optional global path in config.yml)
  if (nzchar(out$alphamate_executable))
    out$alphamate_ok <- file.exists(out$alphamate_executable) ||
      nzchar(Sys.which(out$alphamate_executable))
  out
}
