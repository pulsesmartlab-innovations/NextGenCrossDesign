#!/usr/bin/env Rscript

# update-backend.R -- refresh the nextgenCrossDesign backend the workbench needs.
#
# The workbench runs the backend out-of-process (see SystemRequirements), so the
# backend is a separately installed R package, not an automatic dependency. Run
# this whenever the backend publishes a new tagged release, and the workbench
# picks up the change on its next run -- it calls the backend by name and filters
# arguments against the installed backend's live formals, so no app edit is
# needed for most backend updates.
#
# Usage:
#   Rscript tools/update-backend.R              # install the required version (below)
#   Rscript tools/update-backend.R 0.4.1        # install a specific version
#   Rscript tools/update-backend.R v0.4.1       # 'v' prefix is accepted too
#
# Install source (in order of precedence):
#   NGCD_BACKEND_TARBALL=/path/to/pkg.tar.gz    # install a local source tarball
#   (otherwise)                                 # install_github the pinned tag
#
# Private backend repo: install_github needs a GitHub token with repo read scope.
#   Set it first, e.g.  Sys.setenv(GITHUB_PAT = "ghp_xxx")  (GITHUB_TOKEN works too)

BACKEND_PKG  <- "nextgenCrossDesign"
BACKEND_REPO <- "pulsesmartlab-innovations/nextgenCrossDesignR"

# --- resolve the target version -------------------------------------------------
# Priority: CLI arg > NGCD_BACKEND_VERSION > config.template.yml > fallback.
read_required_version <- function() {
  candidates <- c(
    file.path("inst", "app", "config.template.yml"),               # git checkout
    system.file("app", "config.template.yml", package = "nextgenCrossWorkbench")  # installed
  )
  for (p in candidates) {
    if (nzchar(p) && file.exists(p)) {
      line <- grep("required_backend_version", readLines(p, warn = FALSE), value = TRUE)
      if (length(line)) {
        v <- sub('.*required_backend_version:\\s*"?([0-9][0-9.]*)"?.*', "\\1", line[[1]])
        if (nzchar(v) && grepl("^[0-9]", v)) return(v)
      }
    }
  }
  NA_character_
}

args    <- commandArgs(trailingOnly = TRUE)
env_ver <- Sys.getenv("NGCD_BACKEND_VERSION", unset = "")
version <- if (length(args) && nzchar(args[[1]])) {
  args[[1]]
} else if (nzchar(env_ver)) {
  env_ver
} else {
  read_required_version()
}
if (is.na(version) || !nzchar(version)) {
  stop("Could not determine a backend version. Pass one explicitly, e.g. ",
       "Rscript tools/update-backend.R 0.4.1", call. = FALSE)
}
version <- sub("^v", "", version)          # normalise: store bare "0.4.1"
tag     <- paste0("v", version)            # git tags are v-prefixed

# --- install --------------------------------------------------------------------
tarball <- Sys.getenv("NGCD_BACKEND_TARBALL", unset = "")
if (nzchar(tarball)) {
  if (!file.exists(tarball)) stop("NGCD_BACKEND_TARBALL not found: ", tarball, call. = FALSE)
  message("Installing ", BACKEND_PKG, " from local tarball: ", tarball)
  install.packages(tarball, repos = NULL, type = "source")
} else {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop("The 'remotes' package is required for install_github. ",
         "Install it with install.packages(\"remotes\"), or set ",
         "NGCD_BACKEND_TARBALL to install from a local tarball instead.", call. = FALSE)
  }
  ref <- paste0(BACKEND_REPO, "@", tag)
  message("Installing ", BACKEND_PKG, " from GitHub: ", ref)
  message("  (private repo: set GITHUB_PAT / GITHUB_TOKEN if this fails on auth)")
  remotes::install_github(ref, upgrade = "never")
}

# --- verify ---------------------------------------------------------------------
if (!requireNamespace(BACKEND_PKG, quietly = TRUE)) {
  stop("Install ran but ", BACKEND_PKG, " is still not loadable. Check the log above.",
       call. = FALSE)
}
installed <- as.character(utils::packageVersion(BACKEND_PKG))
ok <- utils::compareVersion(installed, version) >= 0
message("")
message("Installed ", BACKEND_PKG, " ", installed,
        " (required >= ", version, "): ", if (ok) "OK" else "STILL BELOW REQUIRED")
if (!ok) {
  stop("Installed version ", installed, " is older than the required ", version,
       ". Check that the tag ", tag, " exists on ", BACKEND_REPO, ".", call. = FALSE)
}
message("The workbench will use this backend on its next run.")
