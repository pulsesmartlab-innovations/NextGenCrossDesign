# NextGenCrossDesign workbench — container image for ShinyProxy (one per user).
#
# The image bundles BOTH packages, because the workbench runs the backend as a
# subprocess (Rscript) in the same container:
#   - nextgenCrossDesign   (backend engine; compiles C++ via Rcpp)
#   - nextgenCrossWorkbench (this Shiny front-end)
#
# Build context expects two source tarballs beside this Dockerfile:
#   nextgenCrossDesign_<ver>.tar.gz     (build from the backend repo: R CMD build .)
#   nextgenCrossWorkbench_<ver>.tar.gz  (build from this repo:        R CMD build .)
# Tarballs avoid putting a GitHub token in the image for the private backend repo.
#
# The minimum backend version the front-end actually requires is NOT hardcoded
# here: it lives in one place, inst/BACKEND_VERSION (read at runtime by
# R/config.R::ngcd_default_backend_version()). This build reads that same file
# and fails fast if the backend tarball you're bundling is older than it, so
# the two repos can never silently drift apart again. Bump inst/BACKEND_VERSION
# when the required backend version changes; nothing else needs to change.
#
# Build (both ARGs are required — no baked-in version defaults):
#   docker build -t ngcd-workbench:<frontend-ver> \
#     --build-arg BACKEND_TARBALL=nextgenCrossDesign_<backend-ver>.tar.gz \
#     --build-arg FRONTEND_TARBALL=nextgenCrossWorkbench_<frontend-ver>.tar.gz \
#     -f deploy/Dockerfile .

# Pin a specific R version for reproducible deployments.
FROM rocker/r-ver:4.4.2

ARG BACKEND_TARBALL
ARG FRONTEND_TARBALL

# System libraries: a C++ toolchain for the backend kernel, plus the headers the
# R package dependencies need, and pandoc for HTML/PDF report rendering.
RUN apt-get update && apt-get install -y --no-install-recommends \
      build-essential \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libgit2-dev \
      libuv1-dev \
      pandoc \
      zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# CRAN dependencies. The frontend Imports + Suggests it needs at runtime, and the
# backend's hard dependencies. AlphaSimR etc. are backend Suggests used only for
# benchmarking and are NOT needed to serve the app.
#
# The backend Imports block MUST stay in sync with nextgenCrossDesign's
# DESCRIPTION: as of backend 0.17.2 that is Rcpp, lpSolve, jsonlite, Matrix,
# mvtnorm, ggplot2, openxlsx (plus base/recommended parallel + Matrix that ship
# with R). mvtnorm/ggplot2/Matrix were promoted from Suggests to Imports in
# 0.7.0, so they are now required at `R CMD INSTALL` time, not just at runtime.
#
# install.packages() does NOT set a non-zero exit status when a package fails to
# build, so verify every package is actually installed and stop the build if not
# — otherwise a failed dependency only surfaces later as a confusing
# "R CMD INSTALL" error.
RUN R -q -e "pkgs <- c( \
      'shiny','bslib','DT','jsonlite','yaml','base64enc','plotly', \
      'knitr','rmarkdown','openxlsx', \
      'Rcpp','lpSolve','Matrix','mvtnorm','ggplot2'); \
    install.packages(pkgs, repos='https://cloud.r-project.org'); \
    missing <- pkgs[!pkgs %in% rownames(installed.packages())]; \
    if (length(missing)) stop('failed to install: ', paste(missing, collapse=', '))"

# Install the two packages from the tarballs copied into the build context.
# Backend first (frontend's SystemRequirements needs it reachable).
WORKDIR /tmp/pkg
COPY ${BACKEND_TARBALL}  backend.tar.gz
COPY ${FRONTEND_TARBALL} frontend.tar.gz
RUN R CMD INSTALL backend.tar.gz \
    && R CMD INSTALL frontend.tar.gz

# Fail the build if the bundled backend tarball is older than what this
# front-end build requires. The required version is read straight out of the
# just-installed frontend package (inst/BACKEND_VERSION), so this check stays
# correct automatically whenever that one file is bumped — no version literal
# to keep in sync here.
RUN R -q -e "req <- trimws(readLines(system.file('BACKEND_VERSION', package = 'nextgenCrossWorkbench'), n = 1)); \
    got <- as.character(packageVersion('nextgenCrossDesign')); \
    if (package_version(got) < package_version(req)) \
      stop('Bundled backend ', got, ' is older than the required ', req, \
           ' (see inst/BACKEND_VERSION in the front-end repo).') else \
      cat('Backend version check OK: ', got, ' >= ', req, '\n', sep = '')" \
    && rm -rf /tmp/pkg

# Working directory the app runs from (holds config.yml). data_dir defaults to a
# per-session temp dir, which is cleared when the container ends — the correct
# within-session lifetime for a hosted, one-container-per-user deployment.
RUN mkdir -p /srv/workbench
COPY deploy/config.yml /srv/workbench/config.yml

# This is a hosted, multi-user deployment: force server mode (ephemeral
# per-session storage) even if config.yml is replaced with a mounted one.
ENV NGCD_DEPLOYMENT_MODE=server

# Serve on all interfaces so ShinyProxy can reach the app; 3838 is ShinyProxy's
# default app port. run_workbench() is the package's launch entrypoint and calls
# shiny::runApp() itself with the given host/port.
EXPOSE 3838
CMD ["R", "-q", "-e", "nextgenCrossWorkbench::run_workbench(dir='/srv/workbench', host='0.0.0.0', port=3838, launch.browser=FALSE)"]
