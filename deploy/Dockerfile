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
# Build:
#   docker build -t ngcd-workbench:0.15.3 \
#     --build-arg BACKEND_TARBALL=nextgenCrossDesign_0.9.0.tar.gz \
#     --build-arg FRONTEND_TARBALL=nextgenCrossWorkbench_0.15.3.tar.gz \
#     -f deploy/Dockerfile .

# Pin a specific R version for reproducible deployments.
FROM rocker/r-ver:4.4.2

ARG BACKEND_TARBALL=nextgenCrossDesign_0.9.0.tar.gz
ARG FRONTEND_TARBALL=nextgenCrossWorkbench_0.15.3.tar.gz

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
# DESCRIPTION: as of backend 0.9.0 that is Rcpp, lpSolve, jsonlite, Matrix,
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
    && R CMD INSTALL frontend.tar.gz \
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
