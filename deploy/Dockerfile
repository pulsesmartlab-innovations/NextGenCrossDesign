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
#   docker build -t ngcd-workbench:0.10.0 \
#     --build-arg BACKEND_TARBALL=nextgenCrossDesign_0.4.0.tar.gz \
#     --build-arg FRONTEND_TARBALL=nextgenCrossWorkbench_0.10.0.tar.gz \
#     -f deploy/Dockerfile .

# Pin a specific R version for reproducible deployments.
FROM rocker/r-ver:4.4.2

ARG BACKEND_TARBALL=nextgenCrossDesign_0.4.0.tar.gz
ARG FRONTEND_TARBALL=nextgenCrossWorkbench_0.10.0.tar.gz

# System libraries: a C++ toolchain for the backend kernel, plus the headers the
# R package dependencies need, and pandoc for HTML/PDF report rendering.
RUN apt-get update && apt-get install -y --no-install-recommends \
      build-essential \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libgit2-dev \
      pandoc \
      zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# CRAN dependencies. The frontend Imports + Suggests it needs at runtime, and the
# backend's hard dependencies (Rcpp, lpSolve). AlphaSimR etc. are backend
# Suggests used only for benchmarking and are NOT needed to serve the app.
RUN R -q -e "install.packages(c( \
      'shiny','bslib','DT','jsonlite','yaml','base64enc','plotly', \
      'knitr','rmarkdown','openxlsx', \
      'Rcpp','lpSolve' \
    ), repos='https://cloud.r-project.org')"

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

# Serve on all interfaces so ShinyProxy can reach the app; 3838 is ShinyProxy's
# default app port. run_workbench() is the package's launch entrypoint and calls
# shiny::runApp() itself with the given host/port.
EXPOSE 3838
CMD ["R", "-q", "-e", "nextgenCrossWorkbench::run_workbench(dir='/srv/workbench', host='0.0.0.0', port=3838, launch.browser=FALSE)"]
