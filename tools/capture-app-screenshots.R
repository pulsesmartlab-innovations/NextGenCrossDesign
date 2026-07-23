#!/usr/bin/env Rscript
# Capture real app-UI screenshots of the newer results screens (multi-trait joint
# P(superior), Pareto explorer, family sizes, disomic-subgenome) using shinytest2.
#
# Prereqs:
#   - nextgenCrossWorkbench + backend nextgenCrossDesign (>= 0.9.0) installed
#   - install.packages(c("chromote","shinytest2"))  (needs a Chrome/Chromium)
#   - a working dir WB containing config.yml (rscript_path + package_library) and an
#     app.R that calls nextgenCrossWorkbench::workbench_app(dir = ".")
#
# Usage:  WB=/path/to/workdir SHOTS=/path/to/out Rscript tools/capture-app-screenshots.R
#
# Notes: the backend runs synchronously (system2), so after clicking Run we wait a
# fixed interval rather than wait_for_idle (a status poll never lets the app go idle).
Sys.setenv(NOT_CRAN = "true", CI = "true")
library(shinytest2)
WB <- Sys.getenv("WB", unset = getwd()); OUT <- Sys.getenv("SHOTS", unset = tempdir())
snap <- function(app, f) app$get_screenshot(file.path(OUT, f))
clicktab <- function(app, label) {
  app$run_js(sprintf("var a=document.querySelector('a[data-value=\"%s\"]'); if(a) a.click();", label)); Sys.sleep(3)
}

# --- standard run with K=5 (creates gain-diversity tension -> a real Pareto frontier) ---
app <- AppDriver$new(WB, name = "std", width = 1460, height = 1120, load_timeout = 90000, timeout = 180000, seed = 1)
Sys.sleep(4)
app$set_inputs(data_source = "demo", workflow = "standard", wait_ = FALSE); Sys.sleep(5)
app$set_inputs(nav = "Objective", wait_ = FALSE); Sys.sleep(1)
app$set_inputs(multitrait_joint_prob = TRUE, multitrait_targets = "yield: 65\ndisease: 3", wait_ = FALSE); Sys.sleep(1)
app$set_inputs(nav = "Allocation", wait_ = FALSE); Sys.sleep(1)
app$set_inputs(cross_number_mode = "fixed", n_crosses = 5, family_size_total_progeny = 500,
               pareto_explore = TRUE, pareto_lambdas = "0,0.25,0.5,1,2,4", wait_ = FALSE); Sys.sleep(2)
app$set_inputs(nav = "Run", wait_ = FALSE); Sys.sleep(1); app$click("run", wait_ = FALSE); Sys.sleep(35)
app$set_inputs(nav = "Results", wait_ = FALSE); Sys.sleep(5)
snap(app, "screen-25-multitrait-joint.png")
clicktab(app, "Pareto explorer"); snap(app, "screen-24-pareto-explorer.png")
clicktab(app, "Family sizes");    snap(app, "screen-23-family-size.png")
app$stop()

# --- disomic-subgenome run (bundled subgenome demo) ---
app2 <- AppDriver$new(WB, name = "sg", width = 1460, height = 1120, load_timeout = 90000, timeout = 180000, seed = 1)
Sys.sleep(4)
app2$set_inputs(data_source = "demo", workflow = "subgenome", wait_ = FALSE); Sys.sleep(7)
app2$set_inputs(nav = "Run", wait_ = FALSE); Sys.sleep(1); app2$click("run", wait_ = FALSE); Sys.sleep(30)
app2$set_inputs(nav = "Results", wait_ = FALSE); Sys.sleep(5)
snap(app2, "screen-26-subgenome-results.png")
app2$stop()
cat("captured app screenshots to", OUT, "\n")
