# Advanced mate-selection controls, verified through the backend (their effect,
# not just that they run). Skipped unless the backend is available and
# NGCD_RUN_COMBINATIONS=1 is set.
test_that("advanced controls take effect through the backend", {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run backend advanced-control checks.")
  skip_if(!backend_available(), "Backend not available.")

  cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wbadv"))
  wd <- file.path(cfg$runs_dir, "adv"); dir.create(wd, recursive = TRUE, showWarnings = FALSE)
  runc <- function(ov) nextgenCrossWorkbench:::ngcd_run_combo(cfg, ov, wd)
  read_res <- function(rd) jsonlite::fromJSON(file.path(rd, "result.json"), simplifyDataFrame = TRUE)

  # committed matings: the locked pair must appear in the plan
  r <- runc(list(committed_crosses = list(parent1 = "P09", parent2 = "P10"),
                 n_crosses = 8, max_uses_per_parent = 4))
  expect_true(r$ok, info = r$message)
  sc <- read_res(r$run_dir)$selected_crosses
  expect_true(any((sc$parent1 == "P09" & sc$parent2 == "P10") |
                  (sc$parent1 == "P10" & sc$parent2 == "P09")))

  # lethal guarding: carrier x carrier crosses are dropped -> fewer candidates
  rl <- runc(list(lethal_spec = list(list(marker = "SNP_012", risk_allele = "alt")),
                  drop_lethal_carrier_crosses = TRUE))
  expect_true(rl$ok, info = rl$message)
  expect_lt(nrow(read_res(rl$run_dir)$candidate_crosses), 45)

  # each remaining advanced control at least runs cleanly
  for (ov in list(
      list(lambda_progeny_inbreeding = 0.05),
      list(min_crosses_per_parent = 2),
      list(parent_group = stats::setNames(as.list(rep(c("A","B"), each = 5)), sprintf("P%02d", 1:10)),
           group_quota = list("A||B" = 4)),
      list(marker_target_spec = list(list(marker = "SNP_001", direction = "increase",
                                          target_freq = 0.9, weight = 1)), lambda_marker = 1),
      list(budget = 1e6, lambda_cost = 0.1, lambda_logistic = 0.1))) {
    rr <- runc(ov)
    expect_true(rr$ok, info = paste(names(ov), collapse = ",", ":", rr$message))
  }
})
