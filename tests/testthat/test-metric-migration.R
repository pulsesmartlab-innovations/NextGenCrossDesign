# Legacy settings-profile migration for the breeder-intuitive metric names.
# A settings.json saved before the metric-names rework carries raw backend
# tokens (var_complex/pmv/vpm/mean); ngcd_migrate_metric_settings() maps those
# onto the current friendly vocabulary so a restored profile lands on a valid
# dropdown selection instead of leaving the control blank.

test_that("legacy metric presets migrate to friendly representation", {
  expect_equal(nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list(trait_value_metric = "var_complex"))$trait_value_metric, "usefulness")
  expect_equal(nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list(trait_value_metric = "var_complex"))$uc_variance_source, "reliable_family_variance")
  expect_equal(nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list(trait_value_metric = "pmv"))$trait_value_metric, "usefulness")
  expect_equal(nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list(trait_value_metric = "vpm"))$uc_variance_source, "family_variance")
  expect_equal(nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list(trait_value_metric = "mean"))$trait_value_metric, "mid_parent_mean")
  # already-friendly passes through
  expect_equal(nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list(trait_value_metric = "usefulness"))$trait_value_metric, "usefulness")
})

test_that("legacy pmv/vpm also migrate when they appear as uc_variance_source", {
  expect_equal(nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list(uc_variance_source = "pmv"))$uc_variance_source, "reliable_family_variance")
  expect_equal(nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list(uc_variance_source = "vpm"))$uc_variance_source, "family_variance")
  # already-friendly passes through
  expect_equal(nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list(uc_variance_source = "family_variance"))$uc_variance_source, "family_variance")
})

test_that("var_complex/pmv restore as usefulness + reliable, preserving what the user saw", {
  s <- nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list(trait_value_metric = "pmv", uc_variance_source = "vpm"))
  # trait_value_metric drives the mapping; the deterministic reliable_family_variance
  # wins over whatever (unrelated/stale) uc_variance_source value was also saved.
  expect_equal(s$trait_value_metric, "usefulness")
  expect_equal(s$uc_variance_source, "reliable_family_variance")
})

test_that("migration is a no-op on other settings and on NULL/empty input", {
  expect_null(nextgenCrossWorkbench:::ngcd_migrate_metric_settings(NULL))
  expect_equal(nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list()), list())
  s <- nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list(n_crosses = 10, optimizer = "evolution"))
  expect_equal(s$n_crosses, 10)
  expect_equal(s$optimizer, "evolution")
})

test_that("parent_distance and mid_parent_mean pass through untouched", {
  expect_equal(nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list(trait_value_metric = "parent_distance"))$trait_value_metric, "parent_distance")
  expect_equal(nextgenCrossWorkbench:::ngcd_migrate_metric_settings(list(trait_value_metric = "mid_parent_mean"))$trait_value_metric, "mid_parent_mean")
})

test_that("ngcd_metric_label() gives a friendly display name for raw and friendly tokens", {
  expect_equal(nextgenCrossWorkbench:::ngcd_metric_label("var_complex"), "Usefulness")
  expect_equal(nextgenCrossWorkbench:::ngcd_metric_label("usefulness"), "Usefulness")
  expect_equal(nextgenCrossWorkbench:::ngcd_metric_label("mean"), "Mid-parent mean")
  expect_equal(nextgenCrossWorkbench:::ngcd_metric_label(NULL), "?")
})
