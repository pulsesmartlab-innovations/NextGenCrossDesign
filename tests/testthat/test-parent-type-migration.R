# The Parent-type selector (Inbred/DH/RIL) replaced the old "Assume inbred
# parents" checkbox. A settings.json saved before that rework carries the boolean
# `assume_inbred`; ngcd_migrate_parent_type_settings() maps it onto `parent_type`
# (matching ng_reconcile_parent_type in the backend) so a restored profile lands
# on a valid dropdown selection instead of leaving the control blank.

test_that("legacy assume_inbred migrates to parent_type", {
  mig <- nextgenCrossWorkbench:::ngcd_migrate_parent_type_settings
  expect_equal(mig(list(assume_inbred = TRUE))$parent_type, "inbred")   # fully fixed -> block het
  expect_equal(mig(list(assume_inbred = FALSE))$parent_type, "ril")     # accept residual het
  # the stale key is dropped so it can't drive the deprecated backend flag
  expect_null(mig(list(assume_inbred = TRUE))$assume_inbred)
})

test_that("an already-migrated profile passes through unchanged", {
  mig <- nextgenCrossWorkbench:::ngcd_migrate_parent_type_settings
  # parent_type present -> keep it, even if a stale assume_inbred also lingers
  s <- mig(list(parent_type = "dh", assume_inbred = FALSE))
  expect_equal(s$parent_type, "dh")
  expect_null(s$assume_inbred)
})

test_that("migration is a no-op on unrelated settings and NULL/empty input", {
  mig <- nextgenCrossWorkbench:::ngcd_migrate_parent_type_settings
  expect_null(mig(NULL))
  expect_equal(mig(list()), list())
  s <- mig(list(n_crosses = 10, optimizer = "evolution"))
  expect_equal(s$n_crosses, 10)
  expect_null(s$parent_type)      # nothing to migrate, no parent_type invented
})

test_that("parent_type is a restorable setting and rendered from the registry", {
  # registry drives the dropdown; a registry advertising parent_type surfaces its choices
  reg <- list(controls = list(list(id = "parent_type", choices = list(
    list(value = "inbred", label = "Inbred lines"),
    list(value = "dh",     label = "Doubled haploid (DH)"),
    list(value = "ril",    label = "Recombinant inbred line (RIL)")))))
  out <- nextgenCrossWorkbench:::ngcd_control_choices(reg, "parent_type", c(Inbred = "inbred"))
  expect_true(all(c("inbred", "dh", "ril") %in% unname(out)))
  # and it is in the save/restore registry as a select
  reg2 <- nextgenCrossWorkbench:::ngcd_settings_registry()
  expect_equal(reg2[["parent_type"]], "select")
  expect_false("assume_inbred" %in% names(reg2))  # retired
})
