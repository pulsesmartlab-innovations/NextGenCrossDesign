# The backend capability registry is the single source of truth for what the UI
# exposes. Any choice the backend marks status = "experimental"/"guarded" must
# never surface in a dropdown (VALIDATED_STATE frontend-surfacing governance),
# even if the frontend's hardcoded fallback still lists it.

reg_with <- function(id, choices) {
  list(controls = list(list(id = id, choices = choices)))
}

test_that("registry choices merge into the fallback (baseline behavior)", {
  cc <- nextgenCrossWorkbench:::ngcd_control_choices
  reg <- reg_with("optimizer", list(
    list(value = "auto", label = "Auto"),
    list(value = "evolution", label = "Evolution (recommended)")))
  out <- cc(reg, "optimizer", c(Auto = "auto"))
  expect_true(all(c("auto", "evolution") %in% unname(out)))
  expect_equal(unname(out[["Auto"]]), "auto")          # fallback label kept
})

test_that("experimental / guarded registry choices are dropped", {
  cc <- nextgenCrossWorkbench:::ngcd_control_choices
  reg <- reg_with("progeny", list(
    list(value = "DH",  label = "Doubled haploid"),
    list(value = "RIL", label = "Recombinant inbred line"),
    list(value = "poly4x", label = "Autotetraploid", status = "experimental"),
    list(value = "guard", label = "Complex polyploid", status = "guarded")))
  out <- cc(reg, "progeny", NULL)
  expect_true(all(c("DH", "RIL") %in% unname(out)))
  expect_false("poly4x" %in% unname(out))              # experimental hidden
  expect_false("guard"  %in% unname(out))              # guarded hidden
})

test_that("registry status overrides a hardcoded fallback that still lists it", {
  cc <- nextgenCrossWorkbench:::ngcd_control_choices
  reg <- reg_with("progeny", list(
    list(value = "DH",     label = "Doubled haploid"),
    list(value = "poly4x", label = "Autotetraploid", status = "experimental")))
  # frontend fallback still names the now-experimental value
  out <- cc(reg, "progeny", c(`Doubled haploid` = "DH", Autotetraploid = "poly4x"))
  expect_true("DH" %in% unname(out))
  expect_false("poly4x" %in% unname(out))              # backend retraction wins
})

test_that("supported statuses and status-less choices are unaffected", {
  cc <- nextgenCrossWorkbench:::ngcd_control_choices
  reg <- reg_with("grm_method", list(
    list(value = "vanraden", label = "VanRaden", status = "default"),
    list(value = "yang",     label = "Yang",     status = "supported")))
  out <- cc(reg, "grm_method", c(VanRaden = "vanraden"))
  expect_true(all(c("vanraden", "yang") %in% unname(out)))
})
