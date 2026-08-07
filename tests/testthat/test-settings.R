# Save / load settings profiles.
library(shiny)

test_that("settings registry has no duplicate ids and known types", {
  reg <- nextgenCrossWorkbench:::ngcd_settings_registry()
  expect_false(any(duplicated(names(reg))))
  expect_true(all(reg %in% c("radio","slider","checkboxgroup","selectize",
                             "textarea","text","checkbox","select","num")))
})

test_that("collect -> apply round-trips input values", {
  srv <- nextgenCrossWorkbench:::workbench_server(
    nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb")))
  testServer(srv, {
    do.call(session$setInputs, demo_inputs(
      n_crosses = 17, optimizer = "evolution", trait_value_metric = "pmv",
      allocation_method = "alphamate_style", parent_type = "ril",
      selection_prop = 0.25, committed_crosses = "P09,P10", crop = "Barley",
      ploidy = "4", write_outputs = TRUE, diversity_mode = "emphasis"))
    saved <- nextgenCrossWorkbench:::ngcd_collect_settings(input)
    expect_equal(saved$n_crosses, 17)
    expect_equal(saved$optimizer, "evolution")
    expect_equal(saved$committed_crosses, "P09,P10")
    expect_equal(saved$parent_type, "ril")

    # write + read back through JSON to mimic a saved profile file
    tmp <- tempfile(fileext = ".json")
    jsonlite::write_json(list(schema = "ngcd_settings.v1", values = saved),
                         tmp, auto_unbox = TRUE, null = "null", na = "null")
    back <- jsonlite::fromJSON(tmp, simplifyVector = TRUE, simplifyDataFrame = FALSE)$values
    expect_equal(back$n_crosses, 17)
    expect_equal(back$trait_value_metric, "pmv")
    expect_equal(back$crop, "Barley")
    expect_equal(back$ploidy, "4")
    expect_equal(back$parent_type, "ril")
    expect_true(back$write_outputs)
  })
})
