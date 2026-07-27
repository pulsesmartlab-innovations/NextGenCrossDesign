ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")

test_that("objective_mode maps to backend params", {
  s <- ng("ngcd_objective_backend")
  expect_equal(s("single", "yield", NULL, NULL)$prediction_mode, "trait_by_trait")
  expect_equal(s("single", "yield", NULL, NULL)$traits_to_use, "yield")
  expect_false(s("single", "yield", NULL, NULL)$multi_trait_method_applies)

  expect_equal(s("multi", NULL, c("yield", "oil"), NULL)$prediction_mode, "trait_by_trait")
  expect_equal(s("multi", NULL, c("yield", "oil"), NULL)$traits_to_use, c("yield", "oil"))
  expect_true(s("multi", NULL, c("yield", "oil"), NULL)$multi_trait_method_applies)

  expect_equal(s("index", NULL, NULL, "idx")$prediction_mode, "index_as_trait")
  expect_null(s("index", NULL, NULL, "idx")$traits_to_use)
  expect_false(s("index", NULL, NULL, "idx")$multi_trait_method_applies)
})
