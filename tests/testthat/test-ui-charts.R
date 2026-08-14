# Tests for the guided-redesign modelling charts (Task 8).

fixture <- function(n = 40, with_obs = TRUE) {
  set.seed(1)
  pred <- rnorm(n, 220, 45)
  se <- runif(n, 25, 35)
  data.frame(
    Name = sprintf("Sample-%03d", seq_len(n)),
    Trait = rep(c("B_glucan", "Protein"), length.out = n),
    Train_Test_Label = "Train",
    Predicted_value = pred,
    Observed_value = if (with_obs) pred + rnorm(n, 0, 30) else NA_real_,
    Standard_error = se,
    lower_bound = pred - 1.96 * se,
    upper_bound = pred + 1.96 * se,
    Reliability = pmin(0.95, pmax(0.55, runif(n, 0.55, 0.9))),
    stringsAsFactors = FALSE)
}

is_plotly <- function(x) inherits(x, c("plotly", "htmlwidget"))

test_that("every builder returns a plotly widget on good data", {
  df <- fixture()
  expect_true(is_plotly(ngcd_chart_pred_obs_hist(df)))
  expect_true(is_plotly(ngcd_chart_ridgeline(df)))
  expect_true(is_plotly(ngcd_chart_pred_confidence(df)))
  expect_true(is_plotly(ngcd_chart_pred_intervals(df)))
  perf <- data.frame(trait = c("B_glucan", "Protein"), model = c("BRR", "BayesA"),
                     root_mean_squared_error = c(57.2, 0.55))
  expect_true(is_plotly(ngcd_chart_perf_by_model(perf)))
})

test_that("builders are empty-safe (missing data / columns)", {
  expect_true(is_plotly(ngcd_chart_pred_obs_hist(NULL)))
  expect_true(is_plotly(ngcd_chart_pred_obs_hist(data.frame())))
  expect_true(is_plotly(ngcd_chart_pred_confidence(fixture()[, "Name", drop = FALSE])))
  expect_true(is_plotly(ngcd_chart_pred_intervals(data.frame(Predicted_value = 1))))  # no bounds
  expect_true(is_plotly(ngcd_chart_perf_by_model(data.frame(model = "BRR"))))          # no metric
})

test_that("predicted-only data still plots (no Observed column)", {
  df <- fixture(with_obs = FALSE)
  df$Observed_value <- NULL
  expect_true(is_plotly(ngcd_chart_pred_obs_hist(df)))
  expect_true(is_plotly(ngcd_chart_ridgeline(df)))
})

test_that("trait filter narrows the data used", {
  df <- fixture()
  # a trait with no rows -> empty-safe, still a widget
  expect_true(is_plotly(ngcd_chart_pred_obs_hist(df, trait = "Yield")))
  expect_true(is_plotly(ngcd_chart_pred_confidence(df, trait = "Protein")))
})

test_that("reliability banding thresholds are correct", {
  b <- ngcd_reliability_band(c(0.9, 0.7, 0.5))
  expect_equal(as.character(b), c("high", "moderate", "low"))
  expect_equal(levels(b), c("high", "moderate", "low"))
})

# --- workbench-native builders (real result schema) ------------------------
wb_cc <- function(n = 40) {
  set.seed(2)
  data.frame(
    parent1 = sprintf("P%02d", sample(10, n, replace = TRUE)),
    parent2 = sprintf("P%02d", sample(10, n, replace = TRUE)),
    multi_trait_score = rnorm(n, 1.2, 0.4),
    pair_kinship = runif(n, 0, 0.5),
    yield_value = rnorm(n, 220, 40),
    disease_value = rnorm(n, 3, 1),
    stringsAsFactors = FALSE)
}
wb_sc <- function(n = 12) {
  d <- wb_cc(n)
  d$cross_confidence <- runif(n, 0.5, 0.95)
  d$risk_bin <- sample(c("low", "moderate", "high"), n, replace = TRUE)
  d$priority_tier <- "tier_1"
  d
}

test_that("schema helpers pick the right columns", {
  cc <- wb_cc()
  expect_equal(ngcd_cross_score_col(cc), "multi_trait_score")
  expect_equal(ngcd_trait_value_cols(cc), c("yield_value", "disease_value"))
  expect_equal(ngcd_cross_score_col(data.frame(yield_value = 1:3)), "yield_value")
  expect_true(is.na(ngcd_cross_score_col(data.frame(x = 1))))
})

test_that("workbench-native builders return plotly on real-shaped data", {
  cc <- wb_cc(); sc <- wb_sc()
  expect_true(is_plotly(ngcd_chart_cross_scores(cc)))
  expect_true(is_plotly(ngcd_chart_cross_scores(cc, trait = "yield")))
  expect_true(is_plotly(ngcd_chart_cross_scores_ridge(cc)))
  expect_true(is_plotly(ngcd_chart_cross_confidence(sc)))
  expect_true(is_plotly(ngcd_chart_cross_diversity(cc, sc)))
})

test_that("workbench-native builders are empty-safe", {
  expect_true(is_plotly(ngcd_chart_cross_scores(NULL)))
  expect_true(is_plotly(ngcd_chart_cross_scores(data.frame())))
  expect_true(is_plotly(ngcd_chart_cross_scores_ridge(data.frame(multi_trait_score = 1:3))))  # no _value cols
  expect_true(is_plotly(ngcd_chart_cross_confidence(wb_cc())))          # no cross_confidence
  expect_true(is_plotly(ngcd_chart_cross_diversity(data.frame(x = 1)))) # missing cols
})

test_that("trait-model reliability chart renders and is empty-safe", {
  es <- data.frame(trait = c("yield", "disease"),
                   marker_effect_reliability = c(0.62, 0.81),
                   direction = c("maximize", "minimize"), stringsAsFactors = FALSE)
  expect_true(is_plotly(ngcd_chart_trait_reliability(es)))
  expect_true(is_plotly(ngcd_chart_trait_reliability(NULL)))
  expect_true(is_plotly(ngcd_chart_trait_reliability(data.frame(trait = "x"))))  # missing reliability col
})
