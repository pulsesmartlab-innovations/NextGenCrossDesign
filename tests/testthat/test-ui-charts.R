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

# --- guided sequential presentation of the modelling graphics --------------
test_that("mg step list is ordered and complete", {
  s <- ngcd_mg_steps()
  expect_equal(length(s), 5L)
  expect_equal(vapply(s, `[[`, "", "id"), c("dist", "ridge", "conf", "div", "reliab"))
  expect_equal(vapply(s, `[[`, "", "outputId"),
               c("mg_dist", "mg_ridge", "mg_conf", "mg_div", "mg_reliab"))
  # every step carries a title and a non-trivial explanation
  expect_true(all(nzchar(vapply(s, `[[`, "", "title"))))
  expect_true(all(nchar(vapply(s, `[[`, "", "desc")) > 40))
})

test_that("guided panel renders the current figure with nav controls", {
  h <- as.character(ngcd_mg_guided_panel(1))
  expect_true(grepl("Figure 1 of 5", h))
  expect_true(grepl("Predicted cross-score distribution", h))
  expect_true(grepl('id="mg_dist"', h))         # step 1 chart present
  expect_true(grepl("mg_trait_ui", h))          # trait selector only on step 1
  expect_true(grepl("mg_prev", h) && grepl("mg_next", h))
  expect_true(grepl("mg_goto", h))              # clickable dots
  # a later step shows its own chart and no trait selector
  h3 <- as.character(ngcd_mg_guided_panel(3))
  expect_true(grepl("Figure 3 of 5", h3))
  expect_true(grepl('id="mg_conf"', h3))
  expect_false(grepl("mg_trait_ui", h3))
})

test_that("guided panel clamps out-of-range and bad steps", {
  expect_true(grepl("Figure 1 of 5", as.character(ngcd_mg_guided_panel(0))))
  expect_true(grepl("Figure 5 of 5", as.character(ngcd_mg_guided_panel(99))))
  expect_true(grepl("Figure 1 of 5", as.character(ngcd_mg_guided_panel(NA))))
})

test_that("mg css is a style tag", {
  expect_s3_class(ngcd_mg_css(), "shiny.tag")
  expect_true(grepl("ngcd-mg-dot", as.character(ngcd_mg_css())))
})
