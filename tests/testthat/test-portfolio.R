# Portfolio & risk scatter helper: builds a plotly widget when
# cross_level / cross_upside / risk_bin are present, else returns NULL
# (multi-trait runs / older results without portfolio columns).

test_that("ngcd_portfolio_plotly builds a widget when columns present, else NULL", {
  res <- list(settings = list(selection_prop = 0.1),
    selected_crosses = data.frame(
      cross = c("A/B","C/D","E/F"), cross_level = c(3,2,1), cross_upside = c(2,1,2),
      cross_confidence = c(.9,.5,.7),
      risk_bin = factor(c("low","high","med"), levels = c("low","med","high"), ordered = TRUE),
      portfolio_profile = factor(c("breakthrough","long_shot","workhorse")),
      priority_tier = factor(c("highly_priority","priority","medium_priority")),
      stringsAsFactors = FALSE))
  p <- nextgenCrossWorkbench:::ngcd_portfolio_plotly(res)
  expect_true(inherits(p, "plotly") || inherits(p, "htmlwidget"))
  expect_null(nextgenCrossWorkbench:::ngcd_portfolio_plotly(list(selected_crosses = data.frame(a = 1))))
})
