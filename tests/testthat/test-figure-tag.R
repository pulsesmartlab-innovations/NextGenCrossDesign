# Phase 3, Task 1: ngcd_figure_tag() disclosure helper. No shiny session, no
# I/O - purely the returned shiny.tag markup. See R/helpers.R (sibling of
# ngcd_guide()).

ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")

test_that("ngcd_figure_tag() returns a collapsed ndsu-figtag details tag", {
  tag <- ng("ngcd_figure_tag")("x_fig", "Trait distribution")

  expect_s3_class(tag, "shiny.tag")
  expect_identical(tag$attribs$class, "ndsu-figtag")
  expect_null(tag$attribs$open)

  html <- as.character(tag)
  expect_true(grepl("Figure", html, fixed = TRUE))
  expect_true(grepl("Trait distribution", html, fixed = TRUE))
  expect_true(grepl('id="x_fig"', html, fixed = TRUE))
})

test_that("open = TRUE adds the open attribute; open = FALSE (default) does not", {
  tag_open <- ng("ngcd_figure_tag")("x_fig", "Trait distribution", open = TRUE)
  expect_identical(tag_open$attribs$open, "open")
  expect_true(grepl("open=\"open\"", as.character(tag_open), fixed = TRUE))

  tag_closed <- ng("ngcd_figure_tag")("x_fig", "Trait distribution", open = FALSE)
  expect_null(tag_closed$attribs$open)
})
