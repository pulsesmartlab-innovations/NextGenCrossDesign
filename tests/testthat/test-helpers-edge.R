# Edge-case coverage for the formatting / IO / audit helpers. These run without
# the backend and target the messy-real-world inputs the workbench must survive.

ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")

# ---- ngcd_read_full: delimiters, quoting, encodings, degenerate files --------

test_that("reader auto-detects tab and pipe delimiters", {
  read_full <- ng("ngcd_read_full")

  tabf <- tempfile(fileext = ".tsv")
  writeLines(c("NAME\tyield\tdisease", "P01\t3.2\t1.1", "P02\t4.0\t0.9"), tabf)
  d1 <- read_full(tabf)
  expect_equal(names(d1), c("NAME", "yield", "disease"))
  expect_equal(nrow(d1), 2L)

  pipef <- tempfile(fileext = ".csv")
  writeLines(c("NAME|yield|disease", "P01|3.2|1.1", "P02|4.0|0.9"), pipef)
  d2 <- read_full(pipef)
  expect_equal(names(d2), c("NAME", "yield", "disease"))
  expect_equal(d2$yield, c(3.2, 4.0))
})

test_that("reader honours quoted fields containing the delimiter", {
  read_full <- ng("ngcd_read_full")
  f <- tempfile(fileext = ".csv")
  writeLines(c('NAME,label', 'P01,"Fargo, ND"', 'P02,"Casselton, ND"'), f)
  d <- read_full(f)
  expect_equal(d$label, c("Fargo, ND", "Casselton, ND"))
  expect_equal(ncol(d), 2L)
})

test_that("reader trims header whitespace and strips a BOM on the first name", {
  read_full <- ng("ngcd_read_full")
  f <- tempfile(fileext = ".csv")
  con <- file(f, "wb")
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)),
             charToRaw("NAME , yield\nP01,3.2\n")), con)
  close(con)
  d <- read_full(f)
  expect_equal(names(d)[1], "NAME")          # BOM stripped
  expect_true("yield" %in% names(d))          # header whitespace trimmed
})

test_that("reader returns NULL for a missing path and a 1-column frame otherwise", {
  read_full <- ng("ngcd_read_full")
  expect_null(read_full(NULL))
  expect_null(read_full(file.path(tempdir(), "does-not-exist-xyz.csv")))

  onecol <- tempfile(fileext = ".csv")
  writeLines(c("NAME", "P01", "P02", "P03"), onecol)
  d <- read_full(onecol)
  expect_equal(ncol(d), 1L)
  expect_equal(nrow(d), 3L)
})

# ---- ngcd_num: NA / empty / vector / rounding -------------------------------

test_that("ngcd_num formats, rounds, and guards NA/empty", {
  num <- ng("ngcd_num")
  expect_equal(num(NA), "--")
  expect_equal(num(NULL), "--")
  expect_equal(num(numeric(0)), "--")
  expect_equal(num(3.14159, 2), "3.14")
  expect_equal(num(c(1.5, 9.9)), "1.500")   # uses first element only
  expect_equal(num(1234.5, 1), "1,234.5")   # thousands separator
})

# ---- ngcd_guess_col: case-insensitive, first hit, no match -------------------

test_that("column guessing is case-insensitive, prefers first hit, else NULL", {
  guess <- ng("ngcd_guess_col")
  cols <- c("Sample", "YIELD", "Disease_Score")
  expect_equal(guess(cols, c("yield")), "YIELD")
  expect_equal(guess(cols, c("nope", "sample")), "Sample")
  expect_null(guess(cols, c("chromosome", "position")))
})

# ---- ngcd_het_violators: none / all / NA / tetraploid ------------------------

test_that("het audit flags nobody when parents are fully homozygous", {
  het <- ng("ngcd_het_violators")
  g <- data.frame(NAME = c("A", "B", "C"),
                  m1 = c(0, 2, 0), m2 = c(2, 0, 2), m3 = c(0, 0, 2))
  r <- het(g, "NAME", c("m1", "m2", "m3"), ploidy = 2)
  expect_equal(r$n, 0L)
  expect_length(r$ids, 0L)
  expect_lte(r$max_frac, r$frac_tol)
})

test_that("het audit flags a fully heterozygous parent", {
  het <- ng("ngcd_het_violators")
  g <- data.frame(NAME = c("Aa", "BB"),
                  m1 = c(1, 0), m2 = c(1, 2), m3 = c(1, 0), m4 = c(1, 2))
  r <- het(g, "NAME", c("m1", "m2", "m3", "m4"), ploidy = 2)
  expect_true("Aa" %in% r$ids)
  expect_false("BB" %in% r$ids)
  expect_equal(r$n, 1L)
})

test_that("het audit tolerates NA dosages and empty marker sets", {
  het <- ng("ngcd_het_violators")
  g <- data.frame(NAME = c("A", "B"), m1 = c(NA, 2), m2 = c(0, NA))
  r <- het(g, "NAME", c("m1", "m2"), ploidy = 2)
  expect_true(is.finite(r$max_frac))
  # no marker columns -> a well-formed empty result, never an error
  r0 <- het(g, "NAME", character(0))
  expect_equal(r0$n, 0L)
})

test_that("het audit respects ploidy for tetraploid coding (2 = het midpoint)", {
  het <- ng("ngcd_het_violators")
  g <- data.frame(NAME = c("mid", "hom"),
                  m1 = c(2, 0), m2 = c(2, 4), m3 = c(2, 0), m4 = c(2, 4))
  r <- het(g, "NAME", c("m1", "m2", "m3", "m4"), ploidy = 4)
  expect_true("mid" %in% r$ids)   # dosage 2 is maximally heterozygous at ploidy 4
  expect_false("hom" %in% r$ids)
})

# ---- UI helpers return valid Shiny tags -------------------------------------

test_that("badge/callout/kpi/section/guide produce renderable Shiny tags", {
  render <- function(x) as.character(x)
  expect_match(render(ng("ngcd_badge")("Live", "ok")), "ndsu-badge")
  expect_match(render(ng("ngcd_callout")("hi", kind = "warn")), "ndsu-callout")
  expect_match(render(ng("ngcd_kpi")("42", "crosses")), "ndsu-kpi")
  expect_match(render(ng("ngcd_section")("Title", "Sub")), "ndsu-section-title")
  g <- ng("ngcd_guide")("Configure", "Scoring", shiny::div("body"), next_hint = "go on")
  expect_match(render(g), "Configure")
  expect_match(render(g), "Next")
})

test_that("badge/callout reject unknown kinds via match.arg", {
  expect_error(ng("ngcd_badge")("x", "purple"))
  expect_error(ng("ngcd_callout")("x", kind = "purple"))
})

# ---- DT helpers degrade gracefully on empty input ---------------------------

test_that("DT helpers show a friendly placeholder for empty/NULL data", {
  dt  <- ng("ngcd_dt")(NULL)
  dte <- ng("ngcd_dt_editable")(data.frame())
  expect_s3_class(dt, "datatables")
  expect_s3_class(dte, "datatables")
  # a populated frame round-trips into a datatable
  d <- ng("ngcd_dt")(data.frame(a = 1:3, b = rnorm(3)))
  expect_s3_class(d, "datatables")
})
