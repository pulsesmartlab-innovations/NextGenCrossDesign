test_that("demo data files exist and are well-formed", {
  p <- demo_paths()
  for (f in p) expect_true(file.exists(f))
  g <- nextgenCrossWorkbench:::ngcd_read_full(p$genotype)
  expect_equal(nrow(g), 10L)
  expect_true("NAME" %in% names(g))
  expect_equal(ncol(g), 13L)  # NAME + 12 markers
  ph <- nextgenCrossWorkbench:::ngcd_read_full(p$phenotype)
  expect_true(all(c("yield", "disease") %in% names(ph)))
})

test_that("ngcd_read_full handles delimiters and a UTF-8 BOM", {
  rd <- nextgenCrossWorkbench:::ngcd_read_full
  base <- demo_paths()$genotype
  ncol_ref <- ncol(rd(base))

  # semicolon-delimited (common European/Excel export)
  semi <- tempfile(fileext = ".csv")
  writeLines(gsub(",", ";", readLines(base)), semi)
  d_semi <- rd(semi)
  expect_equal(ncol(d_semi), ncol_ref)      # not one mashed-together column
  expect_equal(names(d_semi)[1], "NAME")

  # tab-delimited
  tabf <- tempfile(fileext = ".tsv")
  writeLines(gsub(",", "\t", readLines(base)), tabf)
  expect_equal(ncol(rd(tabf)), ncol_ref)

  # UTF-8 BOM prefix must not corrupt the first column name
  bomf <- tempfile(fileext = ".csv")
  con <- file(bomf, "wb")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(charToRaw(paste0(paste(readLines(base), collapse = "\n"), "\n")), con)
  close(con)
  d_bom <- rd(bomf)
  expect_equal(names(d_bom)[1], "NAME")      # BOM stripped
  expect_equal(ncol(d_bom), ncol_ref)
})

test_that("column guessing finds conventional names", {
  gc <- nextgenCrossWorkbench:::ngcd_guess_col
  expect_equal(gc(c("NAME", "SNP_1"), c("NAME", "parent", "id")), "NAME")
  expect_equal(gc(c("parent", "x"), c("NAME", "parent")), "parent")
  expect_null(gc(c("a", "b"), c("NAME", "id")))
})

test_that("het-violator audit matches backend semantics", {
  hv <- nextgenCrossWorkbench:::ngcd_het_violators
  markers <- paste0("SNP_", 1:10)
  # fully inbred -> no violators
  inbred <- data.frame(NAME = c("A", "B"),
    matrix(rep(c(0, 2), each = 10), nrow = 2, byrow = TRUE,
           dimnames = list(NULL, markers)), check.names = FALSE)
  r0 <- hv(inbred, "NAME", markers)
  expect_equal(r0$n, 0L)
  # one parent all-heterozygous -> flagged
  het <- inbred; het[2, markers] <- 1
  r1 <- hv(het, "NAME", markers)
  expect_equal(r1$n, 1L)
  expect_equal(r1$ids, "B")
  expect_gt(r1$max_frac, 0.02)
})

test_that("error hints map known messages", {
  eh <- nextgenCrossWorkbench:::ngcd_error_hint
  expect_match(eh("residual-heterozygosity tolerance"), "inbred", ignore.case = TRUE)
  expect_match(eh("marker map is missing"), "marker", ignore.case = TRUE)
  expect_match(eh("needs lpSolve"), "lpSolve|evolution|greedy", ignore.case = TRUE)
  expect_null(eh("some totally novel message"))
})

test_that("config write drops NULL/empty and roundtrips", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  nextgenCrossWorkbench:::ngcd_write_config(
    list(a = 1, b = NULL, c = "", d = "keep", e = c(0.1, 0.2)), tmp)
  back <- jsonlite::fromJSON(tmp)
  expect_true(all(c("a", "d", "e") %in% names(back)))
  expect_false(any(c("b", "c") %in% names(back)))
  expect_equal(back$d, "keep")
  expect_equal(back$e, c(0.1, 0.2))
})

test_that("input materialization writes the four CSVs", {
  d <- demo_paths()
  data <- list(
    genotype  = nextgenCrossWorkbench:::ngcd_read_full(d$genotype),
    phenotype = nextgenCrossWorkbench:::ngcd_read_full(d$phenotype),
    map       = nextgenCrossWorkbench:::ngcd_read_full(d$map),
    direction = nextgenCrossWorkbench:::ngcd_read_full(d$direction))
  rd <- tempfile("run"); dir.create(rd)
  paths <- nextgenCrossWorkbench:::ngcd_materialize_inputs(data, rd)
  expect_true(file.exists(paths$genotype))
  expect_true(file.exists(paths$phenotype))
  g2 <- nextgenCrossWorkbench:::ngcd_read_full(paths$genotype)
  expect_equal(nrow(g2), 10L)
})
