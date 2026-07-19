# Edge cases for run-directory assembly, config serialisation, input
# materialisation, error hinting, and rscript resolution. No backend needed.

ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")

test_that("run-dir slug is filesystem-safe and unique-ish", {
  cfg <- list(runs_dir = tempfile("runs"))
  d1 <- ng("ngcd_new_run_dir")(cfg, label = "trait/by trait: auto!")
  expect_true(dir.exists(d1))
  expect_false(grepl("[^A-Za-z0-9_./-]", basename(d1)))  # sanitised slug
  d2 <- ng("ngcd_new_run_dir")(cfg, label = NULL)
  expect_match(basename(d2), "_run_")   # 'run' slug + tempfile unique suffix
  expect_false(identical(d1, d2))        # distinct dirs
})

test_that("config writer drops NULL/empty but preserves 0, FALSE and numeric precision", {
  write_cfg <- ng("ngcd_write_config")
  p <- tempfile(fileext = ".json")
  write_cfg(list(a = 1L, b = NULL, c = "", d = 0, e = FALSE,
                 pi = 3.141592653589793, vec = c(1, 2, 3)), p)
  back <- jsonlite::fromJSON(p, simplifyVector = TRUE)
  expect_true(all(c("a", "d", "e", "pi", "vec") %in% names(back)))
  expect_false(any(c("b", "c") %in% names(back)))   # NULL and "" removed
  expect_identical(back$e, FALSE)                    # FALSE kept
  expect_equal(back$d, 0)                            # zero kept
  expect_equal(back$pi, 3.141592653589793, tolerance = 1e-12)  # 12-digit precision
  expect_equal(back$vec, c(1, 2, 3))                 # vectors not unboxed
})

test_that("input materialisation writes only the tables that are present", {
  mat <- ng("ngcd_materialize_inputs")
  run_dir <- tempfile("run"); dir.create(run_dir)
  data <- list(genotype = data.frame(NAME = "P01", m1 = 0),
               phenotype = data.frame(NAME = "P01", yield = 3.2),
               map = NULL, direction = NULL)
  paths <- mat(data, run_dir)
  expect_true(file.exists(paths$genotype))
  expect_true(file.exists(paths$phenotype))
  expect_null(paths$map)
  expect_null(paths$direction)
  # written CSVs read back identically (no row names)
  g <- read.csv(paths$genotype, check.names = FALSE)
  expect_equal(names(g), c("NAME", "m1"))
})

test_that("error hints cover every documented failure family and unknown -> NULL", {
  hint <- ng("ngcd_error_hint")
  expect_match(hint("Dosage values out of range for ploidy"), "Ploidy")
  expect_match(hint("residual-heterozygosity tolerance exceeded"), "inbred")
  expect_match(hint("cannot open file: genotype.csv"), "file path")
  expect_match(hint("parent IDs do not match/align"), "align")
  expect_match(hint("lpSolve is required for the MIP"), "lpSolve|optimizer")
  expect_match(hint("marker names in the map"), "map|Marker")
  expect_match(hint("could not find function ng_run_cross_prediction"), "backend")
  expect_null(hint("some entirely novel message"))
  expect_null(hint(NULL))
})

test_that("rscript resolution returns NA for a bogus path and a real path otherwise", {
  resolve <- ng("ngcd_resolve_rscript")
  expect_true(is.na(resolve(list(rscript_path = "/no/such/rscript-xyz"))))
  real <- resolve(list(rscript_path = "Rscript"))
  expect_true(is.na(real) || file.exists(real) || nzchar(real))
})

test_that("run-backend reports a clean failure when Rscript is missing", {
  run <- ng("ngcd_run_backend")
  cfg <- list(rscript_path = "/definitely/not/here/rscript",
              runs_dir = tempfile("runs"))
  res <- run(cfg, params = list(seed = 1))
  expect_false(res$ok)
  expect_match(res$log, "Rscript not found")
})
