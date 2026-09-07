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

test_that("check args are forwarded and the removed parameters never are", {
  ngcd_coerce_backend_args <- ngcd_runner_env()$ngcd_coerce_backend_args
  args <- list(trait_checks = list(list(trait = "yield", check = "CHK_A")),
               check_geno = list(list(id = "CHK_A", m1 = 0, m2 = 2)))
  out <- ngcd_coerce_backend_args(args)
  expect_s3_class(out$trait_checks, "data.frame")
  expect_true(is.matrix(out$check_geno))
  expect_equal(rownames(out$check_geno), "CHK_A")
  expect_null(out$check_basis)
  expect_null(out$exclude_threshold_violators)
  # progeny size is passed straight through, never defaulted on the way
  expect_equal(ngcd_coerce_backend_args(c(args, list(check_progeny_size = 250)))$check_progeny_size, 250)
})

test_that("check_pheno JSON rows reshape into a proper data.frame, not a mangled 1-row table", {
  # Regression: as.data.frame() on a raw list of JSON row-lists silently produces
  # a garbled 1-row data.frame with duplicated column names (one group per input
  # row) rather than erroring - so this needs the same as_rows_df() reshaping
  # check_geno gets, or the backend's ng_check_records_from_pheno() (which calls
  # as.data.frame() on whatever check_pheno arrives as) reads nonsense silently.
  coerce <- ngcd_runner_env()$ngcd_coerce_backend_args
  args <- list(check_pheno = list(list(NAME = "CHK_A", yield = 5.2),
                                  list(NAME = "CHK_B", yield = 6.1)))
  out <- coerce(args)
  expect_s3_class(out$check_pheno, "data.frame")
  expect_equal(nrow(out$check_pheno), 2L)
  expect_setequal(names(out$check_pheno), c("NAME", "yield"))
  expect_equal(out$check_pheno$NAME, c("CHK_A", "CHK_B"))
  expect_equal(out$check_pheno$yield, c(5.2, 6.1))
})

test_that("check_geno is keyed by the breeder's chosen check_id_col, not blindly by column 1", {
  # Regression: the runner hardcoded id_col <- names(cg)[[1L]] while the UI, the
  # per-trait check picker and the check/parent clash guard all honoured
  # input$check_id_col. A breeder picking any column but the first got a matrix keyed
  # by the wrong column, and the backend then failed with "trait_checks names check
  # line(s) absent from check_geno" -- an error pointing nowhere near the cause.
  coerce <- ngcd_runner_env()$ngcd_coerce_backend_args

  # ID column SECOND (a marker column leads the file): the user's pick must win.
  rows <- list(list(SNP_000 = 1, NAME = "CHK_A", SNP_001 = 0, SNP_002 = 2),
               list(SNP_000 = 0, NAME = "CHK_B", SNP_001 = 2, SNP_002 = 1))
  out <- coerce(list(check_geno = rows, check_id_col = "NAME"))
  expect_true(is.matrix(out$check_geno))
  expect_equal(rownames(out$check_geno), c("CHK_A", "CHK_B"))
  expect_setequal(colnames(out$check_geno), c("SNP_000", "SNP_001", "SNP_002"))
  expect_equal(unname(out$check_geno["CHK_B", "SNP_001"]), 2)
  expect_null(out$check_id_col)   # meta key: consumed here, never sent to the backend
  # ...and it is a recognised meta key, so it must not trip the "unrecognised
  # config keys" warning either.
  expect_silent(coerce(list(check_geno = rows, check_id_col = "NAME")))

  # ID column FIRST: unchanged behaviour with no check_id_col (older config /
  # in-process caller), and a check_id_col naming a column the table does not have
  # falls back to column 1 rather than erroring.
  rows1 <- list(list(NAME = "CHK_A", SNP_001 = 0, SNP_002 = 2),
                list(NAME = "CHK_B", SNP_001 = 2, SNP_002 = 1))
  expect_equal(rownames(coerce(list(check_geno = rows1))$check_geno), c("CHK_A", "CHK_B"))
  expect_equal(rownames(coerce(list(check_geno = rows1,
                                    check_id_col = "not_a_column"))$check_geno),
               c("CHK_A", "CHK_B"))
})

# ---------------------------------------------------------------------------
# Robustness-quantile routing (workbench 0.28.0). robustness_quantile is a
# robust-allocation setting AND, from backend 0.25.0, a real
# ng_run_cross_prediction() formal: the run has to cache the exact tail the
# post-run allocator will ask for. These are the in-process unit tests of that
# routing; the end-to-end proof that a plan comes out is in test-robust-plan.R.
# ---------------------------------------------------------------------------

test_that("ngcd_robust_quantile: NULL unless robust allocation is on, then the slider value (0.25 default)", {
  e <- ngcd_runner_env()
  f <- e$ngcd_robust_quantile
  expect_null(f(list()))                                       # nothing requested
  expect_null(f(list(robustness_quantile = 0.1)))              # quantile without the toggle
  expect_null(f(list(robust_allocation = FALSE, robustness_quantile = 0.1)))
  expect_equal(f(list(robust_allocation = TRUE)), 0.25)        # app default
  expect_equal(f(list(robust_allocation = TRUE, robustness_quantile = 0.05)), 0.05)
  expect_equal(f(list(robust_allocation = TRUE, robustness_quantile = 0.5)), 0.5)
})

test_that("robustness_quantile reaches the backend run only when robust allocation is on, and never as an unknown key", {
  skip_if_not(requireNamespace("nextgenCrossDesign", quietly = TRUE), "Backend not installed.")
  skip_if_not("robustness_quantile" %in%
                names(formals(nextgenCrossDesign::ng_run_cross_prediction)),
              "Backend predates robustness_quantile as a formal.")
  e <- ngcd_runner_env()
  coerce <- e$ngcd_coerce_backend_args
  base <- list(schema = "ng_run_config.v1", genotype_file = "g.csv",
               phenotype_file = "p.csv", n_crosses = 7)

  # Off: nothing extra is sent, so a plain run caches no extra tail.
  expect_false("robustness_quantile" %in% names(coerce(base)))
  # A stray quantile with the toggle off is still not forwarded.
  expect_false("robustness_quantile" %in%
                 names(coerce(c(base, list(robustness_quantile = 0.1)))))
  # On: forwarded, and with EXACTLY the value the post-run allocator will request.
  on <- coerce(c(base, list(robust_allocation = TRUE, robustness_quantile = 0.1)))
  expect_equal(on$robustness_quantile, 0.1)
  expect_equal(on$robustness_quantile, e$ngcd_robust_quantile(
    c(base, list(robust_allocation = TRUE, robustness_quantile = 0.1))))
  # On with no explicit value: the app default is cached, not nothing.
  expect_equal(coerce(c(base, list(robust_allocation = TRUE)))$robustness_quantile, 0.25)
  # And it is never reported as an unrecognised config key (it stays a meta key
  # for the generic path; the injection above is what forwards it).
  expect_silent(coerce(c(base, list(robust_allocation = TRUE, robustness_quantile = 0.1,
                                    robust_objective = "posterior_quantile",
                                    robust_top_n_target = 10L))))
})
