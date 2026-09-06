# The per-file guided import: uploading a file reveals ITS preview + column
# mapping (from that file) + inline cross-file validation, in its own card.

test_that("each file's card renders its preview + column mapping + validation", {
  skip_on_cran()
  gp <- tempfile(fileext = ".csv"); pp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(NAME = c("P1", "P2", "P3"), S1 = c(0, 2, 0), S2 = c(2, 2, 0)), gp, row.names = FALSE)
  utils::write.csv(data.frame(NAME = c("P1", "P2", "P4"), yield = c(1, 2, 3), protein = c(4, 5, 6)), pp, row.names = FALSE)

  srv <- nextgenCrossWorkbench:::workbench_server(
    nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb")))
  h <- function(o) paste(as.character(o), collapse = " ")
  shiny::testServer(srv, {
    session$setInputs(workflow = "standard", data_source = "upload")
    session$setInputs(f_geno  = list(name = "g.csv", size = 1, type = "text/csv", datapath = gp))
    session$setInputs(f_pheno = list(name = "p.csv", size = 1, type = "text/csv", datapath = pp))
    session$flushReact()

    gs <- h(output$geno_step); ps <- h(output$pheno_step)
    expect_match(gs, "genotype_id_col")          # its own parent-ID picker
    expect_match(gs, "<table")                    # preview from THIS file
    expect_match(ps, "phenotype_id_col")
    expect_match(ps, "yield, protein")            # trait columns come from the phenotype file
    expect_match(ps, "2 of 3")                    # inline cross-file ID-match validation
    expect_match(h(output$import_strip), "Genotype")   # status strip
  })
})

test_that("strip chips link to their card, and re-uploading one file keeps the others (revisitable)", {
  skip_on_cran()
  gp  <- tempfile(fileext = ".csv"); pp <- tempfile(fileext = ".csv"); pp2 <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(NAME = c("P1", "P2"), S1 = c(0, 2)), gp, row.names = FALSE)
  utils::write.csv(data.frame(NAME = c("P1", "P2"), yield = c(1, 2)), pp, row.names = FALSE)
  utils::write.csv(data.frame(NAME = c("P1", "P2"), newtrait = c(9, 8)), pp2, row.names = FALSE)

  srv <- nextgenCrossWorkbench:::workbench_server(
    nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb")))
  shiny::testServer(srv, {
    session$setInputs(workflow = "standard", data_source = "upload")
    session$setInputs(f_geno  = list(name = "g.csv",  size = 1, type = "text/csv", datapath = gp))
    session$setInputs(f_pheno = list(name = "p.csv",  size = 1, type = "text/csv", datapath = pp))
    session$flushReact()

    strip <- paste(as.character(output$import_strip), collapse = " ")
    expect_match(strip, 'href="#imp-geno"', fixed = TRUE)     # chip jumps to the genotype card
    expect_match(strip, 'href="#imp-pheno"', fixed = TRUE)
    geno_rows <- nrow(rv$data$genotype)

    # re-upload a DIFFERENT phenotype: genotype must be untouched
    session$setInputs(f_pheno = list(name = "p2.csv", size = 1, type = "text/csv", datapath = pp2))
    session$flushReact()
    expect_equal(nrow(rv$data$genotype), geno_rows)           # other file preserved
    expect_true("newtrait" %in% names(rv$data$phenotype))     # this file updated
  })
})

test_that("import cards adapt to workflow (poly = genotype + phenotype only)", {
  skip_on_cran()
  srv <- nextgenCrossWorkbench:::workbench_server(
    nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb")))
  shiny::testServer(srv, {
    # the map/direction cards are wrapped in workflow conditionalPanels in the UI;
    # the strip drops them for polyploid (single-trait, dosage-only).
    session$setInputs(workflow = "polyploid", data_source = "upload")
    session$flushReact()
    strip <- paste(as.character(output$import_strip), collapse = " ")
    expect_match(strip, "Genotype"); expect_match(strip, "Phenotype")
    expect_false(grepl("Direction", strip, fixed = TRUE))   # no trait-direction step for poly
    expect_false(grepl("3 Map", strip, fixed = TRUE))       # no marker-map step for poly
  })
})

# ---- check-line import (own card, separate from the parent pool) ----------

test_that("the check file is a distinct optional input, separate from the parents", {
  cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
  html <- suppressWarnings(as.character(nextgenCrossWorkbench:::workbench_ui(cfg, dev = FALSE)))
  expect_true(grepl("f_check", html, fixed = TRUE))
  expect_true(grepl("f_check_pheno", html, fixed = TRUE))
  expect_true(grepl("imp-check", html, fixed = TRUE))
  # the card must say plainly that checks are never crossed
  expect_true(grepl("never crossed", html, fixed = TRUE))
})

test_that("uploading a check genotype file populates its own store, preview, and ID picker - and never touches the parent pool", {
  skip_on_cran()
  # NOTE: this test deliberately does NOT upload a parent phenotype file. Doing
  # so currently triggers a pre-existing, unrelated bug at the trait_checks
  # call site in build_params() (R/app.R ~1258): it still passes a `bases =`
  # argument to ngcd_build_trait_checks(), which Task 1 (commit 10ce71d)
  # already dropped from that function's signature. That call site is owned by
  # Task 4, not this task, so it is left untouched here (see task-2-report.md).
  gp <- tempfile(fileext = ".csv"); cg <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(NAME = c("P1", "P2", "P3"), S1 = c(0, 2, 0), S2 = c(2, 2, 0)), gp, row.names = FALSE)
  utils::write.csv(data.frame(NAME = c("Check1", "Check2"), S1 = c(2, 0), S2 = c(0, 2)), cg, row.names = FALSE)

  srv <- nextgenCrossWorkbench:::workbench_server(
    nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb")))
  shiny::testServer(srv, {
    session$setInputs(workflow = "standard", data_source = "upload")
    session$setInputs(f_geno  = list(name = "g.csv", size = 1, type = "text/csv", datapath = gp))
    session$flushReact()
    parent_rows_before <- nrow(rv$data$genotype)
    expect_null(rv$data$check_geno)   # no check file yet

    session$setInputs(f_check = list(name = "c.csv", size = 1, type = "text/csv", datapath = cg))
    session$flushReact()

    expect_equal(nrow(rv$data$check_geno), 2L)
    expect_true(all(c("Check1", "Check2") %in% rv$data$check_geno$NAME))
    # the candidate-parent pool is untouched by the check upload
    expect_equal(nrow(rv$data$genotype), parent_rows_before)
    expect_false(any(rv$data$genotype$NAME %in% c("Check1", "Check2")))

    cs <- paste(as.character(output$check_step), collapse = " ")
    expect_match(cs, "check_id_col")
    expect_match(cs, "<table")
  })
})

test_that("a check phenotype file loads into its own store without a parent phenotype file present", {
  skip_on_cran()
  cg <- tempfile(fileext = ".csv"); cp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(NAME = c("Check1", "Check2"), S1 = c(2, 0), S2 = c(0, 2)), cg, row.names = FALSE)
  utils::write.csv(data.frame(NAME = c("Check1", "Check2"), yield = c(9, 8)), cp, row.names = FALSE)

  srv <- nextgenCrossWorkbench:::workbench_server(
    nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb")))
  shiny::testServer(srv, {
    session$setInputs(workflow = "standard", data_source = "upload")
    session$setInputs(f_check       = list(name = "c.csv",  size = 1, type = "text/csv", datapath = cg))
    session$setInputs(f_check_pheno = list(name = "cp.csv", size = 1, type = "text/csv", datapath = cp))
    session$flushReact()

    expect_equal(nrow(rv$data$check_pheno), 2L)
    expect_true("yield" %in% names(rv$data$check_pheno))
  })
})

test_that("check phenotype gets its own preview even when no check genotype is uploaded", {
  skip_on_cran()
  cp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(NAME = c("Check1", "Check2"), yield = c(9, 8)), cp, row.names = FALSE)

  srv <- nextgenCrossWorkbench:::workbench_server(
    nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb")))
  shiny::testServer(srv, {
    session$setInputs(workflow = "standard", data_source = "upload")
    session$setInputs(f_check_pheno = list(name = "cp.csv", size = 1, type = "text/csv", datapath = cp))
    session$flushReact()

    expect_null(rv$data$check_geno)
    expect_equal(nrow(rv$data$check_pheno), 2L)
    cs <- paste(as.character(output$check_step), collapse = " ")
    expect_match(cs, "Optional")     # still prompts for the (missing) check genotype
    expect_match(cs, "<table")       # AND shows the phenotype preview it does have
    expect_false(grepl("not evaluable", cs, fixed = TRUE))
  })
})

test_that("no check file loaded is a quiet optional state, not an error", {
  skip_on_cran()
  srv <- nextgenCrossWorkbench:::workbench_server(
    nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb")))
  shiny::testServer(srv, {
    session$setInputs(workflow = "standard", data_source = "upload")
    session$flushReact()
    expect_null(rv$data$check_geno)
    expect_null(rv$data$check_pheno)
    cs <- paste(as.character(output$check_step), collapse = " ")
    expect_match(cs, "Optional")
  })
})
