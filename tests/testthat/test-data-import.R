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
