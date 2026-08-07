# The residual-heterozygosity block (DH/RIL models require inbred parents) must
# be (a) detected up front with the same rule as the backend, and (b) fixable in
# one click from the error panel. Backend-gated (runs the real prediction).
library(shiny)

test_that("het parents are flagged and the one-click exclusion recovers the run", {
  skip_on_cran()
  skip_if(Sys.getenv("NGCD_RUN_COMBINATIONS") != "1",
          "Set NGCD_RUN_COMBINATIONS=1 to run the backend het-fix check.")
  skip_if(!backend_available(), "Backend not available.")

  set.seed(7); np <- 20L; nm <- 200L
  g <- matrix(sample(c(0L, 2L), np * nm, replace = TRUE), np, nm)
  for (r in c(5L, 11L, 17L)) g[r, sample(nm, 6L)] <- 1L   # ~3% het -> non-inbred
  gd <- data.frame(NAME = sprintf("P%02d", seq_len(np)))
  for (i in seq_len(nm)) gd[[sprintf("M%03d", i)]] <- g[, i]
  wr <- function(df) { p <- tempfile(fileext = ".csv"); utils::write.csv(df, p, row.names = FALSE); p }
  gf <- wr(gd)
  pf <- wr(data.frame(NAME = gd$NAME, yield = rnorm(np, 55, 5), disease = rnorm(np, 3, 1)))
  mf <- wr(data.frame(SNP_code = sprintf("M%03d", seq_len(nm)),
                      Chromosome = rep(1:5, length.out = nm), Position_BP = sample(1e7, nm)))
  df_ <- tempfile(fileext = ".csv")
  writeLines(c("Trait,Selection_direction", "yield,increase", "disease,decrease"), df_)

  cfg <- nextgenCrossWorkbench:::ngcd_load_config(tempfile("wbhet"))
  testServer(nextgenCrossWorkbench:::workbench_server(cfg), {
    session$setInputs(data_source = "upload",
      f_geno = list(datapath = gf, name = "g.csv"),
      f_pheno = list(datapath = pf, name = "p.csv"),
      f_map = list(datapath = mf, name = "m.csv"),
      f_dir = list(datapath = df_, name = "d.csv"),
      genotype_id_col = "NAME", phenotype_id_col = "NAME",
      map_marker_col = "SNP_code", map_chr_col = "Chromosome",
      pos_unit = "bp", map_pos_bp_col = "Position_BP",
      direction_trait_col = "Trait", direction_column_col = "Trait",
      direction_direction_col = "Selection_direction",
      objective_mode = "multi", trait_value_metric = "mean",
      progeny = "DH", parent_type = "inbred", n_crosses = 7, max_crosses_per_parent = 4,
      allocation_method = "ocs", optimizer = "auto",
      drop_noninbred_parents = FALSE, seed = 1)
    session$flushReact()

    expect_equal(align_diag()$het_n, 3L)          # same 3 parents the backend rejects

    session$setInputs(run = 1); session$flushReact()
    expect_false(is.null(rv$error))               # blocked
    expect_true(isTRUE(rv$error$het_fix))         # one-click offered
    expect_length(rv$error$het_ids, 3L)

    session$setInputs(fix_exclude_het = 1); session$flushReact()
    expect_false(is.null(rv$result))              # recovered
    expect_true(is.null(rv$error))
    expect_gt(nrow(rv$result$selected_crosses), 0L)
  })
})
