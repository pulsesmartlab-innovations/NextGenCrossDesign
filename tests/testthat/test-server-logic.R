# Server reactive logic (no backend needed) via shiny::testServer.
library(shiny)

srv_cfg <- function() nextgenCrossWorkbench:::ngcd_load_config(tempfile("wb"))
srv <- function() nextgenCrossWorkbench:::workbench_server(srv_cfg())

test_that("ID-column selector stays small for a wide genotype (no selectize warning)", {
  # A genotype with thousands of marker columns must not fill the Genotype-ID
  # <select> with every marker (which triggers Shiny's "large number of options"
  # performance warning). The ID list should collapse to label columns.
  np <- 6; nm <- 2000
  g <- data.frame(NAME = sprintf("P%02d", seq_len(np)))
  for (i in seq_len(nm)) g[[sprintf("SNP_%04d", i)]] <- rep(c(0L, 2L), length.out = np)
  gf <- tempfile(fileext = ".csv"); utils::write.csv(g, gf, row.names = FALSE)
  ph <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(NAME = g$NAME, yield = seq_len(np)), ph, row.names = FALSE)
  mp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(SNP_code = names(g)[-1], Chromosome = 1,
                              Position_BP = seq_len(nm)), mp, row.names = FALSE)
  dr <- tempfile(fileext = ".csv")
  writeLines(c("Trait,Selection_direction", "yield,increase"), dr)

  testServer(srv(), {
    session$setInputs(data_source = "upload",
      f_geno = list(datapath = gf, name = "g.csv"),
      f_pheno = list(datapath = ph, name = "p.csv"),
      f_map = list(datapath = mp, name = "m.csv"),
      f_dir = list(datapath = dr, name = "d.csv"))
    session$flushReact()
    expect_equal(ncol(rv$data$genotype), nm + 1L)     # wide table loaded
    html <- output$colmap_ui
    block <- regmatches(html, regexpr("id=\"genotype_id_col\".*?</select>", html))
    nopt <- length(gregexpr("<option", block)[[1]])
    expect_lt(nopt, 40)                                # not ~2000 options
    expect_match(block, "NAME")                        # the real ID is offered
  })
})

test_that("demo data loads and reports ready + aligned", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs())
    expect_true(data_ready())
    d <- align_diag()
    expect_length(d$miss_map, 0)
    expect_length(d$extra_map, 0)
    expect_equal(d$het_n, 0L)          # demo parents are fully inbred
    expect_equal(d$n_shared_markers, 12L)
    expect_equal(d$n_shared_ids, 10L)
  })
})

test_that("build_params() does not crash when map column selectors are unset", {
  # Regression: resolve_map() indexed rv$data$map[[input$map_pos_bp_col]] with a
  # NULL/empty column id before the dynamic column selectors had populated, which
  # threw `.subset2: attempt to select less than one element` and aborted the run
  # (so no run and no report were ever produced).
  testServer(srv(), {
    session$setInputs(data_source = "demo")
    session$flushReact()
    expect_null(isolate(input$map_pos_bp_col))    # NULL, as at first paint
    p <- NULL
    expect_no_error(p <- build_params())          # must not throw
    expect_true(is.list(p))
    expect_true(!is.null(p$map_position_unit))    # resolved a unit anyway
  })
})

test_that("traits_to_use is omitted when all traits selected, sent when subset", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs(traits_to_use = c("yield", "disease")))
    expect_null(build_params()$traits_to_use)          # all -> omitted
    session$setInputs(traits_to_use = "yield")
    expect_equal(build_params()$traits_to_use, "yield") # subset -> sent
  })
})

test_that("diversity dial maps to exactly one backend key", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs(diversity_mode = "strategy", strategy = "diversity"))
    p <- build_params()
    expect_equal(p$strategy, "diversity")
    expect_null(p$diversity_emphasis); expect_null(p$target_coancestry)

    session$setInputs(diversity_mode = "emphasis", diversity_emphasis = 70)
    p <- build_params()
    expect_equal(p$diversity_emphasis, 70)
    expect_null(p$strategy); expect_null(p$target_coancestry)

    session$setInputs(diversity_mode = "target", target_coancestry = 0.02)
    p <- build_params()
    expect_equal(p$target_coancestry, 0.02)
    expect_null(p$strategy); expect_null(p$diversity_emphasis)
  })
})

test_that("evolution tuning only added for evolution optimizer", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs(optimizer = "greedy_local"))
    expect_null(build_params()$evol_solutions)
    session$setInputs(optimizer = "evolution")
    expect_equal(build_params()$evol_solutions, 100)
  })
})

test_that("weighted trait weights parse into a named list", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs(multi_trait_method = "weighted",
                                            trait_weights = "yield: 0.7\ndisease: 0.3"))
    p <- build_params()
    expect_equal(p$trait_weights$yield, 0.7)
    expect_equal(p$trait_weights$disease, 0.3)
  })
})

test_that("marker mismatch (both directions) is detected", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs())
    # inject: rename a genotype marker column (genotype-only) and a map row (map-only)
    g <- rv$data$genotype; names(g)[names(g) == "SNP_012"] <- "GONLY"; rv$data$genotype <- g
    m <- rv$data$map; m$SNP_code[m$SNP_code == "SNP_011"] <- "MONLY"; rv$data$map <- m
    d <- align_diag()
    expect_true("GONLY" %in% d$miss_map)     # in genotype, not map
    expect_true("MONLY" %in% d$extra_map)    # in map, not genotype
  })
})

test_that("non-inbred parents are detected", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs())
    g <- rv$data$genotype
    g[g$NAME == "P05", paste0("SNP_", sprintf("%03d", 1:12))] <- 1  # all het
    rv$data$genotype <- g
    d <- align_diag()
    expect_true("P05" %in% d$het_ids)
    expect_gte(d$het_n, 1L)
  })
})

test_that("advanced mate-selection controls parse into backend shapes", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs(
      lambda_progeny_inbreeding = 0.05,
      min_crosses_per_parent = 2,
      committed_crosses = "P09,P10\nP01,P02",
      parent_group = "P01,A\nP06,B",
      group_quota = "A||B: 4",
      marker_target_spec = "SNP_001,increase,0.7,1",
      lambda_marker = 1,
      lethal_spec = "SNP_002,alt",
      drop_lethal_carrier_crosses = TRUE,
      budget = 5000, lambda_cost = 0.1, lambda_logistic = 0.2))
    p <- build_params()
    expect_equal(p$lambda_progeny_inbreeding, 0.05)
    expect_equal(p$min_crosses_per_parent, 2)
    # committed matings -> parent1/parent2 vectors
    expect_equal(p$committed_crosses$parent1, c("P09", "P01"))
    expect_equal(p$committed_crosses$parent2, c("P10", "P02"))
    # parent groups -> named
    expect_equal(p$parent_group[["P01"]], "A")
    expect_equal(p$parent_group[["P06"]], "B")
    # group quota
    expect_equal(p$group_quota[["A||B"]], 4)
    # marker target spec -> list with fields
    expect_equal(p$marker_target_spec[[1]]$marker, "SNP_001")
    expect_equal(p$marker_target_spec[[1]]$direction, "increase")
    expect_equal(p$marker_target_spec[[1]]$target_freq, 0.7)
    expect_equal(p$lambda_marker, 1)
    # lethal spec
    expect_equal(p$lethal_spec[[1]]$marker, "SNP_002")
    expect_equal(p$lethal_spec[[1]]$risk_allele, "alt")
    expect_true(p$drop_lethal_carrier_crosses)
    # cost/logistics
    expect_equal(p$budget, 5000)
    expect_equal(p$lambda_cost, 0.1)
    expect_equal(p$lambda_logistic, 0.2)
  })
})

test_that("group_permission is built from parent groups + disallowed pairs", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs(parent_group = "P01,A\nP06,B", group_disallow = "A,B"))
    p <- build_params()
    expect_false(p$group_permission[["A"]][["B"]])
    expect_false(p$group_permission[["B"]][["A"]])
    expect_true(p$group_permission[["A"]][["A"]])
  })
})

test_that("training-set files and id columns are carried", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs(training_genotype_id_col = "NAME", training_phenotype_id_col = "NAME"))
    session$setInputs(f_train_geno = list(datapath = "/tmp/tg.csv", name = "tg.csv"),
                      f_train_pheno = list(datapath = "/tmp/tp.csv", name = "tp.csv"))
    p <- build_params()
    expect_equal(p$training_genotype_file, "/tmp/tg.csv")
    expect_equal(p$training_phenotype_file, "/tmp/tp.csv")
    expect_equal(p$training_genotype_id_col, "NAME")
  })
})

test_that("index_as_trait omits multi_trait_method and trait_weights", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs(objective_mode = "index", index_col = "yield",
                                           multi_trait_method = "weighted",
                                           trait_weights = "yield: 0.5\ndisease: 0.5"))
    p <- build_params()
    expect_null(p$multi_trait_method)   # index defines the objective
    expect_null(p$trait_weights)
    expect_equal(p$index_col, "yield")
    expect_null(p$traits_to_use)
  })
})

test_that("index-column choices exclude the ID column", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs(objective_mode = "index"))
    # the index choices must not include NAME (the phenotype ID)
    idc <- nextgenCrossWorkbench:::ngcd_guess_col(cols()$pheno, c("NAME", "parent", "id"))
    choices <- setdiff(cols()$pheno, idc)
    expect_false("NAME" %in% choices)
    expect_true(all(c("yield", "disease") %in% choices))
  })
})

test_that("ploidy maps to marker_ploidy and ld_ploidy; crop is carried", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs(ploidy = "4", crop = "Barley"))
    p <- build_params()
    expect_equal(p$marker_ploidy, 4L)
    expect_equal(p$ld_ploidy, 4L)
    expect_equal(p$crop, "Barley")
  })
})

test_that("position unit auto/bp/cM/M resolves to backend bp/cM", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs(pos_unit = "cM"))
    p <- build_params()
    expect_equal(p$map_position_unit, "cM")
    expect_equal(p$map_pos_cm_col, "Position_cM")
    expect_null(p$map_pos_bp_col)

    session$setInputs(pos_unit = "bp")
    p2 <- build_params()
    expect_equal(p2$map_position_unit, "bp")
    expect_equal(p2$map_pos_bp_col, "Position_BP")
    expect_null(p2$map_pos_cm_col)

    # Morgans -> cM with divisor 0.01
    session$setInputs(pos_unit = "M")
    p3 <- build_params()
    expect_equal(p3$map_position_unit, "cM")
    expect_equal(p3$map_pos_cm_divisor, 0.01)

    # auto on demo bp positions (millions) -> bp
    session$setInputs(pos_unit = "auto")
    p4 <- build_params()
    expect_equal(p4$map_position_unit, "bp")
  })
})

test_that("RIL finite falls back to infinite when backend lacks nselfing", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs(progeny = "RIL", ril_mode = "finite", nselfing = 6))
    rv$backend <- list(supports_nselfing = FALSE)   # simulate v0.4.0 backend
    p <- build_params()
    expect_equal(p$ril_mode, "infinite")
    expect_null(p$nselfing)
    # with support, finite + nselfing is sent
    rv$backend <- list(supports_nselfing = TRUE)
    p2 <- build_params()
    expect_equal(p2$ril_mode, "finite")
    expect_equal(p2$nselfing, 6)
  })
})

test_that("edited cell updates data and flags edited", {
  testServer(srv(), {
    do.call(session$setInputs, demo_inputs())
    expect_false(rv$edited)
    session$setInputs(edit_pheno_cell_edit = data.frame(row = 1, col = 1, value = "99"))
    expect_true(rv$edited)
    expect_equal(as.numeric(rv$data$phenotype$yield[1]), 99)
  })
})
