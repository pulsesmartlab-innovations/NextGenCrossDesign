# Staged pipeline (Phase 2, Task 2): pure config-subset + invalidation logic.
# No shiny, no backend, no I/O - see R/helpers.R for ngcd_stage_cfg_subset(),
# ngcd_pipeline_init(), ngcd_pipeline_mark().

ng <- function(f) getFromNamespace(f, "nextgenCrossWorkbench")

sample_params <- function(n_crosses = 10, duplicate_threshold = 0.98) {
  list(schema = "ng_run_config.v1",
    genotype_file = "geno.csv", phenotype_file = "pheno.csv",
    map_file = "map.csv", direction_file = "dir.csv",
    genotype_id_col = "NAME", phenotype_id_col = "NAME",
    map_marker_col = "SNP", map_chr_col = "Chr", map_pos_bp_col = "Pos",
    map_position_unit = "bp", bp_per_cm = 1e6,
    direction_trait_col = "Trait", direction_column_col = "Trait",
    direction_direction_col = "Selection_direction",
    prediction_mode = "trait_by_trait", traits_to_use = "yield",
    duplicate_action = "flag", duplicate_threshold = duplicate_threshold,
    ld_pruning = FALSE, marker_ploidy = 2L,
    training_genotype_id_col = "NAME", trait_value_metric = "var_complex",
    uc_variance_source = "posterior", progeny = "RIL", recomb_model = "haldane",
    grm_method = "vanraden", assume_inbred = TRUE,
    min_effect_reliability = 0.1, selection_prop = 0.1, seed = 1L,
    multi_trait_method = "auto", threshold_policy = "soft",
    threshold_penalty_weight = 1, lambda_marker = 0,
    n_crosses = n_crosses, max_crosses_per_parent = 4L,
    min_unique_parents = 2L, max_pair_kinship = 0.5,
    optimizer = "greedy_local", allocation_method = "ocs", use_ocs = TRUE,
    lambda_group = 0.5, lambda_mating = 0, mate_relatedness = "off",
    mate_relatedness_weight = 0, min_crosses_per_parent = 1L,
    priority_breaks = c(0.5, 0.8), priority_labels = c("low", "high"),
    priority_score_weight = 1, priority_kinship_weight = 1)
}

test_that("ngcd_stage_cfg_subset: each stage gets its own keys, not others'", {
  sub <- ng("ngcd_stage_cfg_subset")
  p <- sample_params()

  qc <- sub(p, "qc")
  expect_true(all(c("genotype_file", "phenotype_file", "map_file", "direction_file",
                     "duplicate_threshold", "marker_ploidy") %in% names(qc)))
  expect_false("n_crosses" %in% names(qc))
  expect_false("lambda_group" %in% names(qc))

  predict <- sub(p, "predict")
  expect_true(all(c("trait_value_metric", "progeny", "assume_inbred",
                     "min_effect_reliability", "seed") %in% names(predict)))
  expect_false("duplicate_threshold" %in% names(predict))
  expect_false("n_crosses" %in% names(predict))

  index <- sub(p, "index")
  expect_true(all(c("multi_trait_method", "threshold_policy",
                     "threshold_penalty_weight", "lambda_marker") %in% names(index)))
  expect_false("n_crosses" %in% names(index))
  expect_false("duplicate_threshold" %in% names(index))

  allocate <- sub(p, "allocate")
  expect_true(all(c("n_crosses", "max_crosses_per_parent", "optimizer",
                     "lambda_group", "mate_relatedness_weight",
                     "min_crosses_per_parent") %in% names(allocate)))
  expect_false("duplicate_threshold" %in% names(allocate))
  expect_false("min_effect_reliability" %in% names(allocate))  # predict-only, despite min_ prefix
  expect_false("lambda_marker" %in% names(allocate))            # index-only, despite lambda_ prefix

  rank <- sub(p, "rank")
  expect_true(all(c("priority_breaks", "priority_labels",
                     "priority_score_weight", "priority_kinship_weight") %in% names(rank)))
  expect_false("n_crosses" %in% names(rank))

  # missing keys simply don't contribute
  expect_length(sub(list(n_crosses = 5), "qc"), 0)
})

# Drive a pipeline to "all done" by simulating the effect of Task 3's run:
# mark() once from a fresh init (stages come out all "stale"), then flip every
# stage's status to "done" (cfg is already correctly populated by mark()).
done_pipeline <- function(pipeline_init, mark, params, data_version) {
  p <- mark(pipeline_init(), params, data_version)
  for (s in names(p$stages)) p$stages[[s]]$status <- "done"
  p
}

test_that("ngcd_pipeline_mark: changing an allocate key marks only allocate+rank stale", {
  init <- ng("ngcd_pipeline_init"); mark <- ng("ngcd_pipeline_mark")
  p0 <- sample_params(n_crosses = 10)
  pipeline <- done_pipeline(init, mark, p0, data_version = 1L)

  p1 <- sample_params(n_crosses = 25)   # allocate-stage key changed
  pipeline2 <- mark(pipeline, p1, data_version = 1L)

  expect_identical(pipeline2$stages$qc$status, "done")
  expect_identical(pipeline2$stages$predict$status, "done")
  expect_identical(pipeline2$stages$index$status, "done")
  expect_identical(pipeline2$stages$allocate$status, "stale")
  expect_identical(pipeline2$stages$rank$status, "stale")
})

test_that("ngcd_pipeline_mark: changing a qc key marks every stage stale", {
  init <- ng("ngcd_pipeline_init"); mark <- ng("ngcd_pipeline_mark")
  p0 <- sample_params(duplicate_threshold = 0.98)
  pipeline <- done_pipeline(init, mark, p0, data_version = 1L)

  p1 <- sample_params(duplicate_threshold = 0.90)   # qc-stage key changed
  pipeline2 <- mark(pipeline, p1, data_version = 1L)

  for (s in c("qc", "predict", "index", "allocate", "rank"))
    expect_identical(pipeline2$stages[[s]]$status, "stale", info = s)
})

test_that("ngcd_pipeline_mark: bumping data_version marks every stage stale", {
  init <- ng("ngcd_pipeline_init"); mark <- ng("ngcd_pipeline_mark")
  p0 <- sample_params()
  pipeline <- done_pipeline(init, mark, p0, data_version = 1L)

  pipeline2 <- mark(pipeline, p0, data_version = 2L)   # same params, new data version

  for (s in c("qc", "predict", "index", "allocate", "rank"))
    expect_identical(pipeline2$stages[[s]]$status, "stale", info = s)
  expect_identical(pipeline2$input_version, 2L)
})

test_that("ngcd_pipeline_mark: unchanged params + same data_version keeps done stages done", {
  init <- ng("ngcd_pipeline_init"); mark <- ng("ngcd_pipeline_mark")
  p0 <- sample_params()
  pipeline <- done_pipeline(init, mark, p0, data_version = 1L)

  pipeline2 <- mark(pipeline, p0, data_version = 1L)

  for (s in c("qc", "predict", "index", "allocate", "rank"))
    expect_identical(pipeline2$stages[[s]]$status, "done", info = s)
})

test_that("ngcd_pipeline_mark: never upgrades a stale/blocked stage to done", {
  init <- ng("ngcd_pipeline_init"); mark <- ng("ngcd_pipeline_mark")
  p0 <- sample_params()
  pipeline <- done_pipeline(init, mark, p0, data_version = 1L)
  pipeline$stages$predict$status <- "blocked"   # e.g. a QC gate blocked it in Task 3

  pipeline2 <- mark(pipeline, p0, data_version = 1L)   # nothing changed

  expect_identical(pipeline2$stages$qc$status, "done")
  expect_identical(pipeline2$stages$predict$status, "blocked")  # kept, not upgraded
  # downstream of the blocked stage cascades to stale even though nothing changed
  expect_identical(pipeline2$stages$index$status, "stale")
  expect_identical(pipeline2$stages$allocate$status, "stale")
  expect_identical(pipeline2$stages$rank$status, "stale")
})

test_that("ngcd_pipeline_init: fresh pipeline has all stages stale with no cfg/json", {
  p <- ng("ngcd_pipeline_init")()
  expect_null(p$run_dir)
  expect_null(p$input_version)
  expect_identical(names(p$stages), c("qc", "predict", "index", "allocate", "rank"))
  for (s in p$stages) {
    expect_identical(s$status, "stale")
    expect_null(s$cfg)
    expect_null(s$json)
    expect_null(s$ran_at)
  }
})
