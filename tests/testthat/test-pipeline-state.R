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

test_that("ngcd_pipeline_mark: changing ril_mode/nselfing marks predict+index+allocate+rank stale, qc stays done", {
  init <- ng("ngcd_pipeline_init"); mark <- ng("ngcd_pipeline_mark")
  p0 <- c(sample_params(), list(ril_mode = "infinite"))
  pipeline <- done_pipeline(init, mark, p0, data_version = 1L)

  p1 <- c(sample_params(), list(ril_mode = "finite", nselfing = 8L))
  pipeline2 <- mark(pipeline, p1, data_version = 1L)

  expect_identical(pipeline2$stages$qc$status, "done")
  expect_identical(pipeline2$stages$predict$status, "stale")
  expect_identical(pipeline2$stages$index$status, "stale")
  expect_identical(pipeline2$stages$allocate$status, "stale")
  expect_identical(pipeline2$stages$rank$status, "stale")
})

test_that("ngcd_pipeline_mark: changing a rank-only meta key marks ONLY rank stale", {
  init <- ng("ngcd_pipeline_init"); mark <- ng("ngcd_pipeline_mark")
  p0 <- c(sample_params(), list(robust_allocation = FALSE, cross_number_mode = "fixed",
                                 multitrait_joint_prob = FALSE))
  pipeline <- done_pipeline(init, mark, p0, data_version = 1L)

  p1 <- c(sample_params(), list(robust_allocation = TRUE, cross_number_mode = "auto",
                                 multitrait_joint_prob = TRUE))
  pipeline2 <- mark(pipeline, p1, data_version = 1L)

  expect_identical(pipeline2$stages$qc$status, "done")
  expect_identical(pipeline2$stages$predict$status, "done")
  expect_identical(pipeline2$stages$index$status, "done")
  expect_identical(pipeline2$stages$allocate$status, "done")
  expect_identical(pipeline2$stages$rank$status, "stale")
})

# A representative FULL build_params() param set (R/app.R): every top-level key
# build_params() can emit, including the conditional advanced-knob keys. Kept in
# sync with build_params() so the partition test below fails the moment a NEW
# config key lands in zero stages without being deliberately allow-listed.
full_build_params <- function() {
  c(sample_params(), list(
    # qc-stage keys not in sample_params()
    map_pos_cm_col = "cM", map_pos_cm_divisor = 1, ld_ploidy = 2L,
    index_col = "index", index_direction = "increase",
    duplicate_maf_min = 0.01, duplicate_max_missing_prop = 0.5,
    duplicate_min_compared_markers = 50L,
    ld_window = 100L, ld_r2_threshold = 0.9, ld_maf_threshold = 0.05, ld_backend = "auto",
    # predict-stage keys
    ril_mode = "finite", nselfing = 8L, method_varPMV = "fast",
    run_posterior_prediction = TRUE, posterior_method = "mcmc", n_iter = 1000L, burn_in = 200L,
    training_genotype_file = "tg.csv", training_phenotype_file = "tp.csv",
    training_phenotype_id_col = "NAME",
    # index-stage keys
    trait_weights = c(yield = 1), threshold_penalty_autoscale = TRUE,
    trait_checks = data.frame(trait = "yield", check = "chk", stringsAsFactors = FALSE),
    check_basis = "gebv", exclude_threshold_violators = FALSE,
    drop_lethal_carrier_crosses = FALSE, marker_target_spec = list(),
    lethal_spec = list(),
    # allocate-stage keys
    lambda_parent_use = 0, lambda_parent_use_mode = "absolute",
    local_iter = 2000L, ocs_iter = 5L,
    budget = 1000, lambda_cost = 0, lambda_logistic = 0,
    strategy = "balanced", diversity_emphasis = 0.5, target_coancestry = 0.05,
    evol_solutions = 50L, evol_iterations = 100L, evol_stop = 20L, evol_seed = 1L,
    alphamate_mode = "island", alphamate_target_degree = 30,
    alphamate_max_contributions = 10L, alphamate_n_threads = 1L,
    alphamate_runtime_path = "/opt/am", alphamate_workdir = "/tmp/am",
    alphamate_number_of_parents = 20L, alphamate_lambda_group = 0.5,
    alphamate_keep_files = FALSE, alphamate_lambda_grid = c(0, 1),
    alphamate_evol_solutions = 50L, alphamate_evol_iterations = 100L, alphamate_evol_stop = 20L,
    committed_crosses = list(parent1 = "A", parent2 = "B"),
    parent_group = list(A = "g1"), group_quota = list(g1 = 1),
    group_permission = matrix(TRUE, 1, 1), cross_cost = list(cost = 1),
    cost_col = "cost", logistic_col = "log",
    # rank-stage keys (post-run meta + terminal output side-effects)
    crop = "wheat", priority_threshold_weight = 1,
    cross_number_mode = "auto",
    cross_sweep_k_min = 3L, cross_sweep_k_max = 30L, cross_sweep_k_step = 1L,
    cross_sweep_criterion = "elbow_relative", cross_sweep_relative_threshold = 0.05,
    cross_sweep_ne_min = 30, cross_sweep_coancestry_max = 0.05,
    robust_allocation = TRUE, robust_objective = "posterior_quantile",
    robustness_quantile = 0.25, robust_top_n_target = 10L,
    family_size_total_progeny = 200L, family_size_min = 1L, family_size_max = 50L,
    pareto_explore = TRUE, pareto_lambdas = "0,0.5,1",
    multitrait_joint_prob = TRUE, multitrait_targets = "yield>=5",
    include_trait_gebv = FALSE, write_outputs = TRUE, write_figures = TRUE,
    output_file = "crossing_plan.xlsx",
    # execution-only / meta keys (see allow_list below)
    use_parallel = TRUE, n_threads = 2L))
}

# Config keys that GENUINELY invalidate no stage, so may land in zero stage
# subsets. Explicit + commented so a future new key that maps nowhere FAILS the
# completeness assertion below instead of silently becoming a no-op.
#   schema                 - config-schema tag, never a backend arg
#   use_parallel/n_threads - execution-only knobs; results are deterministic
#                            (same seed) regardless, so nothing to re-run
#   workflow/run_dir/stage - staged-runner control keys (not emitted by a
#                            standard build_params() run; allow-listed defensively)
partition_allow_list <- c("schema", "use_parallel", "n_threads",
                          "workflow", "run_dir", "stage")

test_that("ngcd_stage_key_patterns: the 5-stage partition is collision-free", {
  sub <- ng("ngcd_stage_cfg_subset")
  full <- full_build_params()

  stages <- c("qc", "predict", "index", "allocate", "rank")
  subsets <- lapply(stages, function(s) names(sub(full, s)))
  names(subsets) <- stages

  all_keys <- unlist(subsets)
  dupes <- unique(all_keys[duplicated(all_keys)])
  expect_identical(dupes, character(0))

  # Every key in the representative params list lands in at most one stage.
  for (k in names(full)) {
    hits <- vapply(subsets, function(s) k %in% s, logical(1))
    expect_lte(sum(hits), 1)
  }
})

test_that("ngcd_stage_key_patterns: the partition is COMPLETE (no key invalidates nothing)", {
  sub <- ng("ngcd_stage_cfg_subset")
  full <- full_build_params()
  stages <- c("qc", "predict", "index", "allocate", "rank")
  subsets <- lapply(stages, function(s) names(sub(full, s)))

  # Every key must invalidate >= 1 stage, unless it's an explicitly allow-listed
  # non-invalidating key. A new build_params() key that maps to no stage (the
  # write_outputs / include_trait_gebv class of bug) fails here.
  for (k in names(full)) {
    if (k %in% partition_allow_list) next
    hits <- vapply(subsets, function(s) k %in% s, logical(1))
    expect_gte(sum(hits), 1)  # informative failure: reports the orphaned key
    if (sum(hits) < 1) message("orphaned config key (invalidates no stage): ", k)
  }

  # And the allow-list must stay honest: every allow-listed key that IS present
  # in the param set really maps to nothing (else it should be dropped from the
  # allow-list rather than hidden by it).
  for (k in intersect(partition_allow_list, names(full))) {
    hits <- vapply(subsets, function(s) k %in% s, logical(1))
    expect_identical(sum(hits), 0L, info = k)
  }
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
