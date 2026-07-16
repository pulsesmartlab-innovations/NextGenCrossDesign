# ===========================================================================
# app.R  -  the workbench UI + server, as package functions
# ===========================================================================

# Common crops for the editable target-crop selector (users can also type their own).
NGCD_CROPS <- c("Wheat (spring)", "Wheat (winter)", "Durum wheat", "Barley", "Oat",
  "Maize", "Soybean", "Dry bean", "Field pea", "Lentil", "Chickpea", "Faba bean",
  "Canola", "Flax", "Sunflower", "Sugar beet", "Potato", "Sorghum", "Rice",
  "Cotton", "Alfalfa", "Ryegrass", "Other")

# ---- UI -------------------------------------------------------------------
# `dev` (developer mode) exposes the Setup screen and the Save/Load-settings
# profile tools (with their .json import/export). It defaults to the config's
# developer_mode flag; workbench_app() also turns it on for a ?dev=1 URL.
workbench_ui <- function(cfg, dev = isTRUE(cfg$developer_mode)) {
  brandbar <- function() {
    # Prefer a user-supplied PNG, then the bundled SVG lockup, then a text wordmark.
    logo <- if (file.exists(file.path(cfg$www_dir, "ndsu-logo.png")))
      shiny::div(class = "ndsu-logo-slot", shiny::tags$img(src = "ngcd_www/ndsu-logo.png", alt = "NDSU"))
    else if (file.exists(file.path(cfg$www_dir, "ndsu-logo.svg")))
      shiny::div(class = "ndsu-logo-slot", shiny::tags$img(src = "ngcd_www/ndsu-logo.svg", alt = "North Dakota State University"))
    else shiny::div(class = "ndsu-wordmark", "NDSU")
    lab_logo <- if (file.exists(file.path(cfg$www_dir, "pulsesmartlab-logo.svg")))
      shiny::div(class = "ndsu-lab-logo",
        shiny::tags$img(src = "ngcd_www/pulsesmartlab-logo.svg", alt = "PulseSmartLab - Dr. Sikiru Atanda"))
    else NULL
    shiny::div(class = "ndsu-topbar", logo,
      shiny::div(class = "ndsu-app-title", "NextGenCrossDesign",
        shiny::tags$small("Genomic cross prediction & mate allocation")),
      shiny::div(class = "ndsu-topbar-spacer"),
      lab_logo,
      shiny::div(class = "ndsu-topbar-meta", shiny::textOutput("topbar_status", inline = TRUE)))
  }

  theme <- bslib::bs_theme(version = 5, primary = "#00583d", secondary = "#003524",
    base_font = bslib::font_collection(bslib::font_google("Montserrat", local = FALSE), "Helvetica Neue", "Arial"),
    heading_font = bslib::font_collection(bslib::font_google("Antonio", local = FALSE), "Oswald"))

  shiny::tagList(
    shiny::tags$head(shiny::tags$link(rel = "stylesheet", href = "ngcd_www/ndsu.css")),
    brandbar(),
    shiny::div(class = "ndsu-statusbar",
      shiny::span("Data: ", shiny::tags$b(shiny::textOutput("sb_data", inline = TRUE))),
      shiny::span("Backend: ", shiny::tags$b(shiny::textOutput("sb_backend", inline = TRUE))),
      shiny::span("Objective: ", shiny::tags$b(shiny::textOutput("sb_obj", inline = TRUE))),
      shiny::span("Latest run: ", shiny::tags$b(shiny::textOutput("sb_run", inline = TRUE)))),

    bslib::page_navbar(theme = theme, fillable = FALSE, id = "nav", padding = "16px",

      if (isTRUE(dev)) bslib::nav_panel("Setup",
        ngcd_section("Local R & backend", "Point the app at your R install in config.yml, then verify here."),
        ngcd_guide(1, 10, "Setup", shiny::tagList(
          shiny::tags$p("Confirm the app can reach your R and the analysis engine before you do anything else."),
          shiny::tags$ul(
            shiny::tags$li("Look at ", shiny::tags$b("Connection status"), " - you want four green badges (Rscript, Runner, backend package, version)."),
            shiny::tags$li("If any badge is red, open ", shiny::tags$code("config.yml"), " (path shown on the right), fix ", shiny::tags$code("rscript_path"), " / ", shiny::tags$code("package_library"), ", then click ", shiny::tags$b("Re-check backend"), "."),
            shiny::tags$li("The ", shiny::tags$b("Optional capabilities"), " panel shows extra features and whether the packages they need are installed - missing ones are fine.")),
          shiny::tags$p(class = "help-hint", "You only need to do this once per machine.")),
          next_hint = "go to the Data tab to load your CSV files."),
        bslib::layout_columns(col_widths = c(7, 5),
          bslib::card(bslib::card_header("Connection status"),
            shiny::uiOutput("setup_status"),
            shiny::actionButton("recheck", "Re-check backend", class = "btn-ndsu"),
            shiny::tags$hr(), shiny::verbatimTextOutput("setup_detail")),
          bslib::card(bslib::card_header("How configuration works"),
            shiny::div(class = "help-hint",
              shiny::tags$p("The backend runs as a separate R process. Edit ",
                            shiny::tags$code("config.yml"), " to set ",
                            shiny::tags$code("rscript_path"), " and (optionally) ",
                            shiny::tags$code("package_library"), "."),
              shiny::tags$p("Config file: ", shiny::tags$code(cfg$config_path))))),
        bslib::card(bslib::card_header("Optional capabilities & requirements"),
          shiny::div(class = "help-hint",
            "These optional R packages / tools unlock extra features. Missing ones are fine - the affected feature is disabled or falls back to a native path."),
          shiny::uiOutput("requirements"))),

      bslib::nav_panel("Data",
        ngcd_section("Data import, column mapping & on-the-fly editing",
          "Load four CSVs (or the demo). Tables are editable - fix a value or change a direction, and the edited data is used on the next run."),
        ngcd_guide(2, 10, "Data", shiny::tagList(
          shiny::tags$p("Give the app your four data files and tell it which column means what."),
          shiny::tags$ul(
            shiny::tags$li("Pick ", shiny::tags$b("Bundled demo data"), " to try it immediately, or ", shiny::tags$b("Upload my CSV files"), " for your own: genotype, phenotype, marker map, and trait direction."),
            shiny::tags$li("Check ", shiny::tags$b("Column mapping"), " - the app auto-detects the ID, marker, position and direction columns; override any that guessed wrong."),
            shiny::tags$li("Read the ", shiny::tags$b("Data checks"), " badges: markers and parents should say \"aligned\". If not, the callout tells you what to fix (or tick a reconcile box)."),
            shiny::tags$li("Need a quick fix? Double-click any cell in the tables to edit it; the change is used on your next run.")),
          shiny::tags$p(class = "help-hint", "Genotype dosages are 0/1/2; inbred parents should be 0 or 2.")),
          next_hint = "Objective - choose what you're selecting for."),
        bslib::layout_columns(col_widths = c(4, 8),
          bslib::card(bslib::card_header("Data source"),
            shiny::radioButtons("workflow", "Analysis type",
              c("Standard cross prediction (diploid 0/1/2)" = "standard",
                "Polyploid design (any ploidy, dosage 0..ploidy)" = "polyploid"),
              selected = "standard"),
            shiny::conditionalPanel("input.workflow == 'polyploid'",
              ngcd_callout(kind = "info",
                "Polyploid mode uses ", shiny::tags$b("ng_design_crosses_poly"),
                ": upload a dosage matrix (0..ploidy) and a single-trait phenotype. A marker map and trait-direction file are not needed. ",
                shiny::tags$b("The bundled demo is tetraploid - set Ploidy = 4."))),
            shiny::radioButtons("data_source", NULL,
              c("Bundled demo data" = "demo", "Upload my CSV files" = "upload"), selected = "demo"),
            shiny::conditionalPanel("input.data_source == 'upload'",
              shiny::fileInput("f_geno", "Genotype / dosage CSV", accept = c(".csv", ".txt", ".tsv")),
              shiny::fileInput("f_pheno", "Phenotype CSV", accept = c(".csv", ".txt", ".tsv")),
              shiny::conditionalPanel("input.workflow == 'standard'",
                shiny::fileInput("f_map", "Marker map CSV", accept = c(".csv", ".txt", ".tsv")),
                shiny::fileInput("f_dir", "Trait direction CSV", accept = c(".csv", ".txt", ".tsv"))),
              shiny::div(class = "help-hint", style = "margin-top:-6px",
                "Comma, semicolon or tab separated; a UTF-8 byte-order mark (Excel) is handled automatically."),
              shiny::uiOutput("load_status")),
            shiny::conditionalPanel("input.data_source == 'demo'",
              ngcd_callout("Demo: 10 inbred parents, 12 markers, traits ", shiny::tags$b("yield"),
                           " (increase) and ", shiny::tags$b("disease"), " (decrease).")),
            shiny::tags$hr(),
            shiny::selectizeInput("crop", "Target crop",
              choices = NGCD_CROPS, selected = character(0),
              options = list(create = TRUE, placeholder = "Select or type a crop...")),
            shiny::selectInput("ploidy", "Ploidy",
              choices = c("Diploid (2)" = "2", "Tetraploid (4)" = "4",
                          "Hexaploid (6)" = "6", "Octoploid (8)" = "8"), selected = "2"),
            shiny::conditionalPanel("input.workflow == 'standard'",
              shiny::div(class = "help-hint",
                "Ploidy sets the marker-steering & LD allele model. The core DH/RIL scoring assumes diploid.")),
            shiny::conditionalPanel("input.workflow == 'polyploid'",
              shiny::uiOutput("poly_trait_ui"),
              bslib::layout_columns(col_widths = c(6, 6),
                shiny::checkboxInput("poly_dominance", "Model dominance (heterosis)", FALSE),
                shiny::selectInput("poly_gain", "Cross value", c("mean", "usefulness"), selected = "mean")),
              bslib::layout_columns(col_widths = c(6, 6),
                shiny::selectInput("poly_grm_method", "GRM method", c("vanraden", "yang"), selected = "vanraden"),
                shiny::numericInput("poly_double_reduction", "Double reduction (0..1/6)", 0, min = 0, max = 0.2, step = 0.01)),
              shiny::checkboxInput("poly_run_qc", "Run ploidy-aware QC", TRUE),
              shiny::div(class = "help-hint",
                "Dominance is for clonal/heterosis crops (select clones on genotypic value). Double reduction applies to autopolyploids (ploidy >= 4).")),
            shiny::tags$hr(),
            ngcd_callout(kind = "info", shiny::tags$b("Editing: "),
              "double-click a cell to change it. ",
              shiny::actionLink("reset_data", "Reset edits"), "."),
            shiny::uiOutput("edit_flag")),
          bslib::card(bslib::card_header("Column mapping"), shiny::uiOutput("colmap_ui"))),
        bslib::card(bslib::card_header("Data checks (marker & ID alignment)"),
          shiny::uiOutput("data_checks"),
          shiny::checkboxInput("restrict_shared_markers",
            "Use only markers present in BOTH genotype and map (drop unmapped genotype markers)", FALSE),
          shiny::checkboxInput("restrict_shared_ids",
            "Use only parents present in BOTH genotype and phenotype", FALSE),
          shiny::checkboxInput("drop_noninbred_parents",
            "Exclude non-inbred parents (>2% heterozygous markers) for the DH/RIL model", FALSE)),
        bslib::card(bslib::card_header("Input data (editable)"),
          bslib::navset_tab(
            bslib::nav_panel("Genotype", DT::DTOutput("edit_geno")),
            bslib::nav_panel("Phenotype", DT::DTOutput("edit_pheno")),
            bslib::nav_panel("Marker map", DT::DTOutput("edit_map")),
            bslib::nav_panel("Trait direction", DT::DTOutput("edit_dir"))))),

      bslib::nav_panel("Objective",
        ngcd_section("Prediction mode & multi-trait objective",
          "Trait directions are explicit - risk traits may decrease."),
        ngcd_guide(3, 10, "Objective", shiny::tagList(
          shiny::tags$p("Tell the app what \"good\" means for your program."),
          shiny::tags$ul(
            shiny::tags$li(shiny::tags$b("Trait-by-trait"), " (usual choice): the app scores each trait and combines them. ", shiny::tags$b("Index-as-trait"), ": use only if your phenotype file already has one trusted selection-index column."),
            shiny::tags$li("Tick the traits to include (leave all ticked to use every trait)."),
            shiny::tags$li(shiny::tags$b("Multi-trait method"), ": start with ", shiny::tags$code("auto"), ". Use ", shiny::tags$code("weighted"), " if you have relative trait weights; ", shiny::tags$code("economic_index"), " / ", shiny::tags$code("desired_gain"), " if your direction file carries economic weights.")),
          shiny::tags$p(class = "help-hint", "Direction is set in your trait-direction file - e.g. yield increases, disease decreases.")),
          next_hint = "Scoring - how each cross is valued."),
        bslib::layout_columns(col_widths = c(6, 6),
          bslib::card(bslib::card_header("Prediction mode"),
            shiny::radioButtons("prediction_mode", NULL,
              c("Trait-by-trait" = "trait_by_trait", "Index-as-trait" = "index_as_trait"),
              selected = "trait_by_trait"),
            shiny::conditionalPanel("input.prediction_mode == 'trait_by_trait'", shiny::uiOutput("traits_to_use_ui")),
            shiny::conditionalPanel("input.prediction_mode == 'index_as_trait'",
              shiny::uiOutput("index_col_ui"),
              shiny::selectInput("index_direction", "Index direction", c("increase","decrease")))),
          bslib::card(bslib::card_header("Multi-trait method"),
            shiny::selectInput("multi_trait_method", "Method",
              c("auto","weighted","economic_index","desired_gain"), selected = "auto"),
            shiny::conditionalPanel("input.multi_trait_method == 'weighted'",
              shiny::textAreaInput("trait_weights", "Trait weights ('trait: value' per line)",
                                   placeholder = "yield: 0.5\ndisease: 0.5", height = "90px")),
            shiny::selectInput("threshold_policy", "Threshold policy", c("soft","strict")),
            shiny::numericInput("threshold_penalty_weight", "Threshold penalty weight", 1, min = 0, step = 0.1),
            shiny::checkboxInput("threshold_penalty_autoscale", "Autoscale threshold penalty", TRUE)))),

      bslib::nav_panel("Scoring",
        ngcd_section("Trait value, variance & breeding system"),
        ngcd_guide(4, 10, "Scoring", shiny::tagList(
          shiny::tags$p("Choose how a cross's merit is scored and what kind of progeny you'll make. The defaults are good for most programs."),
          shiny::tags$ul(
            shiny::tags$li(shiny::tags$b("Merit metric"), ": keep ", shiny::tags$code("var_complex"), " (usefulness) for most cases. Switch to ", shiny::tags$code("mean"), " for highly polygenic traits or small training sets."),
            shiny::tags$li(shiny::tags$b("Breeding system"), ": ", shiny::tags$code("DH"), " for doubled haploids, ", shiny::tags$code("RIL"), " for recombinant inbred lines. Leave ", shiny::tags$b("Assume inbred parents"), " ticked unless your parents carry heterozygosity."),
            shiny::tags$li(shiny::tags$b("Selection proportion"), " is the top fraction of progeny you expect to keep (e.g. 0.10 = top 10%).")),
          shiny::tags$p(class = "help-hint", "If Setup/Data flagged non-inbred parents, either drop them (Data tab) or uncheck Assume inbred here.")),
          next_hint = "QC - clean the data before running."),
        bslib::layout_columns(col_widths = c(6, 6),
          bslib::card(bslib::card_header("Merit metric"),
            shiny::selectInput("trait_value_metric", "Trait value metric",
              c("var_complex","uc","pmv","vpm","mean","var_simple"), selected = "var_complex"),
            shiny::div(class = "help-hint", "Recommended: var_complex. 'mean' for polygenic/small training. 'var_simple' is diversity, not merit."),
            shiny::selectInput("uc_variance_source", "UC variance source", c("pmv","vpm","var_simple")),
            shiny::selectInput("method_varPMV", "PMV method", c("fast","full_posterior")),
            shiny::sliderInput("selection_prop", "Selection proportion", 0.01, 0.5, 0.10, step = 0.01)),
          bslib::card(bslib::card_header("Breeding system"),
            shiny::selectInput("progeny", "Progeny type",
              c("DH (doubled haploid)" = "DH", "RIL (recombinant inbred lines)" = "RIL")),
            shiny::conditionalPanel("input.progeny == 'RIL'",
              shiny::selectInput("ril_mode", "RIL mode", c("infinite", "finite"), selected = "infinite"),
              shiny::conditionalPanel("input.ril_mode == 'finite'",
                shiny::numericInput("nselfing", "Number of selfing generations", 8, min = 1, step = 1)),
              shiny::uiOutput("ril_note")),
            shiny::selectInput("recombination_model", "Recombination model", c("haldane","kosambi")),
            shiny::selectInput("grm_method", "GRM method", c("vanraden","yang")),
            shiny::checkboxInput("assume_inbred", "Assume inbred parents", TRUE),
            shiny::numericInput("min_effect_reliability", "Min marker-effect reliability", 0.35, min = 0, max = 1, step = 0.05)))),

      bslib::nav_panel("QC",
        ngcd_section("Data QC & duplicate handling", "Preflight runs in the backend; blocker issues stop the run."),
        ngcd_guide(5, 10, "QC", shiny::tagList(
          shiny::tags$p("Decide how to handle duplicate genotypes and optional marker thinning. The backend also runs its own preflight when you hit Run."),
          shiny::tags$ul(
            shiny::tags$li(shiny::tags$b("Duplicate action"), ": ", shiny::tags$code("remove"), " drops near-identical genotypes (recommended for real data), ", shiny::tags$code("report"), " only flags them, ", shiny::tags$code("none"), " skips the check."),
            shiny::tags$li(shiny::tags$b("LD pruning"), " is optional - turn it on only if you have many correlated markers and want to thin them.")),
          shiny::tags$p(class = "help-hint", "You can leave everything at defaults for a first run.")),
          next_hint = "Allocation - build the crossing plan."),
        bslib::layout_columns(col_widths = c(6, 6),
          bslib::card(bslib::card_header("Duplicate genotypes"),
            shiny::selectInput("duplicate_action", "Duplicate action", c("remove","report","none"), selected = "remove"),
            shiny::numericInput("duplicate_threshold", "Identity threshold", 0.995, min = 0, max = 1, step = 0.001),
            shiny::numericInput("duplicate_maf_min", "Min MAF", 0.01, min = 0, max = 0.5, step = 0.01),
            shiny::numericInput("duplicate_max_missing_prop", "Max missing proportion", 0.40, min = 0, max = 1, step = 0.05),
            shiny::numericInput("duplicate_min_compared_markers", "Min compared markers", 100, min = 1, step = 1)),
          bslib::card(bslib::card_header("LD pruning (optional)"),
            shiny::checkboxInput("ld_pruning", "Enable LD pruning", FALSE),
            shiny::conditionalPanel("input.ld_pruning == true",
              shiny::numericInput("ld_window", "Window", 100, min = 1),
              shiny::numericInput("ld_r2_threshold", "r^2 threshold", 0.9, min = 0, max = 1, step = 0.01),
              shiny::numericInput("ld_maf_threshold", "MAF threshold", 0.01, min = 0, max = 0.5, step = 0.01),
              shiny::selectInput("ld_backend", "Backend", c("auto","cpp","r")))))),

      bslib::nav_panel("Allocation",
        ngcd_section("Mate allocation & gain-diversity balance"),
        ngcd_guide(6, 10, "Allocation", shiny::tagList(
          shiny::tags$p("The heart of the tool: how many crosses to make, how hard to push for gain vs. diversity, and which engine picks them."),
          shiny::tags$ul(
            shiny::tags$li(shiny::tags$b("Number of crosses (K)"), " and ", shiny::tags$b("Max uses per parent"), " set the plan size. Watch for a feasibility warning here or on Run - K can't exceed parents x max-uses / 2."),
            shiny::tags$li(shiny::tags$b("Optimizer"), ": ", shiny::tags$code("auto"), " is safe; ", shiny::tags$code("evolution"), " is recommended for real recurrent programs. MIP options need lpSolve (see Setup)."),
            shiny::tags$li(shiny::tags$b("Gain-diversity dial"), ": pick a preset (High gain / Balanced / Diversity), or use the slider, or cap coancestry directly - choose just one."),
            shiny::tags$li("OCS penalties and AlphaMate-style controls are advanced; the defaults are fine to start.")),
          shiny::tags$p(class = "help-hint", "Moving the dial toward diversity spreads parents out; toward gain concentrates on the best.")),
          next_hint = "Advanced (optional) or skip to Output."),
        bslib::layout_columns(col_widths = c(4, 4, 4),
          bslib::card(bslib::card_header("Plan size & parent use"),
            shiny::radioButtons("cross_number_mode", "Number of crosses",
              c("Fixed number" = "fixed",
                "Optimize automatically (diminishing returns)" = "auto"),
              selected = "fixed"),
            shiny::conditionalPanel("input.cross_number_mode == 'fixed'",
              shiny::numericInput("n_crosses", "Number of crosses (K)", 10, min = 1, step = 1)),
            shiny::conditionalPanel("input.cross_number_mode == 'auto'",
              shiny::div(class = "help-hint",
                "The app sweeps K over a range, then recommends where extra crosses stop adding much gain (the elbow). Your plan is built at that K."),
              bslib::layout_columns(col_widths = c(4, 4, 4),
                shiny::numericInput("cross_sweep_k_min", "K min", 3, min = 1, step = 1),
                shiny::numericInput("cross_sweep_k_max", "K max", 30, min = 2, step = 1),
                shiny::numericInput("cross_sweep_k_step", "step", 1, min = 1, step = 1)),
              shiny::selectInput("cross_sweep_criterion", "Recommendation rule",
                c("Diminishing returns (relative)" = "elbow_relative",
                  "Diminishing returns (kneedle)"  = "elbow_kneedle",
                  "Effective population size floor" = "ne_target",
                  "Coancestry budget"               = "coancestry_budget"),
                selected = "elbow_relative"),
              shiny::conditionalPanel("input.cross_sweep_criterion == 'elbow_relative'",
                shiny::numericInput("cross_sweep_relative_threshold",
                  "Stop when marginal gain falls below this fraction of the first step", 0.05, min = 0.001, max = 1, step = 0.01)),
              shiny::conditionalPanel("input.cross_sweep_criterion == 'ne_target'",
                shiny::numericInput("cross_sweep_ne_min", "Minimum effective population size (Ne)", 30, min = 1, step = 1)),
              shiny::conditionalPanel("input.cross_sweep_criterion == 'coancestry_budget'",
                shiny::numericInput("cross_sweep_coancestry_max", "Maximum group coancestry", 0.05, min = 0, step = 0.01))),
            shiny::numericInput("max_uses_per_parent", "Max uses per parent", 4, min = 1, step = 1),
            shiny::numericInput("min_unique_parents", "Min unique parents (blank = auto)", NA, min = 1, step = 1),
            shiny::numericInput("max_pair_kinship", "Max pair kinship (blank = off)", NA, step = 0.05)),
          bslib::card(bslib::card_header("Optimizer & method"),
            shiny::selectInput("optimizer", "Optimizer",
              c("auto","evolution","greedy_local","repair_local","mip_linear","mip_contribution"), selected = "auto"),
            shiny::div(class = "help-hint", "'evolution' recommended for real programs. MIP needs lpSolve."),
            shiny::uiOutput("mip_note"),
            shiny::selectInput("allocation_method", "Allocation method", c("ocs","alphamate_style","alphamate_executable")),
            shiny::uiOutput("alphamate_note"),
            shiny::checkboxInput("use_ocs", "Use OCS", TRUE),
            shiny::conditionalPanel("input.optimizer == 'evolution'",
              shiny::numericInput("evol_solutions", "Evolution: solutions", 100, min = 2),
              shiny::numericInput("evol_iterations", "Evolution: iterations", 200, min = 1),
              shiny::numericInput("evol_stop", "Evolution: stop after", 40, min = 1),
              shiny::numericInput("evol_seed", "Evolution: seed (blank = none)", NA))),
          bslib::card(bslib::card_header("Gain-diversity dial"),
            shiny::radioButtons("diversity_mode", "Balance control",
              c("Strategy preset" = "strategy", "Emphasis slider" = "emphasis", "Coancestry cap" = "target"),
              selected = "strategy"),
            shiny::conditionalPanel("input.diversity_mode == 'strategy'",
              shiny::selectInput("strategy", "Strategy", c("high_gain","balanced","diversity"), selected = "balanced")),
            shiny::conditionalPanel("input.diversity_mode == 'emphasis'",
              shiny::sliderInput("diversity_emphasis", "0 = max gain ... 100 = max diversity", 0, 100, 45, step = 1)),
            shiny::conditionalPanel("input.diversity_mode == 'target'",
              shiny::numericInput("target_coancestry", "Target group coancestry", 0.05, min = 0, step = 0.01)))),
        bslib::card(bslib::card_header("Robust posterior allocation (optional)"),
          shiny::div(class = "help-hint",
            "Re-optimize the plan on a pessimistic posterior quantile so the chosen crosses hold up under marker-effect uncertainty, not just the point estimate. Turning this on also enables posterior prediction."),
          bslib::layout_columns(col_widths = c(4, 4, 4),
            shiny::checkboxInput("robust_allocation", "Enable robust posterior allocation", FALSE),
            shiny::conditionalPanel("input.robust_allocation == true",
              shiny::selectInput("robust_objective", "Robust objective",
                c("Pessimistic quantile of gain" = "posterior_quantile",
                  "Top-N inclusion probability"   = "posterior_topn_prob"),
                selected = "posterior_quantile")),
            shiny::conditionalPanel("input.robust_allocation == true && input.robust_objective == 'posterior_quantile'",
              shiny::sliderInput("robustness_quantile", "Robustness quantile (lower = more cautious)",
                0.05, 0.5, 0.25, step = 0.05))),
          shiny::conditionalPanel("input.robust_allocation == true && input.robust_objective == 'posterior_topn_prob'",
            shiny::numericInput("robust_top_n_target", "Top-N target (probability a cross is in the best N)", 10, min = 1, step = 1)),
          shiny::conditionalPanel("input.robust_allocation == true",
            shiny::div(class = "help-hint",
              "The robust plan is shown alongside your standard plan on the Results screen, with the crosses that changed highlighted."))),
        bslib::card(bslib::card_header("OCS penalties & optimizer internals (advanced)"),
          bslib::layout_columns(col_widths = c(3,3,3,3),
            shiny::numericInput("lambda_group", "lambda_group", 0.05, step = 0.01),
            shiny::numericInput("lambda_mating", "lambda_mating", 0.02, step = 0.01),
            shiny::numericInput("lambda_parent_use", "lambda_parent_use", 0, step = 0.01),
            shiny::selectInput("lambda_parent_use_mode", "parent-use mode", c("absolute","adaptive"))),
          bslib::layout_columns(col_widths = c(6,6),
            shiny::numericInput("local_iter", "Local search iterations", 2000, min = 1, step = 100),
            shiny::numericInput("ocs_iter", "OCS refine iterations", 5, min = 1, step = 1))),
        shiny::conditionalPanel("input.allocation_method != 'ocs'",
          bslib::card(bslib::card_header("AlphaMate-style controls"),
            bslib::layout_columns(col_widths = c(3,3,3,3),
              shiny::selectInput("alphamate_mode", "Mode", c("ModeOptTarget1","ModeMaxCriterion","ModeMinCoancestry")),
              shiny::numericInput("alphamate_target_degree", "Target degree (20/45/70)", 45, min = 0, max = 90),
              shiny::numericInput("alphamate_max_contributions", "Max contributions (blank = auto)", NA, min = 1),
              shiny::numericInput("alphamate_n_threads", "Threads", 1, min = 1, step = 1)),
            shiny::conditionalPanel("input.allocation_method == 'alphamate_executable'",
              shiny::textInput("alphamate_executable", "AlphaMate executable path", cfg$alphamate_executable %||% ""),
              bslib::layout_columns(col_widths = c(6,6),
                shiny::textInput("alphamate_runtime_path", "Runtime path (blank = default)", ""),
                shiny::textInput("alphamate_workdir", "Working directory (blank = temp)", "")),
              bslib::layout_columns(col_widths = c(4,4,4),
                shiny::numericInput("alphamate_number_of_parents", "Number of parents (blank = auto)", NA, min = 1),
                shiny::numericInput("alphamate_lambda_group", "lambda_group (blank = auto)", NA, step = 0.01),
                shiny::checkboxInput("alphamate_keep_files", "Keep run files", FALSE)),
              shiny::textInput("alphamate_lambda_grid", "lambda grid (comma-separated, blank = auto)", ""),
              bslib::layout_columns(col_widths = c(4,4,4),
                shiny::numericInput("alphamate_evol_solutions", "Evol solutions", 100, min = 2),
                shiny::numericInput("alphamate_evol_iterations", "Evol iterations", 1000, min = 1),
                shiny::numericInput("alphamate_evol_stop", "Evol stop", 200, min = 1)))))),

      bslib::nav_panel("Advanced",
        ngcd_section("Advanced mate-selection controls", "All optional; default to off."),
        ngcd_guide(7, 10, "Advanced", shiny::tagList(
          shiny::tags$p("Optional breeder constraints. Everything here is off by default - skip this tab entirely for a standard run."),
          shiny::tags$ul(
            shiny::tags$li(shiny::tags$b("Progeny inbreeding"), ": penalize related matings."),
            shiny::tags$li(shiny::tags$b("Breeder constraints"), ": force specific matings (committed), require a used parent to be used at least N times, assign parents to groups (e.g. heterotic pools) and cap crosses per group pair."),
            shiny::tags$li(shiny::tags$b("Marker steering & lethal guarding"), ": push a marker's allele frequency, and exclude carrier x carrier matings at a deleterious locus."),
            shiny::tags$li(shiny::tags$b("Cost & logistics"), ": upload a per-cross cost table, set a budget cap and cost/distance emphasis."),
            shiny::tags$li(shiny::tags$b("Training-set augmentation"), ": add extra genotyped+phenotyped lines that sharpen marker effects but are never parents.")),
          shiny::tags$p(class = "help-hint", "Formats are shown in each box, e.g. committed matings are one 'parent1,parent2' per line.")),
          next_hint = "Output - priority tiers and files.", open = FALSE),
        bslib::accordion(open = FALSE,
          bslib::accordion_panel("Progeny inbreeding",
            shiny::numericInput("lambda_progeny_inbreeding", "Avoid inbred/related matings (penalty)", 0, min = 0, step = 0.01)),
          bslib::accordion_panel("Breeder mating constraints",
            shiny::numericInput("min_crosses_per_parent", "Min-use-if-used (0 = off)", 0, min = 0, step = 1),
            shiny::textAreaInput("committed_crosses", "Committed matings ('parent1,parent2' per line)", "", height = "80px"),
            shiny::textAreaInput("parent_group", "Parent groups ('parent,group' per line)", "", height = "80px"),
            shiny::textAreaInput("group_quota", "Group-pair quotas ('A||B: cap' per line)", "", height = "60px"),
            shiny::textAreaInput("group_disallow", "Disallowed group pairs ('groupA,groupB' per line)", "", height = "50px")),
          bslib::accordion_panel("Training-set augmentation (marker effects)",
            shiny::div(class = "help-hint", "Extra genotyped + phenotyped individuals that sharpen the marker-effect fit but are NEVER candidate parents. Supply both files."),
            shiny::fileInput("f_train_geno", "Training genotype CSV", accept = ".csv"),
            shiny::fileInput("f_train_pheno", "Training phenotype CSV", accept = ".csv"),
            shiny::textInput("training_genotype_id_col", "Training genotype ID column (blank = auto)", ""),
            shiny::textInput("training_phenotype_id_col", "Training phenotype ID column (blank = auto)", "")),
          bslib::accordion_panel("Marker steering & lethal guarding",
            shiny::textAreaInput("marker_target_spec", "Marker targets ('marker,direction,target_freq,weight')", "", height = "70px"),
            shiny::numericInput("lambda_marker", "Marker steering weight", 0, min = 0, step = 0.1),
            shiny::textAreaInput("lethal_spec", "Lethal recessives ('marker,risk_allele')", "", height = "60px"),
            shiny::checkboxInput("drop_lethal_carrier_crosses", "Exclude carrier x carrier matings", TRUE)),
          bslib::accordion_panel("Cost & logistics",
            shiny::numericInput("budget", "Budget cap (blank = none)", NA, min = 0),
            shiny::numericInput("lambda_cost", "Cost emphasis", 0, min = 0, step = 0.1),
            shiny::numericInput("lambda_logistic", "Logistic emphasis", 0, min = 0, step = 0.1),
            shiny::div(class = "help-hint", "Budget and cost/logistic emphasis need a per-cross cost table to act on. Upload one (columns: parent1, parent2, then any cost/distance columns) and pick which column is cost / logistic."),
            shiny::fileInput("f_cost", "Cost table CSV (optional)", accept = ".csv"),
            shiny::uiOutput("cost_col_ui")),
          bslib::accordion_panel("Posterior prediction",
            shiny::checkboxInput("run_posterior_prediction", "Run posterior prediction", FALSE),
            shiny::conditionalPanel("input.run_posterior_prediction == true",
              shiny::selectInput("posterior_method", "Method", c("mcmc","closed_form")),
              shiny::numericInput("nIter", "Iterations", 5000, min = 1),
              shiny::numericInput("burnIn", "Burn-in", 500, min = 0),
              shiny::checkboxInput("use_parallel", "Use parallel", FALSE),
              shiny::conditionalPanel("input.use_parallel == true",
                shiny::numericInput("parallel_cores", "Parallel cores (blank = auto)", NA, min = 1, step = 1)))))),

      bslib::nav_panel("Output",
        ngcd_section("Priority ranking & outputs"),
        ngcd_guide(8, 10, "Output", shiny::tagList(
          shiny::tags$p("Decide how the selected crosses are tiered and what files to produce."),
          shiny::tags$ul(
            shiny::tags$li(shiny::tags$b("Priority tiers"), " group the plan into bands (highly priority -> low). The defaults split 100 crosses into 10/25/35/30."),
            shiny::tags$li("Tick ", shiny::tags$b("Write workbook"), " for an Excel crossing plan (needs openxlsx) and ", shiny::tags$b("Write figures"), " for the priority chart (needs ggplot2). Both are checked on Setup's requirements panel."),
            shiny::tags$li("Keep the ", shiny::tags$b("Random seed"), " fixed to make runs reproducible.")),
          shiny::tags$p(class = "help-hint", "Leave workbook/figures off for a quick look; turn them on for the final plan.")),
          next_hint = "Run - start the analysis."),
        bslib::layout_columns(col_widths = c(6,6),
          bslib::card(bslib::card_header("Priority ranking"),
            shiny::textInput("priority_breaks", "Tier breaks (comma-separated)", "0.10, 0.35, 0.70, 1.00"),
            shiny::textInput("priority_labels", "Tier labels (comma-separated)", "highly_priority, priority, medium_priority, low_priority"),
            shiny::numericInput("priority_score_weight", "Score weight", 1, step = 0.05),
            shiny::numericInput("priority_kinship_weight", "Kinship weight", 0.15, step = 0.05),
            shiny::numericInput("priority_threshold_weight", "Threshold weight", 1, step = 0.05)),
          bslib::card(bslib::card_header("Outputs & reproducibility"),
            shiny::checkboxInput("write_outputs", "Write workbook (.xlsx; needs openxlsx)", FALSE),
            shiny::checkboxInput("write_figures", "Write figures (.png; needs ggplot2)", FALSE),
            shiny::textInput("output_file", "Workbook file name", "crossing_plan.xlsx"),
            shiny::numericInput("seed", "Random seed", 20260706, step = 1)))),

      bslib::nav_panel("Run",
        ngcd_section("Run the analysis", "Assembles your configuration (with any table edits) and calls the backend."),
        ngcd_guide(9, 10, "Run", shiny::tagList(
          shiny::tags$p("Launch the analysis and watch for problems."),
          shiny::tags$ul(
            shiny::tags$li("The box under the button is the ", shiny::tags$b("readiness check"), " - it turns from warnings to \"Ready to run\" when data and backend are set."),
            shiny::tags$li("Click ", shiny::tags$b("Run cross prediction"), ". When it finishes, the app jumps to Results automatically."),
            shiny::tags$li("If it fails, a ", shiny::tags$b("debug panel"), " appears with the error, a likely-fix hint, and copyable details - fix the flagged item and run again."),
            if (isTRUE(dev)) shiny::tags$li(shiny::tags$b("Save / load settings"), ": save all your choices as a named profile, then reload it next session and run again without re-entering everything.")),
          shiny::tags$p(class = "help-hint", "Each run is saved under runs/ with its exact config, so results are reproducible.")),
          next_hint = "Results - read the recommended crosses."),
        if (isTRUE(dev)) bslib::card(bslib::card_header("Save / load settings"),
          shiny::div(class = "help-hint",
            "Save all your current settings as a named profile to reload and re-run later. (Data files and column edits are not part of a profile - reselect your data, then load a profile.)"),
          bslib::layout_columns(col_widths = c(5, 3, 4),
            shiny::textInput("preset_name", "Profile name", "my-settings"),
            shiny::div(shiny::tags$label(class = "form-label", " "),
              shiny::div(shiny::actionButton("save_preset", "Save profile", class = "btn-ndsu"))),
            shiny::div(shiny::tags$label(class = "form-label", " "),
              shiny::div(shiny::downloadButton("download_settings", "Download .json", class = "btn-outline-secondary")))),
          bslib::layout_columns(col_widths = c(5, 3, 4),
            shiny::uiOutput("preset_pick_ui"),
            shiny::div(shiny::tags$label(class = "form-label", " "),
              shiny::div(shiny::actionButton("load_preset", "Load profile", class = "btn-ndsu"))),
            shiny::fileInput("upload_settings", "...or load a .json file", accept = ".json"))),
        bslib::card(
          shiny::actionButton("run", "Run cross prediction", class = "btn-run", width = "260px"),
          shiny::uiOutput("run_gate"),
          shiny::uiOutput("error_panel"),
          shiny::tags$hr(), shiny::tags$b("Runner log"),
          shiny::verbatimTextOutput("run_log"))),

      bslib::nav_panel("Results",
        ngcd_section("Results", "Recommended crosses and the evidence behind them."),
        ngcd_guide(10, 10, "Results", shiny::tagList(
          shiny::tags$p("Read your crossing plan and the evidence behind it."),
          shiny::tags$ul(
            shiny::tags$li("The ", shiny::tags$b("KPI row"), " summarizes the plan: crosses, mean gain, group coancestry, unique parents, progeny inbreeding."),
            shiny::tags$li(shiny::tags$b("Selected crosses"), " is your ranked plan by priority tier; ", shiny::tags$b("Candidate scores"), " is every pair it chose from."),
            shiny::tags$li(shiny::tags$b("Gain-diversity frontier"), " shows the trade-off with your plan marked - move the Allocation dial and re-run to shift it."),
            shiny::tags$li(shiny::tags$b("QC audit / Input matching / Marker effects"), " show the provenance; ", shiny::tags$b("Downloads"), " has the workbook and figures if you enabled them.")),
          shiny::tags$p(class = "help-hint", "Change a setting on any tab and click Run again to compare.")),
          next_hint = "adjust settings and re-run to explore alternatives."),
        shiny::uiOutput("results_banner"),
        shiny::uiOutput("results_kpis"),
        bslib::navset_tab(
          bslib::nav_panel("Report", shiny::uiOutput("res_report")),
          bslib::nav_panel("Selected crosses", DT::DTOutput("res_selected")),
          bslib::nav_panel("Candidate scores", DT::DTOutput("res_candidate")),
          bslib::nav_panel("Parent use", DT::DTOutput("res_parentuse")),
          bslib::nav_panel("Robust plan", shiny::uiOutput("res_robust_ui")),
          bslib::nav_panel("Polyploid plan", shiny::uiOutput("res_poly_ui")),
          bslib::nav_panel("Gain-diversity frontier", shiny::uiOutput("res_frontier_ui")),
          bslib::nav_panel("QC audit", shiny::uiOutput("res_qc")),
          bslib::nav_panel("Input matching", shiny::verbatimTextOutput("res_match")),
          bslib::nav_panel("Marker effects", DT::DTOutput("res_effects")),
          bslib::nav_panel("Method & settings", shiny::verbatimTextOutput("res_settings")),
          bslib::nav_panel("Downloads", shiny::uiOutput("res_downloads")))),

      bslib::nav_spacer(),
      bslib::nav_item(shiny::tags$span(class = "help-hint", "nextgenCrossWorkbench"))),

    shiny::div(class = "ndsu-footer",
      "NextGenCrossDesign. ",
      shiny::tags$b("Backend (nextgenCrossDesign): Dr. Sikiru Atanda."), " ",
      shiny::tags$b("Front-end (workbench): Mario Morales."))
  )
}

# ---- server ---------------------------------------------------------------
workbench_server <- function(cfg) {
  function(input, output, session) {
    rv <- shiny::reactiveValues(backend = NULL, result = NULL, last = NULL,
                                run_dir = NULL, runlog = NULL, error = NULL,
                                data = list(genotype = NULL, phenotype = NULL, map = NULL, direction = NULL),
                                edited = FALSE)

    refresh_backend <- function() rv$backend <- ngcd_check_backend(cfg)
    refresh_backend()
    shiny::observeEvent(input$recheck, refresh_backend())

    output$topbar_status <- shiny::renderText({
      b <- rv$backend
      if (is.null(b)) "checking..."
      else if (isTRUE(b$backend_installed) && isTRUE(b$rscript_ok))
        paste0("R ", b$r_version, " - backend ", b$backend_version)
      else "backend not ready"
    })
    output$setup_status <- shiny::renderUI({
      b <- rv$backend; shiny::req(b)
      chip <- function(ok, yes, no, k = "warn") if (isTRUE(ok)) ngcd_badge(yes, "ok") else ngcd_badge(no, k)
      shiny::tagList(
        shiny::tags$p(chip(b$rscript_ok, paste0("Rscript OK (", b$rscript_resolved %||% "?", ")"), "Rscript not found", "error")),
        shiny::tags$p(chip(b$runner_exists, "Runner script found", "Runner script missing", "error")),
        shiny::tags$p(chip(b$backend_installed, paste0("nextgenCrossDesign ", b$backend_version %||% ""), "Backend not installed", "error")),
        shiny::tags$p(chip(b$version_ok, "Version OK", paste0("Needs >= ", cfg$required_backend_version), "warn")),
        if (length(b$messages)) ngcd_callout(kind = "warn", shiny::tags$ul(lapply(b$messages, shiny::tags$li))))
    })
    output$setup_detail <- shiny::renderPrint({
      b <- rv$backend; shiny::req(b)
      utils::str(b[c("rscript_path","rscript_resolved","r_version","backend_installed",
                     "backend_version","version_ok","package_library")])
    })

    output$ril_note <- shiny::renderUI({
      b <- rv$backend
      if (identical(input$ril_mode, "finite") && !isTRUE(b$supports_nselfing))
        ngcd_callout(kind = "warn",
          "The installed backend supports only infinite-generation RILs. Finite RILs with a set number of selfing generations need a newer backend - this run will use ", shiny::tags$b("infinite"), " instead.")
    })
    output$mip_note <- shiny::renderUI({
      b <- rv$backend
      if (!is.null(b) && input$optimizer %in% c("mip_linear","mip_contribution") && isFALSE(b$optional[["lpSolve"]]))
        ngcd_callout(kind = "warn",
          "lpSolve is not installed for the backend, so this MIP optimizer will fall back to greedy/repair. Install lpSolve, or pick 'evolution'/'greedy_local'.")
    })
    output$alphamate_note <- shiny::renderUI({
      b <- rv$backend
      if (identical(input$allocation_method, "alphamate_executable")) {
        path <- input$alphamate_executable %||% b$alphamate_executable %||% ""
        exists <- nzchar(path) && (file.exists(path) || nzchar(Sys.which(path)))
        if (!nzchar(path))
          ngcd_callout(kind = "warn", "No AlphaMate executable set. Enter its path below, or set ",
            shiny::tags$code("alphamate_executable"), " in config.yml. Without it, use 'alphamate_style' (native, no binary).")
        else if (!exists)
          ngcd_callout(kind = "warn", paste0("AlphaMate executable not found at: ", path,
            ". The run will fall back to the native alphamate_style allocator."))
        else ngcd_callout(kind = "info", "External AlphaMate executable found.")
      }
    })

    output$requirements <- shiny::renderUI({
      b <- rv$backend; shiny::req(b)
      opt <- b$optional
      row <- function(pkg, enables) {
        have <- isTRUE(opt[[pkg]])
        shiny::tags$tr(
          shiny::tags$td(if (have) ngcd_badge(pkg, "ok") else ngcd_badge(pkg, "warn")),
          shiny::tags$td(shiny::span(class = "help-hint", enables)))
      }
      am_have <- isTRUE(b$alphamate_ok)
      am_badge <- if (is.na(b$alphamate_ok)) ngcd_badge("AlphaMate exe", "info")
                  else if (am_have) ngcd_badge("AlphaMate exe", "ok") else ngcd_badge("AlphaMate exe", "warn")
      shiny::tagList(
        shiny::tags$table(class = "table table-sm",
          shiny::tags$tbody(
            row("lpSolve", "MIP optimizers (mip_linear, mip_contribution); absent -> greedy/repair"),
            row("openxlsx", "Excel crossing-plan workbook export (Output screen)"),
            row("ggplot2", "Figure export (Output screen)"),
            row("PopVar", "Exact PopVar external baseline (native var_complex is the default)"),
            row("SimpleMating", "SimpleMating external baseline (native proxy otherwise)"),
            row("genomicMateSelectR", "genomicMateSelectR external comparison"),
            row("AlphaSimR", "AlphaSimR simulation / benchmark workflows"),
            row("sommer", "sommer mixed-model variance components (optional)"),
            shiny::tags$tr(
              shiny::tags$td(am_badge),
              shiny::tags$td(shiny::span(class = "help-hint",
                "External AlphaMate binary for allocation_method = alphamate_executable; set ",
                shiny::tags$code("alphamate_executable"), " in config.yml. Native ",
                shiny::tags$code("alphamate_style"), " needs no binary."))))),
        if (isFALSE(opt[["lpSolve"]]))
          ngcd_callout(kind = "warn", "lpSolve is not installed, so the MIP optimizers will fall back to greedy/repair. ",
            shiny::tags$code("install.packages('lpSolve')"), " in your backend R to enable them."))
    })

    # ---- load data into editable store ----
    is_poly <- shiny::reactive(identical(input$workflow, "polyploid"))
    src_files <- shiny::reactive({
      if (identical(input$data_source, "demo"))
        (if (is_poly()) ngcd_poly_demo_files(cfg) else ngcd_demo_files(cfg))
      else list(
        genotype  = if (!is.null(input$f_geno))  input$f_geno$datapath  else NULL,
        phenotype = if (!is.null(input$f_pheno)) input$f_pheno$datapath else NULL,
        map       = if (!is_poly() && !is.null(input$f_map)) input$f_map$datapath else NULL,
        direction = if (!is_poly() && !is.null(input$f_dir)) input$f_dir$datapath else NULL)
    })

    load_data <- function() {
      f <- src_files()
      rd <- function(p) if (!is.null(p) && file.exists(p)) ngcd_read_full(p) else NULL
      rv$data <- list(genotype = rd(f$genotype), phenotype = rd(f$phenotype),
                      map = rd(f$map), direction = rd(f$direction))
      rv$edited <- FALSE
    }
    shiny::observeEvent(src_files(), load_data(), ignoreNULL = FALSE)
    shiny::observeEvent(input$reset_data, load_data())

    # Visible per-file load feedback (upload mode) so a bad file is never a
    # silent empty table - shows rows x cols on success, or why it failed.
    output$load_status <- shiny::renderUI({
      if (!identical(input$data_source, "upload")) return(NULL)
      items <- list(
        list(nm = "Genotype",        up = input$f_geno,  df = rv$data$genotype),
        list(nm = "Phenotype",       up = input$f_pheno, df = rv$data$phenotype),
        list(nm = "Marker map",      up = input$f_map,   df = rv$data$map),
        list(nm = "Trait direction", up = input$f_dir,   df = rv$data$direction))
      rows <- lapply(items, function(it) {
        if (is.null(it$up)) return(NULL)  # not uploaded yet
        fname <- it$up$name %||% ""
        if (is.null(it$df) || !ncol(it$df)) {
          ngcd_badge(paste0(it$nm, ": could not read ", fname), "error")
        } else if (ncol(it$df) < 2) {
          shiny::tagList(ngcd_badge(paste0(it$nm, ": 1 column only"), "warn"),
            shiny::span(class = "help-hint", " - check the file's delimiter/format."))
        } else {
          ngcd_badge(sprintf("%s: %d rows x %d cols", it$nm, nrow(it$df), ncol(it$df)), "ok")
        }
      })
      rows <- Filter(Negate(is.null), rows)
      if (!length(rows)) return(shiny::div(class = "help-hint", "Choose your four files above."))
      shiny::div(style = "display:flex; flex-direction:column; gap:4px; margin-top:8px", rows)
    })

    output$edit_flag <- shiny::renderUI({
      if (isTRUE(rv$edited)) ngcd_callout(kind = "warn",
        shiny::tags$b("Edited. "), "The modified table(s) will be used on the next run.")
    })

    # Polyploid mode needs only dosage + phenotype; standard needs all four.
    data_ready <- shiny::reactive({
      ok1 <- function(d) !is.null(d) && nrow(d) > 0
      if (is_poly()) ok1(rv$data$genotype) && ok1(rv$data$phenotype)
      else all(vapply(rv$data, ok1, logical(1)))
    })
    # phenotype trait-column picker for polyploid mode (single trait)
    output$poly_trait_ui <- shiny::renderUI({
      ph <- rv$data$phenotype
      choices <- if (is.data.frame(ph)) setdiff(names(ph),
        input$phenotype_id_col %||% ngcd_guess_col(names(ph), c("NAME","parent","id","line"))) else character(0)
      shiny::selectInput("poly_trait_col", "Phenotype trait (single)", choices = choices,
        selected = if (length(choices)) choices[1] else NULL)
    })
    cols <- shiny::reactive(list(
      geno  = if (!is.null(rv$data$genotype))  names(rv$data$genotype)  else character(0),
      pheno = if (!is.null(rv$data$phenotype)) names(rv$data$phenotype) else character(0),
      map   = if (!is.null(rv$data$map))       names(rv$data$map)       else character(0),
      dir   = if (!is.null(rv$data$direction)) names(rv$data$direction) else character(0)))

    # Alignment diagnostics: genotype markers vs map, and genotype vs phenotype IDs.
    align_diag <- shiny::reactive({
      g <- rv$data$genotype; m <- rv$data$map; ph <- rv$data$phenotype
      if (is.null(g) || is.null(m) || is.null(ph)) return(NULL)
      gid <- input$genotype_id_col %||% ngcd_guess_col(names(g), c("NAME","parent","id","line"))
      if (is.null(gid) || !gid %in% names(g)) gid <- names(g)[1]
      pid <- input$phenotype_id_col %||% ngcd_guess_col(names(ph), c("NAME","parent","id","line"))
      if (is.null(pid) || !pid %in% names(ph)) pid <- names(ph)[1]
      mcol <- input$map_marker_col %||% ngcd_guess_col(names(m), c("SNP_code","marker","snp","id"))
      if (is.null(mcol) || !mcol %in% names(m)) mcol <- names(m)[1]
      gmark <- setdiff(names(g), gid)
      mmark <- trimws(as.character(m[[mcol]]))
      gids  <- trimws(as.character(g[[gid]]))
      pids  <- trimws(as.character(ph[[pid]]))
      het <- ngcd_het_violators(g, gid, gmark)
      list(gid = gid, pid = pid, mcol = mcol,
           gmark = gmark, mmark = mmark, gids = gids, pids = pids,
           miss_map  = setdiff(gmark, mmark),   # genotype markers absent from map (BLOCKER)
           extra_map = setdiff(mmark, gmark),   # map markers not genotyped (BLOCKER)
           n_shared_markers = length(intersect(gmark, mmark)),
           miss_pheno = setdiff(gids, pids),    # genotyped parents with no phenotype
           miss_geno  = setdiff(pids, gids),
           n_shared_ids = length(intersect(gids, pids)),
           het_ids = het$ids, het_n = het$n, het_max = het$max_frac)
    })

    output$data_checks <- shiny::renderUI({
      d <- align_diag()
      if (is.null(d)) return(ngcd_callout("Load all four tables to run alignment checks."))
      ex <- function(v, n = 6) if (!length(v)) "" else paste0(" e.g. ", paste(utils::head(v, n), collapse = ", "),
                                                              if (length(v) > n) ", ..." else "")
      # The backend requires the genotype and map marker sets to be IDENTICAL;
      # a mismatch in EITHER direction is a blocker.
      mk_ok <- length(d$miss_map) == 0 && length(d$extra_map) == 0
      id_ok <- length(d$miss_pheno) == 0
      shiny::tagList(
        shiny::tags$p(
          if (mk_ok) ngcd_badge(paste0("Markers aligned (", d$n_shared_markers, " shared)"), "ok")
          else ngcd_badge(paste0(length(d$miss_map) + length(d$extra_map), " marker mismatches"), "error"),
          " ",
          if (id_ok) ngcd_badge(paste0("Parents aligned (", d$n_shared_ids, " shared)"), "ok")
          else ngcd_badge(paste0(length(d$miss_pheno), " genotyped parents lack phenotype"), "warn")),
        if (!mk_ok) ngcd_callout(kind = "error",
          shiny::tags$b("Blocker: "), "genotype and map marker sets must match exactly.",
          if (length(d$miss_map)) shiny::tags$div(length(d$miss_map),
            " genotype markers are NOT in the map:", shiny::tags$code(ex(d$miss_map))),
          if (length(d$extra_map)) shiny::tags$div(length(d$extra_map),
            " map markers are NOT in the genotype:", shiny::tags$code(ex(d$extra_map))),
          shiny::tags$div(class = "help-hint",
            "Reconcile the marker names in the two files, or tick ",
            shiny::tags$b("'Use only markers present in BOTH'"),
            " below to keep just the ", d$n_shared_markers, " shared markers for this run.")),
        if (!id_ok) ngcd_callout(kind = "warn",
          length(d$miss_pheno), " genotyped parents have no phenotype", ex(d$miss_pheno),
          ". Tick 'Use only parents present in BOTH' to drop them."),
        if (isTRUE(input$assume_inbred) && d$het_n > 0) ngcd_callout(kind = "warn",
          shiny::tags$b(paste0(d$het_n, " parents are not fully inbred")),
          " (>2% heterozygous markers", ex(d$het_ids), ").",
          shiny::tags$div(class = "help-hint",
            "The DH/RIL model assumes inbred parents. Either tick ",
            shiny::tags$b("'Exclude non-inbred parents'"), " below to drop them, or uncheck ",
            shiny::tags$b("'Assume inbred parents'"), " on the Scoring screen to keep them (results for those parents will be approximate).")))
    })

    # editable tables
    output$edit_geno <- DT::renderDT(ngcd_dt_editable(rv$data$genotype, 8), server = TRUE)
    output$edit_pheno <- DT::renderDT(ngcd_dt_editable(rv$data$phenotype, 8), server = TRUE)
    output$edit_map <- DT::renderDT(ngcd_dt_editable(rv$data$map, 8), server = TRUE)
    output$edit_dir <- DT::renderDT(ngcd_dt_editable(rv$data$direction, 8), server = TRUE)

    apply_edit <- function(key, info) {
      df <- rv$data[[key]]; if (is.null(df)) return()
      rv$data[[key]] <- DT::editData(df, info, rownames = FALSE)
      rv$edited <- TRUE
    }
    shiny::observeEvent(input$edit_geno_cell_edit, apply_edit("genotype", input$edit_geno_cell_edit))
    shiny::observeEvent(input$edit_pheno_cell_edit, apply_edit("phenotype", input$edit_pheno_cell_edit))
    shiny::observeEvent(input$edit_map_cell_edit, apply_edit("map", input$edit_map_cell_edit))
    shiny::observeEvent(input$edit_dir_cell_edit, apply_edit("direction", input$edit_dir_cell_edit))

    # Candidate ID columns. A genotype table can carry thousands of marker
    # columns; listing them all makes the ID <select> huge and slow (Shiny even
    # warns to use server-side selectize). An ID is a *label*, never a numeric
    # marker dosage, so for wide tables we offer only the non-numeric columns
    # (plus the first column and the guessed one). Narrow tables list all.
    id_col_choices <- function(df, guess) {
      if (is.null(df) || !ncol(df)) return(character(0))
      cn <- names(df)
      if (length(cn) <= 40) return(cn)
      cand <- cn[!vapply(df, is.numeric, logical(1))]           # label columns
      cand <- unique(c(cn[1], ngcd_guess_col(cn, guess), cand))
      cand <- cand[!is.na(cand) & nzchar(cand)]
      if (length(cand) > 40) cand <- unique(c(cn[1], ngcd_guess_col(cn, guess)))
      cand[!is.na(cand) & nzchar(cand)]
    }
    # column mapping
    output$colmap_ui <- shiny::renderUI({
      c <- cols()
      if (!length(c$geno)) return(ngcd_callout("Load data to map columns."))
      sel <- function(id, label, choices, guess)
        shiny::selectInput(id, label, choices = c("(auto)" = "", choices),
                           selected = ngcd_guess_col(choices, guess) %||% "")
      id_sel <- function(id, label, df, guess) {
        ch <- id_col_choices(df, guess)
        shiny::selectInput(id, label, choices = c("(auto)" = "", ch),
                           selected = ngcd_guess_col(ch, guess) %||% "")
      }
      shiny::tagList(
        shiny::tags$b("Identity columns"),
        bslib::layout_columns(col_widths = c(6,6),
          id_sel("genotype_id_col", "Genotype ID", rv$data$genotype, c("NAME","parent","id","line")),
          id_sel("phenotype_id_col", "Phenotype ID", rv$data$phenotype, c("NAME","parent","id","line"))),
        if (is_poly()) shiny::div(class = "help-hint",
          "Polyploid mode needs only the identity columns and a single phenotype trait (chosen on the Data-source panel). Marker-map and trait-direction columns are not used.")
        else shiny::tagList(
          shiny::tags$b("Marker map columns"),
          bslib::layout_columns(col_widths = c(6,6),
            sel("map_marker_col", "Marker", c$map, c("SNP_code","marker","snp","id")),
            sel("map_chr_col", "Chromosome", c$map, c("Chromosome","chr","chrom"))),
          shiny::selectInput("pos_unit", "Position unit",
            c("auto (detect)" = "auto", "bp" = "bp", "cM" = "cM", "Morgans (M)" = "M"), selected = "auto"),
          shiny::div(class = "help-hint",
            "auto detects bp vs cM from the values; Morgans are converted to cM. The backend works in bp or cM."),
          shiny::conditionalPanel("input.pos_unit == 'bp'",
            shiny::numericInput("bp_per_cm", "bp per cM (to derive cM)", 1e6, min = 1)),
          shiny::conditionalPanel("input.pos_unit == 'bp' || input.pos_unit == 'auto'",
            sel("map_pos_bp_col", "Position column (bp / auto)", c$map, c("Position_BP","pos","position","bp"))),
          shiny::conditionalPanel("input.pos_unit == 'cM' || input.pos_unit == 'M'",
            sel("map_pos_cm_col", "Position column (cM / Morgans)", c$map, c("Position_cM","cM","pos_cm","cm","Position_M"))),
          shiny::tags$b("Trait direction columns"),
          bslib::layout_columns(col_widths = c(4,4,4),
            sel("direction_trait_col", "Trait label", c$dir, c("Trait","trait")),
            sel("direction_column_col", "Phenotype column", c$dir, c("Trait","column","trait")),
            sel("direction_direction_col", "Direction", c$dir, c("Selection_direction","direction")))))
    })
    # Trait choices come from the DIRECTION file (the backend filters
    # traits_to_use against the direction file's trait label / column), NOT the
    # phenotype columns - otherwise a subset can match nothing.
    full_trait_set <- shiny::reactive({
      d <- rv$data$direction
      if (is.null(d) || !nrow(d)) {
        c <- cols(); idg <- ngcd_guess_col(c$pheno, c("NAME","parent","id","line"))
        return(setdiff(c$pheno, idg))
      }
      tcol <- input$direction_trait_col %||% ngcd_guess_col(names(d), c("Trait","trait"))
      if (is.null(tcol) || !tcol %in% names(d)) tcol <- names(d)[1]
      unique(trimws(as.character(d[[tcol]])))
    })
    output$traits_to_use_ui <- shiny::renderUI({
      traits <- full_trait_set()
      shiny::tagList(
        shiny::checkboxGroupInput("traits_to_use", "Traits to use", choices = traits, selected = traits),
        shiny::div(class = "help-hint", "Leave all checked to use every trait. Uncheck only to run a subset."))
    })
    output$index_col_ui <- shiny::renderUI({
      c <- cols()
      idg <- input$phenotype_id_col %||% ngcd_guess_col(c$pheno, c("NAME","parent","id","line"))
      choices <- setdiff(c$pheno, idg)   # never offer the ID column as the index
      if (!length(choices)) return(ngcd_callout(kind = "warn", "No non-ID phenotype columns found to use as an index."))
      shiny::tagList(
        shiny::selectInput("index_col", "Index column (the pre-built selection index)", choices),
        shiny::div(class = "help-hint",
          "Pick the phenotype column that holds your trusted selection index. The ID column is not listed."))
    })

    # optional per-cross cost table (Advanced -> Cost & logistics)
    cost_data <- shiny::reactive({
      if (is.null(input$f_cost)) return(NULL)
      ngcd_read_full(input$f_cost$datapath)
    })
    output$cost_col_ui <- shiny::renderUI({
      cd <- cost_data(); if (is.null(cd)) return(NULL)
      value_cols <- setdiff(names(cd), c("parent1", "parent2"))
      shiny::tagList(
        shiny::selectInput("cost_col", "Cost column", c("(none)" = "", value_cols)),
        shiny::selectInput("logistic_col", "Logistic column", c("(none)" = "", value_cols)))
    })

    output$sb_data <- shiny::renderText(if (data_ready()) if (isTRUE(rv$edited)) "ready (edited)" else "ready" else "incomplete")
    output$sb_backend <- shiny::renderText({ b <- rv$backend; if (!is.null(b) && isTRUE(b$backend_installed)) "connected" else "not ready" })
    output$sb_obj <- shiny::renderText(input$multi_trait_method %||% "auto")
    output$sb_run <- shiny::renderText(rv$last %||% "none yet")

    # parse helpers
    parse_pairs <- function(txt, sep = ",") {
      txt <- trimws(txt %||% ""); if (!nzchar(txt)) return(NULL)
      rows <- strsplit(txt, "\n")[[1]]; rows <- rows[nzchar(trimws(rows))]
      lapply(rows, function(r) trimws(strsplit(r, sep)[[1]]))
    }
    parse_named_num <- function(txt) {
      p <- parse_pairs(txt, sep = ":"); if (is.null(p)) return(NULL)
      stats::setNames(as.list(suppressWarnings(as.numeric(vapply(p, `[`, "", 2)))), vapply(p, `[`, "", 1))
    }
    num_or_null <- function(x) if (is.null(x) || is.na(x)) NULL else as.numeric(x)
    csv_vec <- function(txt, numeric = FALSE) {
      v <- trimws(strsplit(txt %||% "", ",")[[1]]); v <- v[nzchar(v)]
      if (numeric) as.numeric(v) else v
    }

    # Resolve the UI position unit (auto/bp/cM/M) to what the backend accepts
    # (bp or cM). Morgans -> cM via divisor 0.01; auto -> detect from values.
    resolve_map <- function() {
      pu <- input$pos_unit %||% "auto"
      mapdf   <- rv$data$map
      mapcols <- if (is.data.frame(mapdf)) names(mapdf) else character(0)
      # Resolve a column choice safely: use the input if it names a real column,
      # otherwise guess from the map's columns. Never index with NULL/"" (that
      # throws `.subset2: attempt to select less than one element`).
      # Honor any non-empty column selection (the bp/cM/M branches only pass the
      # name to the backend, they do not index the map here). Fall back to a
      # guess from the map's columns when nothing is selected yet.
      pick <- function(col, guesses) {
        if (!is.null(col) && length(col) == 1L && nzchar(col)) return(col)
        g <- ngcd_guess_col(mapcols, guesses)
        if (!is.null(g) && length(g) == 1L) g else NULL
      }
      bpcol <- pick(input$map_pos_bp_col, c("Position_BP","pos","position","bp"))
      cmcol <- pick(input$map_pos_cm_col, c("Position_cM","cM","pos_cm","cm","Position_M"))
      colvals <- function(col) if (!is.null(col) && col %in% mapcols)
        suppressWarnings(as.numeric(mapdf[[col]])) else numeric(0)

      if (pu == "auto") {
        vals <- colvals(bpcol)
        med  <- if (length(vals)) suppressWarnings(stats::median(vals[is.finite(vals)], na.rm = TRUE)) else NA_real_
        if (is.finite(med) && med > 10000)
          return(list(map_position_unit = "bp", map_pos_bp_col = bpcol, bp_per_cm = num_or_null(input$bp_per_cm)))
        return(list(map_position_unit = "cM", map_pos_cm_col = bpcol %||% cmcol, map_pos_cm_divisor = 1))
      }
      if (pu == "bp") return(list(map_position_unit = "bp", map_pos_bp_col = bpcol, bp_per_cm = num_or_null(input$bp_per_cm)))
      if (pu == "M")  return(list(map_position_unit = "cM", map_pos_cm_col = cmcol, map_pos_cm_divisor = 0.01))
      list(map_position_unit = "cM", map_pos_cm_col = cmcol, map_pos_cm_divisor = 1)  # cM
    }

    build_params <- shiny::reactive({
      mapcfg <- resolve_map()
      p <- list(schema = "ng_run_config.v1",
        crop = input$crop,  # metadata (ignored by the runner formals)
        genotype_id_col = input$genotype_id_col, phenotype_id_col = input$phenotype_id_col,
        map_marker_col = input$map_marker_col, map_chr_col = input$map_chr_col,
        map_position_unit = mapcfg$map_position_unit,
        map_pos_bp_col = mapcfg$map_pos_bp_col,
        bp_per_cm = mapcfg$bp_per_cm,
        map_pos_cm_col = mapcfg$map_pos_cm_col,
        map_pos_cm_divisor = mapcfg$map_pos_cm_divisor,
        marker_ploidy = as.integer(input$ploidy %||% "2"),
        ld_ploidy = as.integer(input$ploidy %||% "2"),
        direction_trait_col = input$direction_trait_col, direction_column_col = input$direction_column_col,
        direction_direction_col = input$direction_direction_col,
        prediction_mode = input$prediction_mode,
        traits_to_use = local({
          if (!identical(input$prediction_mode, "trait_by_trait")) return(NULL)
          sel <- input$traits_to_use; all_traits <- full_trait_set()
          # Omit when the user wants all traits (or hasn't subset) so the
          # backend uses its full trait set - avoids empty-intersection errors.
          if (is.null(sel) || !length(sel) || setequal(sel, all_traits)) NULL else sel
        }),
        index_col = if (identical(input$prediction_mode,"index_as_trait")) input$index_col else NULL,
        index_direction = input$index_direction,
        # In index_as_trait mode the pre-built index IS the objective, so the
        # multi-trait method / weights do not apply (and sending them errors).
        multi_trait_method = if (identical(input$prediction_mode,"index_as_trait")) NULL else input$multi_trait_method,
        trait_weights = if (identical(input$prediction_mode,"trait_by_trait") &&
                            identical(input$multi_trait_method,"weighted")) parse_named_num(input$trait_weights) else NULL,
        threshold_policy = input$threshold_policy, threshold_penalty_weight = input$threshold_penalty_weight,
        threshold_penalty_autoscale = input$threshold_penalty_autoscale,
        trait_value_metric = input$trait_value_metric, uc_variance_source = input$uc_variance_source,
        method_varPMV = input$method_varPMV, selection_prop = input$selection_prop,
        progeny = input$progeny, recombination_model = input$recombination_model,
        grm_method = input$grm_method, assume_inbred = input$assume_inbred,
        min_effect_reliability = input$min_effect_reliability,
        duplicate_action = input$duplicate_action, duplicate_threshold = input$duplicate_threshold,
        duplicate_maf_min = input$duplicate_maf_min, duplicate_max_missing_prop = input$duplicate_max_missing_prop,
        duplicate_min_compared_markers = input$duplicate_min_compared_markers,
        ld_pruning = input$ld_pruning, ld_window = input$ld_window, ld_r2_threshold = input$ld_r2_threshold,
        ld_maf_threshold = input$ld_maf_threshold, ld_backend = input$ld_backend,
        n_crosses = if (identical(input$cross_number_mode, "auto"))
          as.integer(input$cross_sweep_k_max %||% 30) else input$n_crosses,
        max_uses_per_parent = input$max_uses_per_parent,
        min_unique_parents = num_or_null(input$min_unique_parents), max_pair_kinship = num_or_null(input$max_pair_kinship),
        optimizer = input$optimizer, allocation_method = input$allocation_method, use_ocs = input$use_ocs,
        lambda_group = input$lambda_group, lambda_mating = input$lambda_mating,
        lambda_parent_use = input$lambda_parent_use, lambda_parent_use_mode = input$lambda_parent_use_mode,
        local_iter = input$local_iter, ocs_iter = input$ocs_iter,
        lambda_progeny_inbreeding = input$lambda_progeny_inbreeding, min_crosses_per_parent = input$min_crosses_per_parent,
        lambda_marker = input$lambda_marker, drop_lethal_carrier_crosses = input$drop_lethal_carrier_crosses,
        budget = num_or_null(input$budget), lambda_cost = input$lambda_cost, lambda_logistic = input$lambda_logistic,
        run_posterior_prediction = input$run_posterior_prediction, posterior_method = input$posterior_method,
        nIter = input$nIter, burnIn = input$burnIn, use_parallel = input$use_parallel,
        parallel_cores = if (isTRUE(input$use_parallel)) num_or_null(input$parallel_cores) else NULL,
        training_genotype_file = if (!is.null(input$f_train_geno)) input$f_train_geno$datapath else NULL,
        training_phenotype_file = if (!is.null(input$f_train_pheno)) input$f_train_pheno$datapath else NULL,
        training_genotype_id_col = input$training_genotype_id_col,
        training_phenotype_id_col = input$training_phenotype_id_col,
        priority_breaks = csv_vec(input$priority_breaks, TRUE), priority_labels = csv_vec(input$priority_labels),
        priority_score_weight = input$priority_score_weight, priority_kinship_weight = input$priority_kinship_weight,
        priority_threshold_weight = input$priority_threshold_weight,
        write_outputs = input$write_outputs, write_figures = input$write_figures,
        output_file = input$output_file, seed = input$seed)

      # Robust posterior allocation (meta keys the runner reads). Requires
      # posterior prediction, so force it on when robust is requested.
      if (isTRUE(input$robust_allocation)) {
        p$robust_allocation <- TRUE
        p$robust_objective <- input$robust_objective %||% "posterior_quantile"
        p$robustness_quantile <- num_or_null(input$robustness_quantile) %||% 0.25
        if (identical(p$robust_objective, "posterior_topn_prob"))
          p$robust_top_n_target <- as.integer(input$robust_top_n_target %||% 10)
        p$run_posterior_prediction <- TRUE
      }

      # Cross-number mode: fixed (default) or auto diminishing-returns sweep.
      # These are meta keys the headless runner reads (not backend formals).
      p$cross_number_mode <- input$cross_number_mode %||% "fixed"
      if (identical(p$cross_number_mode, "auto")) {
        p$cross_sweep_k_min <- as.integer(input$cross_sweep_k_min %||% 3)
        p$cross_sweep_k_max <- as.integer(input$cross_sweep_k_max %||% 30)
        p$cross_sweep_k_step <- as.integer(input$cross_sweep_k_step %||% 1)
        p$cross_sweep_criterion <- input$cross_sweep_criterion %||% "elbow_relative"
        p$cross_sweep_relative_threshold <- num_or_null(input$cross_sweep_relative_threshold) %||% 0.05
        p$cross_sweep_ne_min <- num_or_null(input$cross_sweep_ne_min) %||% 30
        p$cross_sweep_coancestry_max <- num_or_null(input$cross_sweep_coancestry_max) %||% 0.05
      }

      # RIL mode: send finite + nselfing only if the backend supports it; else
      # fall back to infinite (v0.4.0 backend only supports infinite RILs).
      if (identical(input$progeny, "RIL")) {
        if (identical(input$ril_mode, "finite") && isTRUE(rv$backend$supports_nselfing)) {
          p$ril_mode <- "finite"; p$nselfing <- as.integer(input$nselfing %||% 8)
        } else {
          p$ril_mode <- "infinite"
        }
      }
      if (identical(input$diversity_mode, "strategy")) p$strategy <- input$strategy
      if (identical(input$diversity_mode, "emphasis")) p$diversity_emphasis <- input$diversity_emphasis
      if (identical(input$diversity_mode, "target")) p$target_coancestry <- input$target_coancestry
      if (identical(input$optimizer, "evolution")) {
        p$evol_solutions <- input$evol_solutions; p$evol_iterations <- input$evol_iterations
        p$evol_stop <- input$evol_stop; p$evol_seed <- num_or_null(input$evol_seed)
      }
      if (!identical(input$allocation_method, "ocs")) {
        p$alphamate_mode <- input$alphamate_mode; p$alphamate_target_degree <- input$alphamate_target_degree
        p$alphamate_max_contributions <- num_or_null(input$alphamate_max_contributions)
        p$alphamate_n_threads <- input$alphamate_n_threads
        if (identical(input$allocation_method, "alphamate_executable")) p$alphamate_executable <- input$alphamate_executable
      }
      cc <- parse_pairs(input$committed_crosses)
      if (!is.null(cc)) p$committed_crosses <- list(parent1 = vapply(cc, `[`, "", 1), parent2 = vapply(cc, `[`, "", 2))
      pg <- parse_pairs(input$parent_group)
      if (!is.null(pg)) p$parent_group <- stats::setNames(as.list(vapply(pg, `[`, "", 2)), vapply(pg, `[`, "", 1))
      gq <- parse_named_num(input$group_quota); if (!is.null(gq)) p$group_quota <- gq
      mt <- parse_pairs(input$marker_target_spec)
      if (!is.null(mt)) p$marker_target_spec <- lapply(mt, function(r)
        list(marker = r[1], direction = r[2] %||% "increase",
             target_freq = suppressWarnings(as.numeric(r[3])), weight = suppressWarnings(as.numeric(r[4]))))
      ls <- parse_pairs(input$lethal_spec)
      if (!is.null(ls)) p$lethal_spec <- lapply(ls, function(r) list(marker = r[1], risk_allele = r[2] %||% "alt"))

      # group permission matrix (from parent groups + disallowed pairs)
      dis <- parse_pairs(input$group_disallow)
      if (!is.null(p$parent_group) && !is.null(dis)) {
        groups <- unique(unlist(p$parent_group))
        perm <- stats::setNames(lapply(groups, function(g)
          stats::setNames(as.list(rep(TRUE, length(groups))), groups)), groups)
        for (d in dis) {
          a <- d[1]; b <- d[2]
          if (a %in% groups && b %in% groups) { perm[[a]][[b]] <- FALSE; perm[[b]][[a]] <- FALSE }
        }
        p$group_permission <- perm
      }

      # per-cross cost table + which columns are cost / logistic
      cd <- cost_data()
      if (!is.null(cd)) {
        p$cross_cost <- as.list(cd)
        if (nzchar(input$cost_col %||% "")) p$cost_col <- input$cost_col
        if (nzchar(input$logistic_col %||% "")) p$logistic_col <- input$logistic_col
      }

      # external AlphaMate executable advanced knobs
      if (identical(input$allocation_method, "alphamate_executable")) {
        p$alphamate_runtime_path <- input$alphamate_runtime_path
        p$alphamate_workdir <- input$alphamate_workdir
        p$alphamate_number_of_parents <- num_or_null(input$alphamate_number_of_parents)
        p$alphamate_lambda_group <- num_or_null(input$alphamate_lambda_group)
        p$alphamate_keep_files <- input$alphamate_keep_files
        lg <- csv_vec(input$alphamate_lambda_grid, TRUE); if (length(lg)) p$alphamate_lambda_grid <- lg
        p$alphamate_evol_solutions <- input$alphamate_evol_solutions
        p$alphamate_evol_iterations <- input$alphamate_evol_iterations
        p$alphamate_evol_stop <- input$alphamate_evol_stop
      }
      p
    })

    output$run_gate <- shiny::renderUI({
      msgs <- character(0)
      b <- rv$backend
      if (is_poly()) {
        if (!data_ready()) msgs <- c(msgs, "A dosage file and a phenotype file must be loaded.")
        if (is.null(b) || !isTRUE(b$backend_installed)) msgs <- c(msgs, "Backend is not ready (see Setup).")
        # dosages must fit within the selected ploidy
        g <- rv$data$genotype
        if (is.data.frame(g)) {
          gid <- input$genotype_id_col %||% ngcd_guess_col(names(g), c("NAME","parent","id","line"))
          mcols <- setdiff(names(g), if (!is.null(gid) && gid %in% names(g)) gid else names(g)[1])
          dmax <- suppressWarnings(max(as.matrix(sapply(g[mcols], function(x) suppressWarnings(as.numeric(x)))), na.rm = TRUE))
          pl <- as.integer(input$ploidy %||% "2")
          if (is.finite(dmax) && dmax > pl)
            msgs <- c(msgs, sprintf("Dosage values go up to %g but Ploidy is set to %d. Set Ploidy = %g (or higher) on the Data screen.", dmax, pl, dmax))
        }
        if (length(msgs)) return(ngcd_callout(kind = "warn", shiny::tags$ul(lapply(msgs, shiny::tags$li))))
        return(ngcd_callout("Ready to run the polyploid design. Click the button above."))
      }
      if (!data_ready()) msgs <- c(msgs, "All four data tables must be loaded.")
      if (is.null(b) || !isTRUE(b$backend_installed)) msgs <- c(msgs, "Backend is not ready (see Setup).")
      d <- align_diag()
      if (!is.null(d) && (length(d$miss_map) || length(d$extra_map)) && !isTRUE(input$restrict_shared_markers))
        msgs <- c(msgs, paste0("Genotype and map marker sets do not match (",
          length(d$miss_map), " only in genotype, ", length(d$extra_map),
          " only in map) - this will block the run. Fix the names, or tick 'Use only markers present in BOTH' on the Data screen."))
      if (!is.null(d) && isTRUE(input$assume_inbred) && d$het_n > 0 &&
          !isTRUE(input$drop_noninbred_parents))
        msgs <- c(msgs, paste0(d$het_n,
          " parents are not fully inbred - the DH/RIL model will block the run. Tick 'Exclude non-inbred parents' on the Data screen, or uncheck 'Assume inbred parents' on Scoring."))
      # Feasibility: distinct crosses can't exceed n_parents * max_uses / 2.
      if (!is.null(d)) {
        n_par <- if (isTRUE(input$restrict_shared_ids)) d$n_shared_ids else length(d$gids)
        mu <- input$max_uses_per_parent %||% 6
        capacity <- floor(n_par * mu / 2)
        if (is.finite(capacity) && !is.null(input$n_crosses) && input$n_crosses > capacity)
          msgs <- c(msgs, sprintf(
            "n_crosses (%d) exceeds the capacity of %d for %d parents at max %d uses each - the allocator may return a shorter plan or report 'no feasible plan'. Lower n_crosses or raise max uses per parent.",
            input$n_crosses, capacity, n_par, mu))
      }
      if (length(msgs)) ngcd_callout(kind = "warn", shiny::tags$ul(lapply(msgs, shiny::tags$li)))
      else ngcd_callout("Ready to run. Click the button above.")
    })

    # ---- error/debug panel ----
    output$error_panel <- shiny::renderUI({
      e <- rv$error; if (is.null(e)) return(NULL)
      dbg <- paste0(
        "== ERROR ==\n", e$message,
        "\n\n== COMMAND ==\n", e$command,
        "\n\n== CONFIG ==\n", e$config_path,
        "\n\n== RUN LOG (tail) ==\n", e$log_tail)
      shiny::tagList(
        ngcd_callout(kind = "error",
          shiny::tags$b("Run failed. "), shiny::tags$span(e$message),
          if (!is.null(e$hint)) shiny::tags$div(class = "help-hint", shiny::tags$b("Likely fix: "), e$hint),
          if (isTRUE(e$het_fix)) shiny::tags$div(style = "margin-top:10px",
            shiny::actionButton("fix_exclude_het",
              sprintf("Exclude %d non-inbred parent%s & re-run", length(e$het_ids),
                      if (length(e$het_ids) == 1) "" else "s"),
              class = "btn-ndsu"),
            shiny::span(class = "help-hint", style = "margin-left:8px",
              "Drops only the flagged parents; keeps the rest of your data."))),
        shiny::tags$details(
          shiny::tags$summary(shiny::tags$b("Debug details (click to expand)")),
          shiny::tags$p(class = "help-hint", "Copy this when asking for help. Re-run the same config outside the app with the command below."),
          shiny::tags$pre(dbg),
          shiny::tags$p(class = "help-hint", "Run folder: ", shiny::tags$code(e$run_dir))))
    })

    # ---- save / load settings profiles ----
    presets_dir <- file.path(cfg$work_dir, "presets")
    if (!dir.exists(presets_dir)) dir.create(presets_dir, recursive = TRUE, showWarnings = FALSE)
    presets_refresh <- shiny::reactiveVal(0)
    preset_files <- shiny::reactive({
      presets_refresh()
      sort(list.files(presets_dir, pattern = "\\.json$"))
    })
    output$preset_pick_ui <- shiny::renderUI({
      f <- preset_files()
      shiny::selectInput("preset_pick", "Saved profiles",
        choices = if (length(f)) f else c("(none saved yet)" = ""))
    })
    write_settings <- function(path) {
      payload <- list(schema = "ngcd_settings.v1",
                      package_version = as.character(utils::packageVersion("nextgenCrossWorkbench")),
                      values = ngcd_collect_settings(input))
      jsonlite::write_json(payload, path, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE)
    }
    apply_settings_file <- function(path) {
      dat <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = FALSE),
                      error = function(e) NULL)
      if (is.null(dat) || is.null(dat$values)) { shiny::showNotification("Not a valid settings file.", type = "error"); return(invisible()) }
      ngcd_apply_settings(session, dat$values)
      shiny::showNotification("Settings loaded. Review the tabs, then Run.", type = "message")
    }
    shiny::observeEvent(input$save_preset, {
      nm <- gsub("[^A-Za-z0-9_-]+", "_", input$preset_name %||% "settings")
      if (!nzchar(nm)) nm <- "settings"
      path <- file.path(presets_dir, paste0(nm, ".json"))
      write_settings(path); presets_refresh(presets_refresh() + 1)
      shiny::showNotification(paste0("Saved profile '", nm, "'."), type = "message")
    })
    output$download_settings <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_-]+", "_", input$preset_name %||% "settings"), ".json"),
      content = function(file) write_settings(file))
    shiny::observeEvent(input$load_preset, {
      p <- input$preset_pick
      if (is.null(p) || !nzchar(p)) { shiny::showNotification("No saved profile selected.", type = "warning"); return() }
      apply_settings_file(file.path(presets_dir, p))
    })
    shiny::observeEvent(input$upload_settings, {
      shiny::req(input$upload_settings)
      apply_settings_file(input$upload_settings$datapath)
    })

    # Polyploid config (meta keys the runner's polyploid_design path reads).
    build_poly_params <- function() {
      p <- list(schema = "ng_run_config.v1", workflow = "polyploid_design",
        crop = input$crop,
        genotype_id_col = input$genotype_id_col %||% "NAME",
        phenotype_id_col = input$phenotype_id_col %||% "NAME",
        poly_trait_col = input$poly_trait_col,
        ploidy = as.integer(input$ploidy %||% "2"),
        n_crosses = if (identical(input$cross_number_mode, "auto"))
          as.integer(input$cross_sweep_k_max %||% 30) else input$n_crosses,
        max_uses_per_parent = input$max_uses_per_parent,
        dominance = isTRUE(input$poly_dominance),
        gain = input$poly_gain %||% "mean",
        double_reduction = num_or_null(input$poly_double_reduction) %||% 0,
        grm_method = input$poly_grm_method %||% "vanraden",
        selection_prop = input$selection_prop,
        run_qc = isTRUE(input$poly_run_qc),
        method = input$optimizer,
        max_pair_kinship = num_or_null(input$max_pair_kinship),
        seed = input$seed, write_outputs = FALSE, write_figures = FALSE)
      # gain-diversity dial (forwarded to the native allocator)
      if (identical(input$diversity_mode, "strategy")) p$strategy <- input$strategy
      if (identical(input$diversity_mode, "emphasis")) p$diversity_emphasis <- input$diversity_emphasis
      if (identical(input$diversity_mode, "target")) p$target_coancestry <- input$target_coancestry
      cc <- parse_pairs(input$committed_crosses)
      if (!is.null(cc)) p$committed_crosses <- list(parent1 = vapply(cc, `[`, "", 1), parent2 = vapply(cc, `[`, "", 2))
      p
    }

    do_run <- function(force_drop_het = FALSE) {
      if (!data_ready()) {
        shiny::showNotification(if (is_poly()) "Load a dosage file and a phenotype file first."
                                else "Load all four data tables first.", type = "error"); return() }
      b <- rv$backend
      if (is.null(b) || !isTRUE(b$backend_installed)) { shiny::showNotification("Backend not ready.", type = "error"); return() }

      prog <- shiny::Progress$new(session); on.exit(prog$close())
      prog$set(message = "Assembling configuration...", value = 0.15)
      poly <- is_poly()
      if (poly) {
        params   <- build_poly_params()
        run_data <- list(genotype = rv$data$genotype, phenotype = rv$data$phenotype)
        label    <- paste0("polyploid_", input$ploidy %||% "2")
      } else {
        params <- build_params()
        label  <- paste0(input$prediction_mode, "_", input$optimizer)
        # Optional reconciliation: drop unmapped markers / unphenotyped parents.
        run_data <- rv$data
        d <- align_diag()
        if (!is.null(d)) {
          if (isTRUE(input$restrict_shared_markers) && (length(d$miss_map) || length(d$extra_map))) {
            shared <- intersect(d$gmark, d$mmark)
            run_data$genotype <- run_data$genotype[, c(d$gid, intersect(names(run_data$genotype), shared)), drop = FALSE]
            run_data$map <- run_data$map[trimws(as.character(run_data$map[[d$mcol]])) %in% shared, , drop = FALSE]
            shiny::showNotification(
              paste0("Restricted to ", length(shared), " shared markers (dropped ",
                     length(d$miss_map), " from genotype, ", length(d$extra_map), " from map)."),
              type = "warning")
          }
          if (isTRUE(input$restrict_shared_ids) && length(d$miss_pheno)) {
            shared <- intersect(d$gids, d$pids)
            run_data$genotype  <- run_data$genotype[trimws(as.character(run_data$genotype[[d$gid]])) %in% shared, , drop = FALSE]
            run_data$phenotype <- run_data$phenotype[trimws(as.character(run_data$phenotype[[d$pid]])) %in% shared, , drop = FALSE]
            shiny::showNotification(paste0("Restricted to ", length(shared), " shared parents for this run."), type = "warning")
          }
          if ((isTRUE(input$drop_noninbred_parents) || isTRUE(force_drop_het)) && length(d$het_ids)) {
            drop <- d$het_ids
            run_data$genotype  <- run_data$genotype[!trimws(as.character(run_data$genotype[[d$gid]])) %in% drop, , drop = FALSE]
            run_data$phenotype <- run_data$phenotype[!trimws(as.character(run_data$phenotype[[d$pid]])) %in% drop, , drop = FALSE]
            shiny::showNotification(paste0("Dropped ", length(drop), " non-inbred parents for this run."), type = "warning")
          }
        }
      }

      out <- ngcd_run_backend(cfg, params, data = run_data, label = label, progress = prog)
      rv$run_dir <- out$run_dir; rv$runlog <- out$log

      if (isTRUE(out$ok)) {
        rv$result <- out$result; rv$error <- NULL
        rv$last <- format(Sys.time(), "%H:%M:%S")
        # Generate the run report (HTML + PDF) for the Report tab / link.
        rdir <- file.path(cfg$work_dir, "_report")
        if (!dir.exists(rdir)) dir.create(rdir, recursive = TRUE, showWarnings = FALSE)
        rv$report_ts <- as.integer(Sys.time())
        rv$report_error <- NULL
        tryCatch({
          ngcd_report_html(out$result, file.path(rdir, "report.html"))
          ngcd_report_pdf(out$result, file.path(rdir, "report.pdf"))
        }, error = function(e) {
          rv$report_error <- conditionMessage(e)
          shiny::showNotification(paste0("Run finished, but the report could not be built: ",
            conditionMessage(e)), type = "warning", duration = NULL)
        })
        shiny::showNotification("Run complete.", type = "message")
        bslib::nav_select("nav", "Results")
      } else {
        rv$result <- NULL
        log_lines <- strsplit(out$log %||% "", "\n")[[1]]
        emsg <- out$error_message %||% "Unknown error."
        dd <- tryCatch(align_diag(), error = function(e) NULL)
        rv$error <- list(
          message = emsg,
          hint = ngcd_error_hint(emsg),
          # offer the one-click exclusion only when the failure is the
          # residual-heterozygosity block AND we actually detected the parents.
          het_fix = grepl("residual-heterozygosity|assume_inbred|inbred", tolower(emsg)) &&
                    !is.null(dd) && length(dd$het_ids) > 0,
          het_ids = if (!is.null(dd)) dd$het_ids else character(0),
          command = out$command %||% "",
          config_path = out$config_path %||% "",
          run_dir = out$run_dir %||% "",
          log_tail = paste(utils::tail(log_lines, 25), collapse = "\n"))
        shiny::showNotification("Run failed - see the debug panel on the Run screen.", type = "error", duration = NULL)
      }
    }
    shiny::observeEvent(input$run, do_run())
    # One-click recovery from the residual-heterozygosity block: drop the
    # flagged parents (also ticking the Data-screen box) and re-run immediately.
    shiny::observeEvent(input$fix_exclude_het, {
      shiny::updateCheckboxInput(session, "drop_noninbred_parents", value = TRUE)
      do_run(force_drop_het = TRUE)
    })

    output$run_log <- shiny::renderText(rv$runlog %||% "No run yet.")

    res <- shiny::reactive(rv$result)
    output$results_banner <- shiny::renderUI({
      if (!is.null(rv$error))
        ngcd_callout(kind = "error", shiny::tags$b("Last run failed. "),
          "See the debug panel on the ", shiny::tags$b("Run"), " screen.")
    })
    output$results_kpis <- shiny::renderUI({
      r <- res(); if (is.null(r)) return(ngcd_callout("Run an analysis to see results."))
      ps <- r$plan_summary; sw <- r$cross_number_sweep
      crit_lab <- c(elbow_relative = "diminishing returns", elbow_kneedle = "diminishing returns (kneedle)",
                    ne_target = "effective population size", coancestry_budget = "coancestry budget")
      shiny::tagList(
        if (!is.null(sw) && !is.null(sw$recommended_k)) ngcd_callout(kind = "info",
          shiny::tags$b(sprintf("Number of crosses chosen automatically: K = %d", as.integer(sw$recommended_k))),
          sprintf(" (%s rule%s). See the diminishing-returns chart on the Report tab.",
                  crit_lab[[sw$criterion %||% "elbow_relative"]] %||% "diminishing returns",
                  if (length(sw$k_range)) sprintf(", swept K = %d-%d", min(sw$k_range), max(sw$k_range)) else "")),
        bslib::layout_columns(col_widths = c(2,2,2,2,2,2),
          bslib::card(ngcd_kpi(ngcd_num(ps$n_crosses, 0), "Crosses")),
          bslib::card(ngcd_kpi(ngcd_num(ps$mean_gain), "Mean gain")),
          bslib::card(ngcd_kpi(ngcd_num(ps$group_coancestry), "Group coancestry")),
          bslib::card(ngcd_kpi(ngcd_num(ps$unique_parents, 0), "Unique parents")),
          bslib::card(ngcd_kpi(ngcd_num(ps$max_parent_use, 0), "Max parent use")),
          bslib::card(ngcd_kpi(ngcd_num(ps$mean_progeny_inbreeding), "Mean progeny F"))))
    })
    output$res_selected <- DT::renderDT({
      r <- res(); shiny::req(r); df <- r$selected_crosses
      keep <- intersect(c("priority_rank","parent1","parent2","multi_trait_score","pair_kinship","priority_tier","priority_percentile"), names(df))
      extra <- grep("_value$", names(df), value = TRUE)
      ngcd_dt(df[, unique(c(keep, extra)), drop = FALSE], page = 15, priority_col = "priority_tier")
    })
    output$res_candidate <- DT::renderDT({ r <- res(); shiny::req(r); ngcd_dt(r$candidate_crosses, page = 15) })
    output$res_parentuse <- DT::renderDT({
      r <- res(); shiny::req(r); df <- r$selected_crosses
      pu <- table(c(df$parent1, df$parent2))
      ngcd_dt(data.frame(parent = names(pu), uses = as.integer(pu), row.names = NULL)[order(-as.integer(pu)), ], page = 15)
    })
    # ---- Report tab: executive summary + figures + open link + downloads ----
    # Only the figures that apply to this run are shown (no empty placeholders);
    # each is an interactive plotly chart (static fallback if plotly is absent),
    # and the same set + order is used in the HTML/PDF report.
    have_plotly <- requireNamespace("plotly", quietly = TRUE)
    output$res_report <- shiny::renderUI({
      r <- res(); if (is.null(r)) return(ngcd_callout("Run an analysis to generate the report."))
      ts <- rv$report_ts %||% 0
      figs <- ngcd_report_active(r)
      fig_cards <- lapply(figs, function(f) {
        bslib::card(full_screen = TRUE,
          bslib::card_header(f$title),
          shiny::div(class = "help-hint", style = "padding:0 12px", f$desc),
          if (have_plotly) plotly::plotlyOutput(paste0("repfig_", f$id), height = "380px")
          else shiny::plotOutput(paste0("repfigp_", f$id), height = "400px"))
      })
      shiny::tagList(
        shiny::div(style = "display:flex; gap:10px; align-items:center; margin-bottom:10px; flex-wrap:wrap",
          shiny::tags$a(href = paste0("ngcd_report/report.html?ts=", ts), target = "_blank",
            class = "btn btn-ndsu", "Open full report "),
          shiny::downloadButton("dl_report_pdf", "Download PDF", class = "btn-outline-secondary"),
          shiny::downloadButton("dl_report_html", "Download HTML", class = "btn-outline-secondary")),
        shiny::div(class = "ndsu-report", shiny::HTML(ngcd_exec_summary_html(r, figs))),
        if (!have_plotly) ngcd_callout(kind = "info", "Install ", shiny::tags$code("plotly"),
          " for interactive charts; the Open-full-report link and PDF still include every figure."),
        shiny::tags$hr(),
        do.call(shiny::tagList, fig_cards))
    })
    output$dl_report_pdf <- shiny::downloadHandler(
      filename = function() "crossing_report.pdf",
      content = function(file) { r <- res(); shiny::req(r); ngcd_report_pdf(r, file) })
    output$dl_report_html <- shiny::downloadHandler(
      filename = function() "crossing_report.html",
      content = function(file) { r <- res(); shiny::req(r); ngcd_report_html(r, file) })

    # register one interactive + one static output per registry figure, each
    # gated on its own applicability so non-applicable figures never render.
    for (.f in ngcd_fig_registry()) local({
      ff <- .f
      if (have_plotly)
        output[[paste0("repfig_", ff$id)]] <- plotly::renderPlotly({
          r <- res(); shiny::req(r, isTRUE(ff$applies(r))); ff$ply(r)
        })
      output[[paste0("repfigp_", ff$id)]] <- shiny::renderPlot({
        r <- res(); shiny::req(r, isTRUE(ff$applies(r))); ff$base(r)
      })
    })

    # ---- robust posterior plan ----
    output$res_robust_ui <- shiny::renderUI({
      r <- res(); shiny::req(r)
      rp <- r$robust_plan
      if (is.null(rp)) return(ngcd_callout(kind = "info",
        shiny::tags$b("Robust posterior allocation was not run."),
        shiny::tags$p("Enable ", shiny::tags$b("Robust posterior allocation"), " on the ",
          shiny::tags$b("Allocation"), " screen (it also turns on posterior prediction), then re-run. ",
          "It re-optimizes the plan on a pessimistic posterior quantile so the chosen crosses hold up under marker-effect uncertainty.")))
      if (!is.null(rp$error)) return(ngcd_callout(kind = "warn",
        shiny::tags$b("Robust allocation could not be computed: "), rp$error))
      sm <- rp$summary %||% list()
      obj_lab <- if (identical(sm$robust_objective, "posterior_topn_prob"))
        sprintf("top-%s inclusion probability", sm$robust_top_n_target %||% "N")
      else sprintf("pessimistic %s quantile of gain", sm$robustness_quantile %||% "?")
      shiny::tagList(
        ngcd_callout(kind = "info",
          shiny::tags$b(sprintf("Robust plan (%s).", obj_lab)),
          sprintf(" It keeps %s of your standard %s crosses and changes %s to more uncertainty-robust choices.",
                  rp$n_shared_with_standard %||% "?", rp$n_crosses %||% "?", rp$n_changed %||% "?"),
          " Gain column: ", shiny::tags$code(rp$gain_col %||% "?"), "."),
        shiny::div(class = "help-hint",
          "Columns show the robust gain and the posterior mean / lower / upper for each cross."),
        DT::DTOutput("res_robust_tbl"))
    })
    output$res_robust_tbl <- DT::renderDT({
      r <- res(); shiny::req(r); rp <- r$robust_plan
      shiny::req(is.list(rp), is.data.frame(rp$crosses))
      ngcd_dt(rp$crosses, page = 15)
    })

    # ---- polyploid design plan ----
    output$res_poly_ui <- shiny::renderUI({
      r <- res(); shiny::req(r)
      if (!isTRUE(r$poly_design)) return(ngcd_callout(kind = "info",
        shiny::tags$b("This was a standard (diploid) run."),
        shiny::tags$p("Switch ", shiny::tags$b("Analysis type"), " to ",
          shiny::tags$b("Polyploid design"), " on the Data screen to design crosses for autopolyploids or clonal / heterosis crops (dosage 0..ploidy, optional dominance).")))
      pp <- r$poly_plan %||% list(); st <- r$settings %||% list(); qc <- pp$qc %||% list()
      shiny::tagList(
        ngcd_callout(kind = "info",
          shiny::tags$b(sprintf("Polyploid design (ploidy %s%s).",
            r$ploidy %||% st$ploidy %||% "?",
            if (isTRUE(pp$dominance)) ", additive + dominance" else ", additive")),
          sprintf(" Cross value: %s. GRM: %s. Double reduction: %s.",
            st$gain %||% "mean", st$grm_method %||% "vanraden", st$double_reduction %||% 0)),
        if (length(qc)) shiny::div(class = "help-hint",
          sprintf("Ploidy-aware QC: %s samples x %s markers; %s monomorphic dropped, %s duplicate samples; %s markers kept, %s samples kept.",
            qc$n_samples %||% "?", qc$n_markers %||% "?", qc$n_monomorphic %||% 0,
            qc$n_duplicate_samples %||% 0, qc$n_markers_clean %||% "?", qc$n_samples_clean %||% "?")),
        shiny::tags$p(class = "help-hint", "Columns include the cross mean, heterosis, additive/dominance variance, and dominance-aware usefulness."),
        DT::DTOutput("res_poly_tbl"))
    })
    output$res_poly_tbl <- DT::renderDT({
      r <- res(); shiny::req(r, isTRUE(r$poly_design))
      df <- r$poly_plan$crosses %||% r$selected_crosses
      shiny::req(is.data.frame(df))
      ngcd_dt(df[, setdiff(names(df), ".linear_gain"), drop = FALSE], page = 15)
    })

    has_frontier <- function(r) {
      fr <- r$plan_summary$frontier
      !is.null(fr) && is.data.frame(fr) && nrow(fr) > 0
    }
    output$res_frontier_ui <- shiny::renderUI({
      r <- res(); shiny::req(r)
      if (has_frontier(r)) {
        if (requireNamespace("plotly", quietly = TRUE))
          return(shiny::tagList(
            shiny::div(class = "help-hint", "Hover a point for its details; drag to zoom, double-click to reset."),
            plotly::plotlyOutput("res_frontier_plotly", height = "440px")))
        return(shiny::plotOutput("res_frontier", height = "420px"))
      }
      am <- r$settings$allocation_method %||% "?"
      ngcd_callout(kind = "info",
        shiny::tags$b("No frontier for this run - this is expected, not an error."),
        shiny::tags$p("The gain-diversity frontier is produced only by the ", shiny::tags$b("OCS"),
          " allocator when the gain-diversity dial is engaged (a strategy preset, the emphasis slider, or a coancestry cap)."),
        shiny::tags$p("This run used ", shiny::tags$b(paste0("allocation_method = ", am)),
          if (grepl("alphamate", am)) shiny::tagList(", which hits a single operating point set by ",
            shiny::tags$code("alphamate_target_degree"), " (20 = gain-heavy, 45 = balanced, 70 = diversity-heavy) rather than sweeping a frontier.")
          else ", which did not sweep a frontier (e.g. a fixed lambda_group with no dial)."),
        shiny::tags$p("To get the frontier chart, set ", shiny::tags$b("Allocation method = ocs"),
          " on the Allocation screen and keep the dial on a strategy / emphasis / coancestry-cap setting."))
    })
    output$res_frontier <- shiny::renderPlot({
      r <- res(); shiny::req(r); fr <- r$plan_summary$frontier
      if (is.null(fr) || !is.data.frame(fr) || !nrow(fr)) { plot.new(); graphics::text(0.5, 0.5, "No frontier for this run."); return() }
      op_x <- r$plan_summary$group_coancestry; op_y <- r$plan_summary$mean_gain
      plot(fr$group_coancestry, fr$mean_gain, type = "b", pch = 19, col = "#00583d",
           xlab = "Group coancestry (diversity <- ... -> gain)", ylab = "Mean gain",
           main = "Gain-diversity frontier", cex = 1.1)
      if (!is.null(op_x) && !is.null(op_y))
        graphics::points(op_x, op_y, pch = 21, bg = "#FFC425", col = "#003524", cex = 2.4, lwd = 2)
      graphics::legend("bottomright", legend = c("frontier","selected plan"), pch = c(19,21),
                       col = c("#00583d","#003524"), pt.bg = c(NA,"#FFC425"), bty = "n")
    })
    if (requireNamespace("plotly", quietly = TRUE)) {
      output$res_frontier_plotly <- plotly::renderPlotly({
        r <- res(); shiny::req(r); fr <- r$plan_summary$frontier
        shiny::req(is.data.frame(fr) && nrow(fr) > 0)
        ngcd_frontier_plotly(fr, r$plan_summary$group_coancestry, r$plan_summary$mean_gain)
      })
    }
    output$res_qc <- shiny::renderUI({
      r <- res(); shiny::req(r); qc <- r$qc; status <- qc$status %||% "unknown"
      kind <- if (identical(status,"pass")) "ok" else if (identical(status,"blocker")) "error" else "warn"
      shiny::tagList(shiny::tags$p(ngcd_badge(paste0("QC: ", status), kind)),
        if (!is.null(qc$counts)) shiny::tags$p(sprintf("Blockers: %s | Warnings: %s | Issues: %s",
          qc$counts$blockers %||% 0, qc$counts$warnings %||% 0, qc$counts$issues %||% 0)),
        if (!is.null(qc$tables) && is.data.frame(qc$tables)) shiny::renderTable(qc$tables))
    })
    output$res_match <- shiny::renderPrint({ r <- res(); shiny::req(r); utils::str(r$input_match_audit) })
    output$res_effects <- DT::renderDT({ r <- res(); shiny::req(r); df <- r$effect_summary; if (is.null(df)) ngcd_dt(NULL) else ngcd_dt(df, page = 10) })
    output$res_settings <- shiny::renderPrint({ r <- res(); shiny::req(r)
      cat("Package version:", r$package_version, "\nPrediction mode:", r$prediction_mode, "\n\n"); utils::str(r$settings) })

    output$res_downloads <- shiny::renderUI({
      r <- res(); shiny::req(r); rd <- rv$run_dir
      files <- if (!is.null(rd) && dir.exists(rd)) list.files(rd, recursive = FALSE) else character(0)
      files <- setdiff(files, "inputs")
      if (!length(files)) return(ngcd_callout("No output files. Enable 'Write workbook/figures' on Output to generate them."))
      shiny::tagList(shiny::tags$p("Files in this run's folder:"),
        shiny::tags$ul(lapply(files, function(fn) shiny::tags$li(shiny::downloadLink(paste0("dl_", make.names(fn)), fn)))),
        shiny::div(class = "help-hint", paste0("Run folder: ", rd)))
    })
    shiny::observe({
      r <- res(); shiny::req(r); rd <- rv$run_dir; shiny::req(rd)
      for (fn in setdiff(list.files(rd), "inputs")) local({
        f <- fn
        output[[paste0("dl_", make.names(f))]] <- shiny::downloadHandler(
          filename = function() f, content = function(file) file.copy(file.path(rd, f), file, overwrite = TRUE))
      })
    })
  }
}
