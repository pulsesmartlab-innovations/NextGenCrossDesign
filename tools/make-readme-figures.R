#!/usr/bin/env Rscript
# Regenerate the analytical README figures (the cross-number optimizer chart) from the backend on a small simulated panel. The app renders the
# same computations interactively; these static PNGs are for the repo landing page.
#
#   Rscript tools/make-readme-figures.R
#
# Requires: nextgenCrossDesign (backend) + ggplot2. Writes to man/figures/.
suppressMessages({library(nextgenCrossDesign); library(ggplot2)})
set.seed(11)
FIG <- file.path("man", "figures"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

## ---- simulated inbred panel with a few elite parents (skewed merit) ----
nP <- 24; nM <- 800; nChr <- 6; nQTL <- 30
ids <- sprintf("P%02d", seq_len(nP))
G <- matrix(2 * rbinom(nP * nM, 1, 0.5), nP, nM,
            dimnames = list(ids, sprintf("m%03d", seq_len(nM))))
qtl <- sort(sample(nM, nQTL)); beta <- numeric(nM); beta[qtl] <- abs(rnorm(nQTL, 2, 0.5))
for (p in ids[1:5]) G[p, qtl] <- ifelse(runif(nQTL) < 0.9, 2, 0)   # elite parents
chr <- rep(seq_len(nChr), length.out = nM)
pos <- ave(seq_len(nM), chr, FUN = function(i) (seq_along(i) - 1) * 2)
map <- ng_prepare_marker_map(data.frame(marker = colnames(G), chr = chr, pos_cm = pos), colnames(G))
gv  <- as.numeric(scale(G, scale = FALSE) %*% beta)
y   <- setNames(gv + rnorm(nP, sd = 0.5 * sd(gv)), ids)
eff <- ng_fit_ridge_effects(G, y, ids)
scores <- ng_score_crosses(geno = G, effects = eff, marker_map = map, ids = ids,
                           pairs = ng_make_pairs(ids, include_self = FALSE), target = "DH")
K <- ng_parent_kinship(G)
gcol <- if ("usefulness_pmv" %in% names(scores)) "usefulness_pmv" else "rank_score"

## ---- cross-number optimizer (diminishing returns) ----
curve <- ng_optimize_mating_plan_curve(scores, K_range = 2:24, parent_kinship = K,
             criterion = "elbow_relative", relative_threshold = 0.5, max_crosses_per_parent = 3)
recK <- attr(curve, "elbow_K")
p1 <- ng_plot_diminishing_returns(curve) +
  labs(title = "Cross-number optimizer — diminishing returns",
       subtitle = sprintf("Recommended number of crosses (marginal-gain elbow): %s", recK %||% "n/a")) +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
ggsave(file.path(FIG, "screen-22-cross-number-optimizer.png"), p1,
       width = 8.5, height = 5, dpi = 130, bg = "white")


cat("wrote man/figures/screen-22-cross-number-optimizer.png\n")
