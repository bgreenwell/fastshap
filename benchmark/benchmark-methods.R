## Benchmark: exact/reference vs. method = "strumbelj" (default) vs.
## method = "permutation"
##
## Compares accuracy (agreement with a ground truth) and wall-clock time
## across a grid of `nsim` values, for three model types:
##   - lm       (ground truth = exact Linear SHAP)
##   - xgboost  (ground truth = exact Tree SHAP; skipped if not installed)
##   - ranger   (no exact method available for random forests, so the ground
##               truth here is a "reference" value: the default estimator run
##               at a much larger nsim than anything in the timed grid below.
##               This is NOT exact -- it's a high-precision MC estimate used
##               as a stand-in so ranger can still be scored for accuracy;
##               skipped if ranger is not installed)
##
## This directory is excluded from the built package via .Rbuildignore; run it
## from an interactive session or `Rscript benchmark/benchmark-methods.R` from
## the package root (with the dev version of fastshap installed).
##
## Outputs (written to this directory):
##   - benchmark-results.csv
##   - benchmark-plot.png

suppressMessages(library(fastshap))

set.seed(101)
n_background  <- 1000L
p_features    <- 10L
n_explain     <- 50L
nsim_grid     <- c(10L, 25L, 50L, 100L, 250L, 500L)
timing_reps   <- 3L
reference_nsim <- 5000L  # for models with no exact method (e.g., ranger)

trn <- gen_friedman(n_background, seed = 101)
X <- subset(trn, select = -y)
newdata <- X[seq_len(n_explain), ]


#' Time and score one (model, method, nsim) combination
#'
#' Uses a fixed seed so every repetition is identical (fastshap's batched
#' estimators are seed-reproducible), so `timing_reps` only reduces timing
#' noise -- it does not change the result used for the accuracy metrics.
#'
#' `ground_truth` is either true exact Shapley values or (for models with no
#' exact method) a high-nsim reference estimate -- `ground_truth_label`
#' records which, so the printed/saved results stay honest about it.
benchmark_one <- function(fit, pred_wrapper, method, nsim, ground_truth,
                          ground_truth_label, model_label) {
  times <- numeric(timing_reps)
  result <- NULL
  for (i in seq_len(timing_reps)) {
    set.seed(2024)
    t <- system.time({
      result <- explain(fit, X = X, pred_wrapper = pred_wrapper,
                        newdata = newdata, nsim = nsim, method = method)
    })
    times[i] <- t[["elapsed"]]
  }
  err <- as.numeric(result) - as.numeric(ground_truth)
  data.frame(
    model = model_label,
    method = method,
    nsim = nsim,
    elapsed = median(times),
    cor = cor(as.numeric(result), as.numeric(ground_truth)),
    rmse = sqrt(mean(err^2)),
    max_abs_err = max(abs(err)),
    ground_truth = ground_truth_label
  )
}


run_scenario <- function(fit, pred_wrapper, ground_truth, model_label,
                         ground_truth_label = "exact") {
  do.call(rbind, lapply(nsim_grid, function(nsim) {
    rbind(
      benchmark_one(fit, pred_wrapper, "strumbelj", nsim, ground_truth,
                   ground_truth_label, model_label),
      benchmark_one(fit, pred_wrapper, "permutation", nsim, ground_truth,
                   ground_truth_label, model_label)
    )
  }))
}


results <- list()


################################################################################
# Scenario 1: lm (exact = Linear SHAP)
################################################################################

cat("Running lm scenario...\n")
fit_lm <- lm(y ~ ., data = trn)
pfun_lm <- function(object, newdata) predict(object, newdata = newdata)
exact_lm <- explain(fit_lm, X = X, newdata = newdata, exact = TRUE)
results$lm <- run_scenario(fit_lm, pfun_lm, exact_lm, "lm")


################################################################################
# Scenario 2: xgboost (exact = Tree SHAP), if available
################################################################################

if (requireNamespace("xgboost", quietly = TRUE)) {
  cat("Running xgboost scenario...\n")
  Xm <- data.matrix(X)
  newdata_m <- data.matrix(newdata)
  set.seed(102)
  fit_xgb <- xgboost::xgboost(x = Xm, y = trn$y, max_depth = 3,
                              learning_rate = 0.1, nrounds = 100,
                              verbosity = 0L)
  pfun_xgb <- function(object, newdata) predict(object, newdata = newdata)
  exact_xgb <- explain(fit_xgb, X = newdata_m, exact = TRUE)
  results$xgb <- run_scenario(fit_xgb, pfun_xgb, exact_xgb, "xgboost")
} else {
  cat("xgboost not installed -- skipping that scenario.\n")
}


################################################################################
# Scenario 3: ranger (no exact method -- scored against a high-nsim reference)
################################################################################

if (requireNamespace("ranger", quietly = TRUE)) {
  cat("Running ranger scenario...\n")
  set.seed(103)
  fit_rfo <- ranger::ranger(y ~ ., data = trn, num.trees = 500, num.threads = 1L)
  pfun_rfo <- function(object, newdata) predict(object, data = newdata)$predictions

  # No exact = TRUE for ranger; use a much larger nsim than the timed grid,
  # via the established baseline method (not permutation, to avoid any
  # appearance of circularity in scoring permutation against its own
  # reference), as a high-precision stand-in instead (labeled as such below,
  # not "exact")
  set.seed(2025)
  reference_rfo <- explain(fit_rfo, X = X, pred_wrapper = pfun_rfo,
                           newdata = newdata, nsim = reference_nsim,
                           method = "strumbelj")
  results$ranger <- run_scenario(fit_rfo, pfun_rfo, reference_rfo, "ranger",
                                 ground_truth_label = paste0("reference (nsim=", reference_nsim, ")"))
} else {
  cat("ranger not installed -- skipping that scenario.\n")
}


################################################################################
# Combine, print, save
################################################################################

res <- do.call(rbind, results)
rownames(res) <- NULL
res$elapsed <- round(res$elapsed, 4)
res$cor <- round(res$cor, 5)
res$rmse <- round(res$rmse, 5)
res$max_abs_err <- round(res$max_abs_err, 5)

cat("\n")
print(res)
cat("\nGround truth by model:\n")
print(unique(res[, c("model", "ground_truth")]), row.names = FALSE)

write.csv(res, file.path("benchmark", "benchmark-results.csv"), row.names = FALSE)
cat("\nResults written to benchmark/benchmark-results.csv\n")


################################################################################
# Plot: accuracy (correlation with ground truth) and time vs. nsim, per
# model/method. Note "ground truth" means true exact Shapley values for lm and
# xgboost, but a high-nsim reference estimate for ranger (see console output
# above) -- these are not the same guarantee, and the plot legend/labels below
# don't distinguish them, so consult the printed table for that distinction.
################################################################################

png(file.path("benchmark", "benchmark-plot.png"), width = 900, height = 450, res = 120)
op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

models <- unique(res$model)
methods <- c("strumbelj", "permutation")
cols <- c(strumbelj = "#1b9e77", permutation = "#d95f02")
ltys <- setNames(seq_along(models), models)

plot(range(res$nsim), range(res$cor), type = "n", log = "x",
    xlab = "nsim", ylab = "cor(estimate, ground truth)",
    main = "Accuracy vs. nsim")
for (m in models) {
  for (meth in methods) {
    d <- res[res$model == m & res$method == meth, ]
    lines(d$nsim, d$cor, col = cols[meth], lty = ltys[m], type = "b", pch = 16)
  }
}
legend("bottomright", legend = c(methods, models),
      col = c(cols, rep("black", length(models))),
      lty = c(1, 1, ltys), pch = c(16, 16, rep(NA, length(models))),
      bty = "n", cex = 0.8)

plot(range(res$nsim), range(res$elapsed), type = "n", log = "xy",
    xlab = "nsim", ylab = "elapsed time (s)",
    main = "Time vs. nsim")
for (m in models) {
  for (meth in methods) {
    d <- res[res$model == m & res$method == meth, ]
    lines(d$nsim, d$elapsed, col = cols[meth], lty = ltys[m], type = "b", pch = 16)
  }
}

par(op)
dev.off()
cat("Plot written to benchmark/benchmark-plot.png\n")
