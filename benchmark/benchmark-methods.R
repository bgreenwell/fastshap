## Benchmark: exact vs. method = "strumbelj" (default) vs. method = "permutation"
##
## Compares accuracy (agreement with exact Shapley values) and wall-clock time
## across a grid of `nsim` values, for two model types that support exact
## Shapley values out of the box:
##   - lm       (exact = Linear SHAP)
##   - xgboost  (exact = Tree SHAP; skipped if xgboost is not installed)
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
n_background <- 1000L
p_features   <- 10L
n_explain    <- 50L
nsim_grid    <- c(10L, 25L, 50L, 100L, 250L, 500L)
timing_reps  <- 3L

trn <- gen_friedman(n_background, seed = 101)
X <- subset(trn, select = -y)
newdata <- X[seq_len(n_explain), ]


#' Time and score one (model, method, nsim) combination
#'
#' Uses a fixed seed so every repetition is identical (fastshap's batched
#' estimators are seed-reproducible), so `timing_reps` only reduces timing
#' noise -- it does not change the result used for the accuracy metrics.
benchmark_one <- function(fit, pred_wrapper, method, nsim, exact_vals,
                          model_label) {
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
  err <- as.numeric(result) - as.numeric(exact_vals)
  data.frame(
    model = model_label,
    method = method,
    nsim = nsim,
    elapsed = median(times),
    cor = cor(as.numeric(result), as.numeric(exact_vals)),
    rmse = sqrt(mean(err^2)),
    max_abs_err = max(abs(err))
  )
}


run_scenario <- function(fit, pred_wrapper, exact_vals, model_label) {
  do.call(rbind, lapply(nsim_grid, function(nsim) {
    rbind(
      benchmark_one(fit, pred_wrapper, "strumbelj", nsim, exact_vals, model_label),
      benchmark_one(fit, pred_wrapper, "permutation", nsim, exact_vals, model_label)
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

write.csv(res, file.path("benchmark", "benchmark-results.csv"), row.names = FALSE)
cat("\nResults written to benchmark/benchmark-results.csv\n")


################################################################################
# Plot: accuracy (correlation with exact) and time vs. nsim, per model/method
################################################################################

png(file.path("benchmark", "benchmark-plot.png"), width = 900, height = 450, res = 120)
op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

models <- unique(res$model)
methods <- c("strumbelj", "permutation")
cols <- c(strumbelj = "#1b9e77", permutation = "#d95f02")
ltys <- setNames(seq_along(models), models)

plot(range(res$nsim), range(res$cor), type = "n", log = "x",
    xlab = "nsim", ylab = "cor(estimate, exact)",
    main = "Accuracy vs. nsim")
for (m in models) {
  for (meth in methods) {
    d <- res[res$model == m & res$method == meth, ]
    lines(d$nsim, d$cor, col = cols[meth], lty = ltys[m], type = "b", pch = 16)
  }
}
legend("bottomright", legend = c(methods, models),
      col = c(cols, "black", "black"),
      lty = c(1, 1, ltys), pch = c(16, 16, NA, NA), bty = "n", cex = 0.8)

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
