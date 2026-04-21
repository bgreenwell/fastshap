# Test the raw argument in explain()

# Load required packages
# exit_if_not(requireNamespace("ranger", quietly = TRUE))

# Generate training data from the Friedman 1 benchmark problem
trn <- fastshap::gen_friedman(100, seed = 101)
X <- subset(trn, select = -y)

# Fit a random forest model
fit <- ranger::ranger(y ~ ., data = trn, num.trees = 100, seed = 101)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Number of simulations
nsim <- 10

# Compute raw Shapley values (multi-row)
shap_raw <- fastshap::explain(fit, X = X, pred_wrapper = pfun, nsim = nsim,
                              newdata = X[1:3, ], raw = TRUE, seed = 101)

# Check output: should be a 3D array of shape (n x p x nsim)
expect_true(is.array(shap_raw))
expect_equal(dim(shap_raw), c(3L, ncol(X), nsim))
expect_equal(dimnames(shap_raw)[[2L]], names(X))

# apply(raw, 1:2, mean) must equal the raw=FALSE output (same seed → same draws)
shap_mean <- fastshap::explain(fit, X = X, pred_wrapper = pfun, nsim = nsim,
                               newdata = X[1:3, ], raw = FALSE, seed = 101)
expect_equal(as.numeric(apply(shap_raw, 1:2, mean)), as.numeric(shap_mean))

# Compute raw Shapley values (single-row) — should also be (1 x p x nsim)
shap_raw_1 <- fastshap::explain(fit, X = X, pred_wrapper = pfun, nsim = nsim,
                                newdata = X[1L, , drop = FALSE], raw = TRUE, seed = 101)
expect_true(is.array(shap_raw_1))
expect_equal(dim(shap_raw_1), c(1L, ncol(X), nsim))

# It should also work with parallel execution
if (requireNamespace("foreach", quietly = TRUE) &&
    requireNamespace("doParallel", quietly = TRUE)) {
  doParallel::registerDoParallel(cores = 2)
  shap_raw_parallel <- fastshap::explain(fit, X = X, pred_wrapper = pfun,
                                         nsim = nsim, newdata = X[1:3, ],
                                         raw = TRUE, parallel = TRUE, seed = 101)
  # Unregister parallel backend
  foreach::registerDoSEQ()

  # Check output
  expect_true(is.array(shap_raw_parallel))
  expect_equal(dim(shap_raw_parallel), c(3L, ncol(X), nsim))
}
