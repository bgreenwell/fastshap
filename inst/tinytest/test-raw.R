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


################################################################################
# Regression tests: nsim=1 output shape (GH issue #77)
#
# replicate(1L, vector) returns a plain vector, not a matrix. This caused
# mean.default() to be dispatched instead of rowMeans(), producing wrong
# results for nrow(newdata) > 1, and a dimnames/colnames error for nrow == 1.
################################################################################

fit.ppr <- ppr(mpg ~ ., data = mtcars, nterms = 5)
pfun.ppr <- function(object, newdata) predict(object, newdata = newdata)
X.ppr <- subset(mtcars, select = -mpg)

# nsim=1, single row: must return a 1 x p matrix (not a vector)
set.seed(1)
shap1 <- fastshap::explain(fit.ppr, X = X.ppr, pred_wrapper = pfun.ppr,
                           nsim = 1L, newdata = X.ppr[1L, , drop = FALSE])
expect_true(is.matrix(shap1))
expect_equal(nrow(shap1), 1L)
expect_equal(ncol(shap1), ncol(X.ppr))
expect_equal(colnames(shap1), names(X.ppr))

# nsim=1, multiple rows: must return an n x p matrix with per-row values
set.seed(2)
shap_multi <- fastshap::explain(fit.ppr, X = X.ppr, pred_wrapper = pfun.ppr,
                                nsim = 1L, newdata = X.ppr[1:3, ])
expect_true(is.matrix(shap_multi))
expect_equal(nrow(shap_multi), 3L)
expect_equal(ncol(shap_multi), ncol(X.ppr))

# nsim=1 and nsim=10 means should be consistent (no global-mean collapse)
set.seed(42)
shap_nsim1 <- fastshap::explain(fit.ppr, X = X.ppr, pred_wrapper = pfun.ppr,
                                nsim = 1L, newdata = X.ppr[1:5, ])
set.seed(42)
shap_nsim10 <- fastshap::explain(fit.ppr, X = X.ppr, pred_wrapper = pfun.ppr,
                                 nsim = 10L, newdata = X.ppr[1:5, ])
# row Shapley values should differ across observations (not collapsed to one mean)
expect_true(length(unique(shap_nsim1[, 1L])) > 1L)
