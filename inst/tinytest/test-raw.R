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

# Compute raw Shapley values
shap_raw <- fastshap::explain(fit, X = X, pred_wrapper = pfun, nsim = nsim, 
                              newdata = X[1:3, ], raw = TRUE, seed = 101)

# Check output of raw results
expect_true(is.list(shap_raw))
expect_equal(length(shap_raw), ncol(X))
expect_true(all(sapply(shap_raw, is.matrix)))
expect_true(all(sapply(shap_raw, function(x) all(dim(x) == c(3, nsim)))))

# It should also work with parallel execution
if (requireNamespace("foreach", quietly = TRUE) && 
    requireNamespace("doParallel", quietly = TRUE)) {
  doParallel::registerDoParallel(cores = 2)
  shap_raw_parallel <- fastshap::explain(fit, X = X, pred_wrapper = pfun, 
                                         nsim = nsim, newdata = X[1:3, ], 
                                         raw = TRUE, parallel = TRUE, seed = 101)
  # Unregister parallel backend
  foreach::registerDoSEQ()
  
  # Check output of raw results
  expect_true(is.list(shap_raw_parallel))
  expect_equal(length(shap_raw_parallel), ncol(X))
  expect_true(all(sapply(shap_raw_parallel, is.matrix)))
  expect_true(all(sapply(shap_raw_parallel, 
                         function(x) all(dim(x) == c(3, nsim)))))
  
  
}
