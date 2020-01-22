# Exits
if (!requireNamespace("earth", quietly = TRUE)) {
  exit_file("Package earth missing")
}

# # Load required packages
# suppressMessages({
#   library(earth)
# })

# Generate training data from the Friedman 1 benchmark problem
trn <- gen_friedman(500, seed = 101)
X <- subset(trn, select = -y)

# Fit a MARS model to the simulated Friedman benchmark data
mars <- earth::earth(y ~ ., data = trn, degree = 2)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)[, 1L, drop = TRUE]
}

# Expect error whenever `adjust = TRUE` and `nsim < 2`
expect_error(
  fastshap::explain(mars, X = X, pred_wrapper = pdun, adjust = TRUE)
)

# Compute approximate Shapley values with adjustment
set.seed(1438)
shap1 <- fastshap::explain(  # explain all rows
  object = mars,
  X = X,
  nsim = 30,
  pred_wrapper = pfun,
  adjust = TRUE
)
set.seed(1439)
shap2 <- fastshap::explain(  # explaiin a single row
  object = mars,
  X = X,
  nsim = 30,
  pred_wrapper = pfun,
  adjust = TRUE,
  newdata = X[1L, ]
)
set.seed(1440)
shap3 <- fastshap::explain(  # explain a few rows
  object = mars,
  X = X,
  nsim = 30,
  pred_wrapper = pfun,
  adjust = TRUE,
  newdata = X[1L:3L, ]
)

# Expectations
tol <- 1e-05
fnull <- mean(fx <- pfun(mars, newdata = X))
diffs <- c(
  max(rowSums(shap1) - (fx - fnull)), 
  max(rowSums(shap2) - (fx[1L] - fnull)), 
  max(rowSums(shap3) - (fx[1L:3L] - fnull))
)
expect_true(max(diffs) < tol)
