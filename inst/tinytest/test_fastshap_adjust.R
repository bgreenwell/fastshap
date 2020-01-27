# Generate training data from the Friedman 1 benchmark problem
trn <- gen_friedman(500, seed = 101)
X <- subset(trn, select = -y)

# Fit a linear model to the simulated Friedman benchmark data
fit <- stats::lm(y ~ ., data = trn)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)
}

# Expect error whenever `adjust = TRUE` and `nsim < 2`
expect_error(
  fastshap::explain(fit, X = X, pred_wrapper = pfun, adjust = TRUE)
)

# Compute approximate Shapley values with adjustment
set.seed(1438)
shap1 <- fastshap::explain(  # explain all rows
  object = fit,
  X = X,
  nsim = 30,
  pred_wrapper = pfun,
  adjust = TRUE
)
set.seed(1439)
shap2 <- fastshap::explain(  # explaiin a single row
  object = fit,
  X = X,
  nsim = 30,
  pred_wrapper = pfun,
  adjust = TRUE,
  newdata = X[1L, ]
)
set.seed(1440)
shap3 <- fastshap::explain(  # explain a few rows
  object = fit,
  X = X,
  nsim = 30,
  pred_wrapper = pfun,
  adjust = TRUE,
  newdata = X[1L:3L, ]
)

# Expectations
tol <- 1e-05
fnull <- mean(fx <- pfun(fit, newdata = X))
diffs <- c(
  max(rowSums(shap1) - (fx - fnull)), 
  max(rowSums(shap2) - (fx[1L] - fnull)), 
  max(rowSums(shap3) - (fx[1L:3L] - fnull))
)
expect_true(max(diffs) < tol)
