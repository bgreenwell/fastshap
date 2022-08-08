# Exits
if (!requireNamespace("earth", quietly = TRUE)) {
  exit_file("Package earth missing")
}

# Check C++ function
num_rows <- 100000
num_cols <- 10
set.seed(888)
O <- fastshap:::genOMat(num_rows, num_cols)
tab <- table(apply(O, MARGIN = 1, FUN = sum))
expect_true(
  all(round(tab / num_rows, digits = 2) == 1 / num_cols)
)

# Generate training data from the Friedman 1 benchmark problem
trn <- gen_friedman(500, seed = 101)
X <- subset(trn, select = -y)

# Fit a fit model to the simulated Friedman benchmark data
fit <- earth::earth(y ~ ., data = trn, degree = 2)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)[, 1L, drop = TRUE]
}

# Generate approximate Shapley values for entire training set
set.seed(102)  # for reproducibility
shap_all <- explain(fit, X = X, pred_wrapper = pfun, nsim = 1)

# Check argument types
expect_error(
  explain(fit, X = X, pred_wrapper = pfun, newdata = data.matrix(X[1L, ]))
)

# Missing arguments
expect_error(  # Argument `X` is missing
  explain(fit, pred_wrapper = pfun, newdata = data.matrix(X[1L, ]))
)
expect_error(  # Argument `pred_wrapper` is missing
  explain(fit, X = X, newdata = data.matrix(X[1L, ]))
)

# Check dimensions
expect_identical(
  current = dim(shap_all),
  target = dim(X)
)

# Check column names
expect_identical(
  current = colnames(shap_all),
  target = names(X)
)

# Check class 
expect_identical(
  current = class(shap_all),
  target = c("matrix", "array")
)

# Check Shapley-based variable importance
vi <- apply(shap_all, MARGIN = 2, FUN = function(x) sum(abs(x)))
expect_identical(
  current = sum(vi[6L:10L]),
  target = 0
)

# Generate approximate Shapley values for a single row using first five features
set.seed(103)  # for reproducibility
shap_3 <- explain(fit, feature_names = names(X)[1L:5L], X = X, 
                  pred_wrapper = pfun, nsim = 1, 
                  newdata = X[1L, , drop = FALSE])

# Check dimensions
expect_identical(
  current = dim(shap_3),
  target = c(1L, 5L)
)

# Check approximate Shapley values for a single feature
set.seed(104)
shap_single <- explain(fit, feature_names = "x3", X = X, pred_wrapper = pfun)

# Check dimensions
expect_identical(
  current = dim(shap_single),
  target = c(nrow(X), 1L)
)

# Check column names
expect_identical(
  current = colnames(shap_single),
  target = "x3"
)

# Check class 
expect_identical(
  current = class(shap_single),
  target = c("matrix", "array")
)
