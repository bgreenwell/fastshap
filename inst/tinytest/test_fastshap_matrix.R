# Exits
if (!requireNamespace("earth", quietly = TRUE)) {
  exit_file("Package earth missing")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  exit_file("Package ggplot2 missing")
}

# Load required packages
suppressMessages({
  # library(earth)
  library(ggplot2)
})

# Generate training data from the Friedman 1 benchmark problem
trn <- gen_friedman(500, seed = 101)
X_dframe <- subset(trn, select = -y)
X_matrix <- data.matrix(X_dframe)

# Fit a MARS model to the simulated Friedman benchmark data
mars <- earth::earth(y ~ ., data = trn, degree = 2)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)[, 1L, drop = TRUE]
}

# Generate approximate Shapley values for entire training set
set.seed(102)  # for reproducibility
shap_dframe <- explain(mars, X = X_dframe, pred_wrapper = pfun, nsim = 10)
set.seed(102)  # for reproducibility
shap_matrix <- explain(mars, X = X_matrix, pred_wrapper = pfun, nsim = 10)

# Check dimensions
expect_identical(
  current = dim(shap_dframe),
  target = dim(shap_matrix)
)

# Check plots
grid.arrange(
  autoplot(shap_dframe),
  autoplot(shap_dframe, type = "dependence", X = X_dframe, feature = "x3"),
  autoplot(shap_matrix),
  autoplot(shap_matrix, type = "dependence", X = X_dframe, feature = "x3"),
  nrow = 2
)
