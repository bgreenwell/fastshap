# Load required packages
library(earth)    # for fitting MARS models
library(ggplot2)
library(mlbench)  # for ML benchmark data sets

# Generate training data from the Friedman 1 benchmark problem
set.seed(101)  # for reproducibility
trn <- as.data.frame(mlbench.friedman1(500))
X_dframe <- subset(trn, select = -y)
X_matrix <- data.matrix(X_dframe)

# Fit a MARS model to the simulated Friedman benchmark data
mars <- earth(y ~ ., data = trn, degree = 2)

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
  autoplot(shap_dframe, type = "dependence", X = X_dframe, feature = "x.3"),
  autoplot(shap_matrix),
  autoplot(shap_matrix, type = "dependence", X = X_dframe, feature = "x.3"),
  nrow = 2
)
