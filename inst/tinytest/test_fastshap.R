# Load required packages
library(earth)    # for fitting MARS models
library(ggplot2)  # for autoplot() generic
library(mlbench)  # for ML benchmark data sets

# Generate training data from the Friedman 1 benchmark problem
set.seed(101)  # for reproducibility
trn <- as.data.frame(mlbench.friedman1(500))
X <- subset(trn, select = -y)

# Fit a MARS model to the simulated Friedman benchmark data
mars <- earth(y ~ ., data = trn, degree = 2)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)[, 1L, drop = TRUE]
}

# Generate approximate Shapley values for entire training set
set.seed(102)  # for reproducibility
shap_all <- fastshap(mars, X = X, pred_wrapper = pfun, nsim = 1)

# Check dimensions
expect_identical(
  current = dim(shap_all),
  target = dim(X)
)

# Check column names
expect_identical(
  current = names(shap_all),
  target = names(X)
)

# Check class 
expect_identical(
  current = class(shap_all),
  target = c("tbl_df", "tbl", "data.frame", "fastshap")
)

# Check Shapley-based variable importance
vi <- apply(shap_all, MARGIN = 2, FUN = function(x) sum(abs(x)))
expect_identical(
  current = sum(vi[6L:10L]),
  target = 0
)

# Generate approximate Shapley values for a single row using first five features
set.seed(103)  # for reproducibility
shap_3 <- fastshap(mars, feature_names = names(X)[1L:5L], X = X, 
                   pred_wrapper = pfun, nsim = 1, 
                   newdata = X[1L, , drop = FALSE])

# Check dimensions
expect_identical(
  current = dim(shap_3),
  target = c(1L, 5L)
)

# Check Shapley-based importance plot and Shapley-based dependence plot
p1 <- autoplot(shap_all)
p2 <- autoplot(shap_all, type = "dependence", feature = "x.3", X = X)

# Check plots
expect_identical(
  current = class(p1),
  target = c("gg", "ggplot")
)
expect_identical(
  current = dim(p1$data),
  target = c(ncol(X), 2L)
)
expect_identical(
  current = class(p2),
  target = c("gg", "ggplot")
)
expect_identical(
  current = dim(p2$data),
  target = c(nrow(X), 2L)
)

# Inspect plots
p1
p2
