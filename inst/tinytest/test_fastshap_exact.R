# Exits
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  exit_file("Package ggplot2 missing")
}
if (!requireNamespace("xgboost", quietly = TRUE)) {
  exit_file("Package xgboost missing")
}

# Load required packages
suppressMessages({
  library(ggplot2)
  # library(xgboost)
})

# Generate training data from the Friedman 1 benchmark problem
trn <- gen_friedman(500, seed = 101)
X <- subset(trn, select = -y)

# Fit a MARS model to the simulated Friedman benchmark data
lin <- lm(y ~ ., data = trn)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)
}

# Generate exact and approximate Shapley values for entire training set
lin_shap <- explain(lin, exact = TRUE)
set.seed(102)
app_shap <- explain(lin, X = X, pred_wrapper = pfun, nsim = 10)

# Check dimensions
expect_identical(
  current = dim(lin_shap),
  target = dim(app_shap)
)

# Check column names
expect_identical(
  current = names(lin_shap),
  target = names(X)
)

# Check class 
expect_identical(
  current = class(lin_shap),
  target = c("tbl_df", "tbl", "data.frame", "explain")
)

# Fit model(s)
set.seed(111)
bst <- xgboost::xgboost(  # params found using `autoxgb::autoxgb()`
  data = data.matrix(subset(trn, select = -y)),
  label = trn$y,
  max_depth = 3,
  eta = 0.1,
  nrounds = 301,
  verbose = 0
)

# Generate exact and approximate Shapley values for entire training set
X <- data.matrix(X)
tree_shap <- explain(bst, X = X, exact = TRUE)
set.seed(132)
app_shap <- explain(bst, X = X, pred_wrapper = pfun, nsim = 10)

# Check dimensions
expect_identical(
  current = dim(tree_shap),
  target = dim(app_shap)
)

# Check column names
expect_identical(
  current = names(tree_shap),
  target = colnames(X)
)

# Check class 
expect_identical(
  current = class(tree_shap),
  target = c("tbl_df", "tbl", "data.frame", "explain")
)
