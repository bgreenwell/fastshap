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

# Fit a MARS model to the simulated Friedman benchmark data
mars <- earth::earth(y ~ ., data = trn, degree = 2)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)[, 1L, drop = TRUE]
}

# Generate approximate Shapley values for entire training set
set.seed(102)  # for reproducibility
shap_all <- explain(mars, X = X, pred_wrapper = pfun, nsim = 1)

# Check argument types
expect_error(
  explain(mars, X = X, pred_wrapper = pfun, newdata = data.matrix(X[1L, ]))
)

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
  target = c("tbl_df", "tbl", "data.frame", "explain")
)

# Check Shapley-based variable importance
vi <- apply(shap_all, MARGIN = 2, FUN = function(x) sum(abs(x)))
expect_identical(
  current = sum(vi[6L:10L]),
  target = 0
)

# Generate approximate Shapley values for a single row using first five features
set.seed(103)  # for reproducibility
shap_3 <- explain(mars, feature_names = names(X)[1L:5L], X = X, 
                  pred_wrapper = pfun, nsim = 1, 
                  newdata = X[1L, , drop = FALSE])

# Check dimensions
expect_identical(
  current = dim(shap_3),
  target = c(1L, 5L)
)

# Check approximate Shapley values for a single feature
set.seed(104)
shap_single <- explain(mars, feature_names = "x3", X = X, pred_wrapper = pfun)

# Check dimensions
expect_identical(
  current = dim(shap_single),
  target = c(nrow(X), 1L)
)

# Check column names
expect_identical(
  current = names(shap_single),
  target = "x3"
)

# Check class 
expect_identical(
  current = class(shap_single),
  target = c("tbl_df", "tbl", "data.frame", "explain")
)


# Check Shapley-based importance plot -----------------------------------------

# Construct Shapley-based importance plots
p1 <- autoplot(shap_all)
p2 <- autoplot(shap_all, num_features = 3)

# Expectations
expect_warning(
  autoplot(shap_all, num_features = 0)
)
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
  target = c(3L, 2L)
)

# Check Shapley-based dependence plot -----------------------------------------

# Construct Shapley-based idependence plots
p3 <- autoplot(shap_all, type = "dependence", feature = "x1", X = X, 
               color_by = "x2", smooth = TRUE)
p4 <- autoplot(shap_all, type = "dependence", X = X)

# Expectations
expect_error(  # missing `X`
  autoplot(shap_all, type = "dependence")
)
expect_identical(
  current = class(p3),
  target = c("gg", "ggplot")
)
expect_identical(
  current = dim(p3$data),
  target = c(nrow(X), 3L)
)
expect_identical(
  current = class(p4),
  target = c("gg", "ggplot")
)
expect_identical(
  current = dim(p4$data),
  target = c(nrow(X), 2L)
)
expect_identical(
  current = p4$data$x,
  target = X$x1
)

# Check Shapley-based contribution plot ---------------------------------------

# Construct Shapley-based contribution plots
p5 <- autoplot(shap_all, type = "contribution")
p6 <- autoplot(shap_all, type = "contribution", row_num = 1)

# Expectations
expect_identical(
  current = p5$data,
  target = p6$data
)
expect_identical(
  current = class(p5),
  target = c("gg", "ggplot")
)
expect_identical(
  current = dim(p5$data),
  target = c(ncol(X), 2L)
)

# Inspect plots
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
