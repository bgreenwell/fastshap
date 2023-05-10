exit_if_not(requireNamespace("xgboost", quietly = TRUE))

# Generate training data from the Friedman 1 benchmark problem
trn <- fastshap::gen_friedman(500, seed = 101)
X <- subset(trn, select = -y)
x <- X[1L, , drop = FALSE]

# Fit a linear model to the simulated Friedman benchmark data
fit_lm <- lm(y ~ ., data = trn)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)
}

# Generate exact and approximate Shapley values for entire training set
ex_exact <- fastshap::explain(fit_lm, exact = TRUE, newdata = x)
set.seed(102)
ex_apprx <- fastshap::explain(fit_lm, X = X, pred_wrapper = pfun, nsim = 1000,
                              newdata = x, adjust = TRUE)

# Check accuracy
expect_true(cor(as.numeric(ex_exact), as.numeric((ex_apprx))) > 0.999)

# Check dimensions
expect_identical(
  current = dim(ex_exact),
  target = dim(ex_apprx)
)

# Check column names
expect_identical(
  current = colnames(ex_exact),
  target = names(X)
)

# Check class 
expect_identical(
  current = class(ex_exact),
  target = c("explain", "matrix", "array")
)

# Fit model(s)
set.seed(111)
fit_xgb <- xgboost::xgboost(
  data = data.matrix(subset(trn, select = -y)),
  label = trn$y,
  max_depth = 3,
  eta = 0.1,
  nrounds = 301,
  verbose = 0
)

# Generate exact and approximate Shapley values for entire training set
x <- data.matrix(X)[1L, , drop = FALSE]
ex_exact <- fastshap::explain(fit_xgb, X = x, exact = TRUE)
set.seed(132)
ex_apprx <- fastshap::explain(fit_xgb, X = data.matrix(X), newdata = x, 
                              adjust = TRUE, pred_wrapper = pfun, nsim = 1000)

# Check accuracy
expect_true(cor(as.numeric(ex_exact), as.numeric((ex_apprx))) > 0.999)

# Check dimensions
expect_identical(
  current = dim(ex_exact),
  target = dim(ex_apprx)
)

# Check column names
expect_identical(
  current = colnames(ex_exact),
  target = colnames(X)
)

# Check class 
expect_identical(
  current = class(ex_exact),
  target = c("matrix", "array")
)
