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
  x = data.matrix(subset(trn, select = -y)),
  y = trn$y,
  max_depth = 3,
  learning_rate = 0.1,
  nrounds = 301,
  verbosity = 0L
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


################################################################################
# Regression tests: exact = TRUE, shap_only = FALSE (GH audit)
################################################################################

# lm: shap_only=FALSE with newdata=NULL should include "baseline" component
ex_lm_list <- fastshap::explain(fit_lm, X = X, exact = TRUE, shap_only = FALSE)
expect_true(is.list(ex_lm_list))
expect_true(all(c("shapley_values", "feature_values", "baseline") %in% names(ex_lm_list)))
expect_true(is.numeric(ex_lm_list$baseline))

# lm: shap_only=FALSE with explicit newdata
ex_lm_list2 <- fastshap::explain(fit_lm, X = X, newdata = X[1:3, ],
                                  exact = TRUE, shap_only = FALSE)
expect_equal(nrow(ex_lm_list2$shapley_values), 3L)

# xgb: shap_only=FALSE when data supplied via X (newdata=NULL)
ex_xgb_list <- fastshap::explain(fit_xgb, X = data.matrix(X[1:5, ]),
                                  exact = TRUE, shap_only = FALSE)
expect_true(is.list(ex_xgb_list))
expect_true(all(c("shapley_values", "feature_values", "baseline") %in% names(ex_xgb_list)))
expect_equal(nrow(ex_xgb_list$shapley_values), 5L)
expect_equal(ncol(ex_xgb_list$shapley_values), ncol(X))  # no BIAS column

# exact = TRUE warning for unsupported model type
fit_ppr <- ppr(y ~ ., data = trn, nterms = 5)
expect_warning(
  fastshap::explain(fit_ppr, X = X, pred_wrapper = pfun, nsim = 5, exact = TRUE),
  pattern = "only supported"
)
