# Tests for the batched Monte Carlo core introduced in version 0.2.0

# Generate training data from the Friedman 1 benchmark problem
trn <- fastshap::gen_friedman(200, seed = 2331)
X <- subset(trn, select = -y)
Xm <- data.matrix(X)

fit <- lm(y ~ ., data = trn)
pfun <- function(object, newdata) {
  predict(object, newdata = as.data.frame(newdata))
}

# batch_size must not change seeded results, only the size of each
# prediction call (all randomness is drawn up front)
ex_all <- fastshap::explain(fit, X = Xm, nsim = 8, pred_wrapper = pfun,
                            newdata = Xm[1:7, ], seed = 1454)
ex_chunked <- fastshap::explain(fit, X = Xm, nsim = 8, pred_wrapper = pfun,
                                newdata = Xm[1:7, ], seed = 1454,
                                batch_size = 10)
expect_equal(ex_all, ex_chunked)

# Same check on the data frame path
ex_all_df <- fastshap::explain(fit, X = X, nsim = 8, pred_wrapper = pfun,
                               newdata = X[1:7, ], seed = 1454)
ex_chunked_df <- fastshap::explain(fit, X = X, nsim = 8, pred_wrapper = pfun,
                                   newdata = X[1:7, ], seed = 1454,
                                   batch_size = 10)
expect_equal(ex_all_df, ex_chunked_df)

# Matrix and data frame paths must agree for identical draws (same seed)
expect_equal(unname(unclass(ex_all)), unname(unclass(ex_all_df)))

# Invalid batch_size errors informatively
expect_error(
  fastshap::explain(fit, X = Xm, nsim = 2, pred_wrapper = pfun,
                    batch_size = 0),
  pattern = "batch_size"
)

# Unknown feature names error informatively (previously failed obscurely)
expect_error(
  fastshap::explain(fit, X = Xm, nsim = 2, pred_wrapper = pfun,
                    feature_names = c("x1", "bogus")),
  pattern = "bogus"
)

# A pred_wrapper returning the wrong number of predictions errors informatively
# (e.g., a multiclass wrapper returning a probability matrix)
bad_pfun <- function(object, newdata) {
  p <- predict(object, newdata = as.data.frame(newdata))
  cbind(p, p)  # simulate a two-column probability matrix
}
expect_error(
  fastshap::explain(fit, X = Xm, nsim = 2, pred_wrapper = bad_pfun),
  pattern = "one prediction per row"
)

# Single-feature background data (p = 1) works and keeps its shape
X1 <- X[, "x4", drop = FALSE]
fit1 <- lm(y ~ x4, data = trn)
ex_p1 <- fastshap::explain(fit1, X = X1, nsim = 3, pred_wrapper = function(object, newdata) {
  predict(object, newdata = as.data.frame(newdata))
}, seed = 42)
expect_true(is.matrix(ex_p1))
expect_identical(dim(ex_p1), c(200L, 1L))
expect_identical(colnames(ex_p1), "x4")

# For p = 1 every raw draw is f(x_i) - f(w) with w an actual background row,
# so each implied background prediction must match a prediction on X exactly
p1fun <- function(object, newdata) {
  predict(object, newdata = as.data.frame(newdata))
}
raw_p1 <- fastshap::explain(fit1, X = X1, nsim = 3, pred_wrapper = p1fun,
                            raw = TRUE, seed = 42)
fx1 <- as.numeric(predict(fit1, newdata = trn))
fw_implied <- rep(fx1, times = 3) - as.numeric(raw_p1[, 1L, ])
expect_true(all(vapply(fw_implied, function(v) {
  any(abs(v - fx1) < 1e-8)
}, logical(1))))
