# Tests for explain_effects() (new in 0.2.0)

# Brute-force exact (background-marginal) Shapley values for small p
brute_shap <- function(object, pf, X, x0) {
  p <- ncol(X)
  vapply(seq_len(p), FUN.VALUE = numeric(1), FUN = function(j) {
    others <- setdiff(seq_len(p), j)
    val <- 0
    for (k in 0:(p - 1)) {
      cs <- utils::combn(others, k)
      w <- factorial(k) * factorial(p - k - 1) / factorial(p)
      for (cc in seq_len(ncol(cs))) {
        S <- cs[, cc]
        M1 <- X
        for (v in c(S, j)) M1[[v]] <- x0[[v]]
        M2 <- X
        for (v in S) M2[[v]] <- x0[[v]]
        val <- val + w * mean(pf(object, M1) - pf(object, M2))
      }
    }
    val
  })
}

set.seed(1454)
n <- 100L
X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
pfun <- function(object, newdata) {
  predict(object, newdata = as.data.frame(newdata))
}


################################################################################
# Additive model: main = total = exact Shapley values (independent features)
################################################################################

trn <- cbind(y = 2 * X$x1 - 3 * X$x2 + 0.5 * X$x3, X)
fit_add <- lm(y ~ ., data = trn)

eff <- fastshap::explain_effects(fit_add, X = X, pred_wrapper = pfun)
expect_inherits(eff, "explain_effects")
expect_identical(dim(eff$main), c(n, 3L))
expect_identical(colnames(eff$main), names(X))
expect_identical(names(eff),
                 c("main", "total", "interaction", "shapley_values", "baseline"))

# No interactions: main and total coincide; gap is zero
expect_equal(eff$main, eff$total, tolerance = 1e-8)
expect_true(max(abs(eff$interaction)) < 1e-8)

# Matches exact Linear SHAP (background = training features)
ex_lm <- fastshap::explain(fit_add, X = X, exact = TRUE)
expect_equal(as.numeric(eff$shapley_values), as.numeric(ex_lm),
             tolerance = 1e-8)

# Baseline is the average background prediction
expect_equal(eff$baseline, mean(pfun(fit_add, X)), tolerance = 1e-8)

# print method runs and shows something sensible
expect_stdout(print(eff), pattern = "aseline")


################################################################################
# Pairwise interactions: two-point average is EXACT
################################################################################

trn2 <- cbind(y = 3 * X$x1 * X$x2 + X$x3, X)
fit_int <- lm(y ~ x1:x2 + x3, data = trn2)  # noiseless fit

x0 <- X[1:4, ]
eff2 <- fastshap::explain_effects(fit_int, X = X, pred_wrapper = pfun,
                                  newdata = x0)
brute2 <- t(vapply(seq_len(nrow(x0)), FUN.VALUE = numeric(3),
                   FUN = function(i) {
  brute_shap(fit_int, pf = pfun, X = X, x0 = x0[i, ])
}))
expect_equal(unname(eff2$shapley_values), unname(brute2), tolerance = 1e-8)

# The interaction gap splits equally between the two partners; zero for x3
expect_equal(eff2$interaction[, "x1"], eff2$interaction[, "x2"],
             tolerance = 1e-8)
expect_true(max(abs(eff2$interaction[, "x3"])) < 1e-8)
expect_true(min(abs(eff2$interaction[, "x1"])) > 1e-4)  # really interacting


################################################################################
# Three-way interaction: two-point average is (documented as) biased
################################################################################

pfun3 <- function(object, newdata) {
  nd <- as.data.frame(newdata)
  5 * nd$x1 * nd$x2 * nd$x3
}
eff3 <- fastshap::explain_effects(NULL, X = X, pred_wrapper = pfun3,
                                  newdata = X[1:3, ])
brute3 <- t(vapply(1:3, FUN.VALUE = numeric(3), FUN = function(i) {
  brute_shap(NULL, pf = pfun3, X = X, x0 = X[i, ])
}))
expect_true(max(abs(unname(eff3$shapley_values) - unname(brute3))) > 1e-4)


################################################################################
# Matrix and data frame backgrounds agree (full background is deterministic)
################################################################################

eff_m <- fastshap::explain_effects(fit_add, X = data.matrix(X),
                                   pred_wrapper = pfun)
expect_equal(unname(eff_m$main), unname(eff$main), tolerance = 1e-8)
expect_equal(unname(eff_m$total), unname(eff$total), tolerance = 1e-8)


################################################################################
# Monte Carlo (nsim) path: right shape, close to full background, and
# invariant to batch_size for a fixed seed
################################################################################

eff_mc <- fastshap::explain_effects(fit_add, X = X, pred_wrapper = pfun,
                                    nsim = 500, seed = 3)
expect_identical(dim(eff_mc$main), c(n, 3L))
# nsim = 500 bootstrap draws from a 100-row background carries real sampling
# noise (bootstrapping the mean of 3 coefficients from few unique rows); this
# just checks the MC path lands in the right ballpark, not exact agreement
expect_true(max(abs(eff_mc$main - eff$main)) < 0.3)

eff_mc2 <- fastshap::explain_effects(fit_add, X = X, pred_wrapper = pfun,
                                     nsim = 500, seed = 3, batch_size = 57)
expect_equal(eff_mc, eff_mc2)


################################################################################
# Single-feature background (p = 1): main = total = f(x) - mean(f(X))
################################################################################

X1 <- X[, "x1", drop = FALSE]
pfun1 <- function(object, newdata) {
  2 * as.data.frame(newdata)$x1
}
eff1 <- fastshap::explain_effects(NULL, X = X1, pred_wrapper = pfun1)
expect_equal(as.numeric(eff1$main), 2 * (X$x1 - mean(X$x1)), tolerance = 1e-8)
expect_equal(eff1$main, eff1$total, tolerance = 1e-12)
expect_equal(eff1$main, eff1$shapley_values, tolerance = 1e-12)


################################################################################
# Input validation
################################################################################

expect_error(fastshap::explain_effects(fit_add, pred_wrapper = pfun),
             pattern = "`X`")
expect_error(fastshap::explain_effects(fit_add, X = X),
             pattern = "pred_wrapper")
expect_error(fastshap::explain_effects(fit_add, X = X, pred_wrapper = pfun,
                                       feature_names = "bogus"),
             pattern = "bogus")
