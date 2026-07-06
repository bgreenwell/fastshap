# Tests for the permutation-walk estimator (explain_permutation(), GH #87).
# Called directly via fastshap:::explain_permutation() in this file, since it
# is not yet wired into explain()'s `method` argument.

# Brute-force exact (background-marginal) Shapley values for small p
# (duplicated from test-effects.R; tinytest files here are self-contained)
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
pfun <- function(object, newdata) predict(object, newdata = as.data.frame(newdata))


################################################################################
# Per-replication exact local accuracy (raw = TRUE): for every (obs, rep),
# fx[i] - rowSums(raw[i, , r]) must equal an actual background prediction
# (the one sampled for that walk) -- the estimator's defining, per-walk
# correctness property (not to be confused with the finite-nsim average,
# which still carries ordinary MC noise).
################################################################################

for (p in c(1L, 3L)) {
  Xp <- X[, seq_len(p), drop = FALSE]
  trn <- cbind(y = as.numeric(as.matrix(Xp) %*% seq_len(p)), Xp)
  fit <- lm(y ~ ., data = trn)
  nsim <- 20L
  set.seed(11)
  raw <- fastshap:::explain_permutation(fit, X = Xp, nsim = nsim,
                                        pred_wrapper = pfun, newdata = Xp,
                                        raw = TRUE)
  expect_identical(dim(raw), c(n, p, nsim))

  fx <- pfun(fit, Xp)
  fw_all <- pfun(fit, Xp)  # background predictions == predictions on X itself
  ok <- vapply(seq_len(n), function(i) {
    all(vapply(seq_len(nsim), function(r) {
      implied_fw <- fx[i] - sum(raw[i, , r])
      any(abs(implied_fw - fw_all) < 1e-8)
    }, logical(1)))
  }, logical(1))
  expect_true(all(ok))
}


################################################################################
# Brute-force exact-Shapley agreement (small p, large nsim)
################################################################################

# Additive (no interaction)
trn_add <- cbind(y = 2 * X$x1 - 3 * X$x2 + 0.5 * X$x3, X)
fit_add <- lm(y ~ ., data = trn_add)
x0 <- X[1:5, ]
set.seed(22)
perm_add <- fastshap:::explain_permutation(fit_add, X = X, nsim = 2000,
                                           pred_wrapper = pfun, newdata = x0)
brute_add <- t(vapply(seq_len(nrow(x0)), FUN.VALUE = numeric(3), FUN = function(i) {
  brute_shap(fit_add, pf = pfun, X = X, x0 = x0[i, ])
}))
expect_true(cor(as.numeric(perm_add), as.numeric(brute_add)) > 0.999)

# Pairwise interaction
trn_int <- cbind(y = 3 * X$x1 * X$x2 + X$x3, X)
fit_int <- lm(y ~ x1:x2 + x3, data = trn_int)
set.seed(23)
perm_int <- fastshap:::explain_permutation(fit_int, X = X, nsim = 2000,
                                           pred_wrapper = pfun, newdata = x0)
brute_int <- t(vapply(seq_len(nrow(x0)), FUN.VALUE = numeric(3), FUN = function(i) {
  brute_shap(fit_int, pf = pfun, X = X, x0 = x0[i, ])
}))
expect_true(cor(as.numeric(perm_int), as.numeric(brute_int)) > 0.999)


################################################################################
# batch_size invariance (all randomness drawn up front)
################################################################################

set.seed(33)
ex_all <- fastshap:::explain_permutation(fit_add, X = X, nsim = 16,
                                         pred_wrapper = pfun, newdata = x0)
set.seed(33)
ex_chunked <- fastshap:::explain_permutation(fit_add, X = X, nsim = 16,
                                             pred_wrapper = pfun, newdata = x0,
                                             batch_size = 7)
expect_equal(ex_all, ex_chunked)

set.seed(33)
raw_all <- fastshap:::explain_permutation(fit_add, X = X, nsim = 16,
                                          pred_wrapper = pfun, newdata = x0,
                                          raw = TRUE)
set.seed(33)
raw_chunked <- fastshap:::explain_permutation(fit_add, X = X, nsim = 16,
                                              pred_wrapper = pfun, newdata = x0,
                                              raw = TRUE, batch_size = 7)
expect_equal(raw_all, raw_chunked)


################################################################################
# Matrix / data-frame parity
################################################################################

Xm <- data.matrix(X)
x0m <- data.matrix(x0)
set.seed(44)
ex_df <- fastshap:::explain_permutation(fit_add, X = X, nsim = 10,
                                        pred_wrapper = pfun, newdata = x0)
set.seed(44)
ex_mat <- fastshap:::explain_permutation(fit_add, X = Xm, nsim = 10,
                                         pred_wrapper = pfun, newdata = x0m)
expect_equal(unname(ex_df), unname(ex_mat))


################################################################################
# raw = TRUE: shape and mean-equals-averaged-output
################################################################################

set.seed(55)
raw3 <- fastshap:::explain_permutation(fit_add, X = X, nsim = 12,
                                       pred_wrapper = pfun, newdata = x0,
                                       raw = TRUE)
set.seed(55)
mean3 <- fastshap:::explain_permutation(fit_add, X = X, nsim = 12,
                                        pred_wrapper = pfun, newdata = x0)
expect_identical(dim(raw3), c(nrow(x0), 3L, 12L))
expect_equal(apply(raw3, 1:2, mean), mean3)


################################################################################
# p = 1 edge case
################################################################################

X1 <- X[, "x1", drop = FALSE]
fit1 <- lm(y ~ x1, data = cbind(y = 2 * X$x1, X1))
set.seed(66)
ex1 <- fastshap:::explain_permutation(fit1, X = X1, nsim = 5,
                                      pred_wrapper = pfun, newdata = X1)
expect_true(is.matrix(ex1))
expect_identical(dim(ex1), c(n, 1L))


################################################################################
# Statistical cross-validation against the default (Strumbelj) estimator
################################################################################

set.seed(77)
ex_default <- explain(fit_add, X = X, pred_wrapper = pfun, newdata = x0, nsim = 2000)
set.seed(78)
ex_perm <- fastshap:::explain_permutation(fit_add, X = X, nsim = 2000,
                                          pred_wrapper = pfun, newdata = x0)
expect_true(cor(as.numeric(ex_default), as.numeric(ex_perm)) > 0.999)


################################################################################
# Endpoint-shortcut call-count check: exactly p genuine pred_wrapper calls per
# chunk (not p + 1), since step 0 uses X[w_idx,] directly (still one call) and
# step p reuses the cached fx (zero additional calls) -- so total calls per
# chunk = 1 (step 0) + (p - 1) (steps 1..p-1) + 0 (step p, cached) = p, plus
# the one up-front fx call shared across all chunks.
################################################################################

call_count <- 0L
counting_pfun <- function(object, newdata) {
  call_count <<- call_count + 1L
  predict(object, newdata = as.data.frame(newdata))
}
p <- 4L
Xp <- X[, rep("x1", p), drop = FALSE]
names(Xp) <- paste0("x", seq_len(p))
Xp[] <- lapply(Xp, function(col) runif(n))
trn_p <- cbind(y = rowSums(Xp), Xp)
fit_p <- lm(y ~ ., data = trn_p)

call_count <- 0L
invisible(fastshap:::explain_permutation(fit_p, X = Xp, nsim = 10,
                                         pred_wrapper = counting_pfun,
                                         newdata = Xp[1:5, ]))
# 1 (up-front fx) + [1 (step 0) + (p - 1) (steps 1..p-1)] for the single chunk
expect_equal(call_count, 1L + p)


################################################################################
# Wired into explain(): method = "permutation" via the public API
################################################################################

p10 <- 10L
Xw <- as.data.frame(matrix(runif(n * p10), n, p10))
names(Xw) <- paste0("x", seq_len(p10))
trn_w <- cbind(y = as.numeric(as.matrix(Xw) %*% seq_len(p10)), Xw)
fit_w <- lm(y ~ ., data = trn_w)
pfun_w <- function(object, newdata) predict(object, newdata = as.data.frame(newdata))

# Basic call works and matches the shape/attributes of the strumbelj path
set.seed(91)
ex_perm_full <- explain(fit_w, X = Xw, pred_wrapper = pfun_w, nsim = 50,
                        method = "permutation")
expect_true(is.matrix(ex_perm_full))
expect_identical(dim(ex_perm_full), c(n, p10))
expect_identical(colnames(ex_perm_full), names(Xw))
expect_identical(class(ex_perm_full), c("explain", "matrix", "array"))
expect_true(!is.null(attr(ex_perm_full, "baseline")))  # NA_real_, but present
expect_true(is.na(attr(ex_perm_full, "baseline")))     # no baseline without adjust

# raw = TRUE through the public API
set.seed(92)
raw_pub <- explain(fit_w, X = Xw, pred_wrapper = pfun_w, nsim = 8,
                   method = "permutation", raw = TRUE)
expect_identical(dim(raw_pub), c(n, p10, 8L))
expect_identical(dimnames(raw_pub)[[2L]], names(Xw))

################################################################################
# feature_names cost-model test: strumbelj scales with length(feature_names);
# permutation does not (must always walk all p features internally).
################################################################################

count_rows <- 0L
row_counting_pfun <- function(object, newdata) {
  count_rows <<- count_rows + NROW(newdata)
  predict(object, newdata = as.data.frame(newdata))
}

count_rows <- 0L
invisible(explain(fit_w, X = Xw, pred_wrapper = row_counting_pfun, nsim = 20,
                  method = "strumbelj", feature_names = names(Xw)))
rows_all_strumbelj <- count_rows

count_rows <- 0L
invisible(explain(fit_w, X = Xw, pred_wrapper = row_counting_pfun, nsim = 20,
                  method = "strumbelj", feature_names = names(Xw)[1:2]))
rows_subset_strumbelj <- count_rows
# Requesting 2 of 10 features should cost roughly 2/10 of requesting all 10
expect_true(rows_subset_strumbelj < rows_all_strumbelj * 0.3)

count_rows <- 0L
invisible(explain(fit_w, X = Xw, pred_wrapper = row_counting_pfun, nsim = 20,
                  method = "permutation", feature_names = names(Xw)))
rows_all_perm <- count_rows

count_rows <- 0L
invisible(explain(fit_w, X = Xw, pred_wrapper = row_counting_pfun, nsim = 20,
                  method = "permutation", feature_names = names(Xw)[1:2]))
rows_subset_perm <- count_rows
# Under method = "permutation", subsetting feature_names does NOT reduce work
expect_equal(rows_subset_perm, rows_all_perm)


################################################################################
# Input validation
################################################################################

expect_error(
  explain(fit_w, X = Xw, pred_wrapper = pfun_w, nsim = 5,
         method = "permutation", parallel = TRUE),
  pattern = "parallel"
)
expect_error(
  explain(fit_w, X = Xw, pred_wrapper = pfun_w, nsim = 5,
         method = "permutation", adjust = TRUE),
  pattern = "adjust"
)
expect_error(
  explain(fit_w, X = Xw, pred_wrapper = pfun_w, nsim = 5, method = "bogus")
)


################################################################################
# Dispatch through explain.lm()'s `...` forwarding (no signature changes there)
################################################################################

set.seed(93)
ex_lm_perm <- explain(fit_w, X = Xw, pred_wrapper = pfun_w, nsim = 30,
                      method = "permutation")
expect_true(is.matrix(ex_lm_perm))
expect_identical(dim(ex_lm_perm), c(n, p10))
