#' Main/total effect contributions for a single feature
#'
#' Unlike `explain_feature()`, which averages over *random* coalitions, this
#' averages over exactly two deterministic coalitions per background draw:
#' the empty coalition (only feature `col` comes from the instance being
#' explained) and the full coalition (every feature except `col` does). Both
#' require the same "Frankenstein" construction used by the Monte Carlo
#' estimator, just with a fixed (rather than randomly permuted) `O` matrix.
#'
#' @return A list with components `main` and `total`, each a numeric vector
#' of length `nrow(newdata)` giving `E_w[f(w <- x_col)]` and
#' `E_w[f(x <- w_col)]`, respectively (see `explain_effects()` for the
#' definitions in terms of these two quantities).
#'
#' @keywords internal
#'
#' @noRd
explain_effects_feature <- function(object, col, X, K, pred_wrapper, newdata,
                                    full, batch_size = NULL) {
  n <- nrow(newdata)
  p <- ncol(X)
  reps_per_chunk <- if (is.null(batch_size)) {
    K
  } else {
    max(1L, min(K, batch_size %/% n))
  }
  chunks <- split(seq_len(K), ceiling(seq_len(K) / reps_per_chunk))
  # When background rows are sampled (not the full background), draw all of
  # the randomness up front so that results depend only on the random seed,
  # not on the chunking induced by `batch_size`
  w_idx_all <- if (full) NULL else sample(nrow(X), size = n * K, replace = TRUE)
  main_sum <- numeric(n)
  total_sum <- numeric(n)
  for (chunk in chunks) {
    k <- length(chunk)
    N <- n * k
    Xrep <- newdata[rep(seq_len(n), times = k), , drop = FALSE]
    w_idx <- if (full) {
      rep(chunk, each = n)  # background row `chunk[r]` for every observation
    } else {
      idx <- ((chunk[1L] - 1L) * n + 1L):(chunk[length(chunk)] * n)
      w_idx_all[idx]
    }
    # Empty coalition: every feature from the background except `col`
    O_empty <- matrix(FALSE, N, p)
    O_empty[, col] <- TRUE
    # Full coalition: every feature from the instance being explained
    O_full <- matrix(TRUE, N, p)
    if (is.matrix(X)) {
      B_bg <- frankenstein_matrix(Xrep, X = X, w_idx = w_idx, O = O_empty, col = col)[[1L]]
      B_tot <- frankenstein_matrix(Xrep, X = X, w_idx = w_idx, O = O_full, col = col)[[2L]]
    } else {
      B_bg <- frankenstein_df(Xrep, X = X, w_idx = w_idx, O = O_empty, col = col)[[1L]]
      B_tot <- frankenstein_df(Xrep, X = X, w_idx = w_idx, O = O_full, col = col)[[2L]]
    }
    pred_bg <- as.numeric(pred_wrapper(object, newdata = B_bg))
    pred_tot <- as.numeric(pred_wrapper(object, newdata = B_tot))
    if (length(pred_bg) != N || length(pred_tot) != N) {
      stop("`pred_wrapper` must return a numeric vector with one prediction ",
           "per row of its `newdata` argument.", call. = FALSE)
    }
    main_sum <- main_sum + rowSums(matrix(pred_bg, nrow = n, ncol = k))
    total_sum <- total_sum + rowSums(matrix(pred_tot, nrow = n, ncol = k))
  }
  list(main = main_sum / K, total = total_sum / K)
}


#' Main, total, and interaction effect decomposition
#'
#' For each feature, decompose its contribution to a prediction into a *main
#' effect* (the feature's effect in isolation), a *total effect* (the
#' feature's effect together with every interaction it participates in), and
#' the gap between them (an *interaction* diagnostic). Unlike [explain()],
#' which estimates Shapley values by averaging over *randomly sampled*
#' coalitions, `explain_effects()` uses only the two coalitions with the
#' highest Shapley weight — the empty coalition and the full coalition — so
#' by default (`nsim = NULL`) it is deterministic given the background data,
#' with no Monte Carlo simulation at all.
#'
#' @inheritParams explain
#'
#' @param nsim Optional positive integer giving the number of background rows
#' to sample per observation. Default (`NULL`) averages over *every* row of
#' `X` (deterministic, exact given the background data, but requires
#' `2 * nrow(newdata) * length(feature_names) * nrow(X)` predictions); set
#' `nsim` to average over a random sample of that many background rows
#' instead (requires `seed` for reproducibility).
#'
#' @details
#' For feature \eqn{j} and observation \eqn{x}, averaging over background
#' draws \eqn{w}:
#'
#' \describe{
#'   \item{`main`}{`E_w[f(x_j, w_{-j})] - E_w[f(w)]`. Since no other feature
#'   of `x` is present, this is free of interaction effects — it is exactly
#'   the centered *partial dependence* of feature \eqn{j} evaluated at
#'   \eqn{x_j}.}
#'   \item{`total`}{`f(x) - E_w[f(w_j, x_{-j})]`. This captures the main
#'   effect of feature \eqn{j} together with every interaction it
#'   participates in, in the context of \eqn{x}.}
#'   \item{`interaction`}{`total - main`. Zero exactly when feature \eqn{j}
#'   enters the model additively (no interactions involving \eqn{j}) at
#'   \eqn{x}; for a two-way interaction the gap splits equally between the
#'   two participating features.}
#'   \item{`shapley_values`}{`(main + total) / 2`. This equals the exact
#'   (background-marginal) Shapley value for any model with at most pairwise
#'   interactions; for higher-order interactions it is a cheap but biased
#'   approximation.}
#' }
#'
#' As with [explain()], this uses marginal (interventional) sampling of the
#' background, so the usual caveat applies: with correlated features, the
#' constructed hybrid observations can be unrealistic combinations of feature
#' values.
#'
#' @return An object of class `"explain_effects"`: a list with components
#' `main`, `total`, and `interaction` (each an `nrow(newdata)` x
#' `length(feature_names)` matrix), `shapley_values` (same shape), and
#' `baseline` (the average prediction over `X`).
#'
#' @references
#' Strumbelj, E., and Igor K. (2014). Explaining prediction models and
#' individual predictions with feature contributions. Knowledge and information
#' systems, 41(3), 647-665.
#'
#' @export
#'
#' @examples
#' data(mtcars)
#' fit <- ppr(mpg ~ ., data = mtcars, nterms = 5)
#' pfun <- function(object, newdata) predict(object, newdata = newdata)
#' eff <- explain_effects(fit, X = subset(mtcars, select = -mpg),
#'                        pred_wrapper = pfun)
#' eff
explain_effects <- function(object, feature_names = NULL, X = NULL,
                            pred_wrapper = NULL, newdata = NULL, nsim = NULL,
                            batch_size = NULL, seed = NULL, ...) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (is.null(X)) {
    stop("Background data required for `explain_effects()`. Please supply ",
         "it via the `X` argument.", call. = FALSE)
  }
  if (is.null(pred_wrapper)) {
    stop("Prediction function required for `explain_effects()`. Please ",
         "supply one via the `pred_wrapper` argument.", call. = FALSE)
  }
  if (!is.null(nsim) && (!is.numeric(nsim) || length(nsim) != 1L || nsim < 1)) {
    stop("`nsim` must be a single positive integer, or `NULL` for full-",
         "background averaging.", call. = FALSE)
  }
  if (!is.null(batch_size) && (!is.numeric(batch_size) ||
      length(batch_size) != 1L || batch_size < 1)) {
    stop("`batch_size` must be a single positive integer.", call. = FALSE)
  }

  if (inherits(X, what = "tbl_df")) {
    X <- as.data.frame(X)
  }
  if (inherits(newdata, what = "tbl_df")) {
    newdata <- as.data.frame(newdata)
  }
  if (is.null(newdata)) {
    newdata <- X
  }
  if (!identical(class(X), class(newdata))) {
    stop("Arguments `X` and `newdata` do not inherit from the same class: ",
         "\n   * `X` inherits from class \"", class(X), "\"\n   * `newdata` ",
         "inherits from class \"", class(newdata), "\"", call. = FALSE)
  }

  if (is.null(feature_names)) {
    feature_names <- colnames(X)
  }
  cols <- match(feature_names, colnames(X))
  if (anyNA(cols)) {
    stop("Some components of `feature_names` were not found in `X`: ",
         paste(feature_names[is.na(cols)], collapse = ", "), call. = FALSE)
  }

  n <- nrow(newdata)
  nfeat <- length(cols)
  full <- is.null(nsim)
  K <- if (full) nrow(X) else nsim

  baseline <- mean(as.numeric(pred_wrapper(object, newdata = X)))
  fx <- as.numeric(pred_wrapper(object, newdata = newdata))

  main <- matrix(NA_real_, nrow = n, ncol = nfeat)
  total <- matrix(NA_real_, nrow = n, ncol = nfeat)
  for (j in seq_len(nfeat)) {
    res <- explain_effects_feature(object, col = cols[j], X = X, K = K,
                                   pred_wrapper = pred_wrapper,
                                   newdata = newdata, full = full,
                                   batch_size = batch_size)
    main[, j] <- res$main - baseline
    total[, j] <- fx - res$total
  }

  interaction <- total - main
  shapley_values <- (main + total) / 2

  colnames(main) <- colnames(total) <- colnames(interaction) <-
    colnames(shapley_values) <- feature_names
  rownames(main) <- rownames(total) <- rownames(interaction) <-
    rownames(shapley_values) <- rownames(newdata)

  structure(list(
    main = main,
    total = total,
    interaction = interaction,
    shapley_values = shapley_values,
    baseline = baseline
  ), class = "explain_effects")

}


#' @rdname explain_effects
#'
#' @param x An object of class `"explain_effects"`.
#'
#' @export
print.explain_effects <- function(x, ...) {
  cat("Feature effect decomposition (fastshap::explain_effects)\n\n")
  cat("Baseline:", format(x$baseline), "\n\n")
  cat("Shapley values (main + total) / 2, first few rows:\n")
  print(utils::head(x$shapley_values))
  invisible(x)
}
