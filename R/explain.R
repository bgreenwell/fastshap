#' Generate permutation-order matrix
#'
#' Build the full logical matrix describing, for each Monte Carlo draw, which
#' features appear before the feature of interest in the associated random
#' permutation (a `TRUE` means the corresponding feature value comes from the
#' instance being explained; a `FALSE` means it comes from the background
#' draw). Column `col` is always `TRUE`.
#'
#' @keywords internal
#'
#' @noRd
gen_perm_matrix <- function(n, p, col) {
  O <- matrix(FALSE, nrow = n, ncol = p)
  O[, col] <- TRUE
  if (p > 1L) {
    O[, -col] <- genOMat(n, p)
  }
  O
}


#' Construct "Frankenstein" matrices (data frame method)
#'
#' Column-wise construction of the two hybrid data sets whose prediction
#' difference estimates the marginal contribution of feature `col`. Each
#' column starts from the background draws (so it inherits the class, and for
#' factors the levels, of the corresponding column of `X`) and the values of
#' the instances being explained are inserted where `O` is `TRUE`.
#'
#' @keywords internal
#'
#' @noRd
frankenstein_df <- function(Xrep, X, w_idx, O, col) {
  B1 <- Xrep
  for (j in seq_along(B1)) {
    b <- X[[j]][w_idx]  # background draws (X's column class/levels)
    keep <- O[, j]
    b[keep] <- Xrep[[j]][keep]
    B1[[j]] <- b
  }
  # B2 differs from B1 in the feature-of-interest column only
  B2 <- B1
  B2[[col]] <- X[[col]][w_idx]
  list(B1, B2)
}


#' Construct "Frankenstein" matrices (matrix method)
#'
#' @keywords internal
#'
#' @noRd
frankenstein_matrix <- function(Xrep, X, w_idx, O, col) {
  W <- X[w_idx, , drop = FALSE]
  B <- genFrankensteinMatrices(Xrep, W, O, feature = col)
  colnames(B[[1L]]) <- colnames(B[[2L]]) <- colnames(X)
  B
}


#' Monte Carlo differences for a single feature
#'
#' Estimate all `nsim` Monte Carlo replications of the marginal-contribution
#' differences for one feature in a small number of (large) `pred_wrapper()`
#' calls: the replications are stacked row-wise so each chunk requires only
#' two prediction calls, which is substantially faster than one pair of calls
#' per replication for most models.
#'
#' @return An `nrow(newdata)` x `nsim` matrix of prediction differences.
#'
#' @keywords internal
#'
#' @noRd
explain_feature <- function(object, col, X, nsim, pred_wrapper, newdata,
                            batch_size = NULL) {
  n <- nrow(newdata)
  p <- ncol(X)
  sims_per_chunk <- if (is.null(batch_size)) {
    nsim
  } else {
    max(1L, min(nsim, batch_size %/% n))
  }
  chunks <- split(seq_len(nsim), ceiling(seq_len(nsim) / sims_per_chunk))
  # Draw all of the randomness up front so that results depend only on the
  # random seed, not on the chunking induced by `batch_size` (which only
  # bounds the size of the constructed data sets and prediction calls)
  w_idx <- sample(nrow(X), size = n * nsim, replace = TRUE)
  O <- gen_perm_matrix(n * nsim, p, col = col)
  D <- matrix(NA_real_, nrow = n, ncol = nsim)
  for (chunk in chunks) {
    N <- n * length(chunk)
    idx <- ((chunk[1L] - 1L) * n + 1L):(chunk[length(chunk)] * n)
    Xrep <- newdata[rep(seq_len(n), times = length(chunk)), , drop = FALSE]
    B <- if (is.matrix(X)) {
      frankenstein_matrix(Xrep, X = X, w_idx = w_idx[idx],
                          O = O[idx, , drop = FALSE], col = col)
    } else {
      frankenstein_df(Xrep, X = X, w_idx = w_idx[idx],
                      O = O[idx, , drop = FALSE], col = col)
    }
    p1 <- pred_wrapper(object, newdata = B[[1L]])
    p2 <- pred_wrapper(object, newdata = B[[2L]])
    if (length(p1) != N || length(p2) != N) {
      stop("`pred_wrapper` must return a numeric vector with one prediction ",
           "per row of its `newdata` argument (expected length ", N,
           ", got lengths ", length(p1), " and ", length(p2), "). For ",
           "multiclass problems, return the predicted probability/score for ",
           "a single reference class.", call. = FALSE)
    }
    D[, chunk] <- as.numeric(p1) - as.numeric(p2)
  }
  D
}


#' Fast approximate Shapley values
#'
#' Compute fast (approximate) Shapley values for a set of features using the
#' Monte Carlo algorithm described in Strumbelj and Igor (2014). An efficient
#' algorithm for tree-based models, commonly referred to as Tree SHAP, is also
#' supported for [lightgbm](https://cran.r-project.org/package=lightgbm) and
#' [xgboost](https://cran.r-project.org/package=xgboost) models; see Lundberg
#' et. al. (2020) for details.
#'
#' @param object A fitted model object (e.g., a [ranger::ranger()],
#' [xgboost::xgboost()], or [earth::earth()] object, to name
#' a few).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest. If `NULL` (default) they will be
#' taken from the column names of `X`.
#'
#' @param X A matrix-like R object (e.g., a data frame or matrix) containing
#' ONLY the feature columns from the training data (or suitable background data
#' set). **NOTE:** This argument is required whenever `exact = FALSE`.
#'
#' @param pred_wrapper Prediction function that requires two arguments,
#' `object` and `newdata`. **NOTE:** This argument is required
#' whenever `exact = FALSE`. The output of this function should be
#' determined according to:
#'
#' \describe{
#'   \item{Regression}{A numeric vector of predicted outcomes.}
#'   \item{Binary classification}{A vector of predicted class probabilities
#'   for the reference class.}
#'   \item{Multiclass classification}{A vector of predicted class probabilities
#'   for the reference class.}
#' }
#'
#' @param nsim The number of Monte Carlo repetitions to use for estimating each
#' Shapley value (only used when `exact = FALSE`). Default is 1.
#' **NOTE:** To obtain the most accurate results, `nsim` should be set
#' as large as feasibly possible.
#'
#' @param newdata A matrix-like R object (e.g., a data frame or matrix)
#' containing ONLY the feature columns for the observation(s) of interest; that
#' is, the observation(s) you want to compute explanations for. Default is
#' `NULL` which will produce approximate Shapley values for all the rows in
#' `X` (i.e., the training data).
#'
#' @param adjust Logical indicating whether or not to adjust the sum of the
#' estimated Shapley values to satisfy the *local accuracy* property; that is,
#' to equal the difference between the model's prediction for that sample and
#' the average prediction over all the training data (i.e., `X`). Default is
#' `FALSE` and setting to `TRUE` requires `nsim` > 1.
#'
#' @param exact Logical indicating whether to compute exact Shapley values.
#' Currently only supported for [stats::lm()], [xgboost::xgboost()], and
#' [lightgbm::lightgbm()] objects (binary/regression only — multiclass is not
#' yet supported). Passing `exact = TRUE` for any other model type issues a
#' warning and falls back to the Monte Carlo approximation. Note that
#' `exact = TRUE` for [stats::lm()] returns explanations for each of the
#' [stats::terms()] in the model. Default is `FALSE`.
#'
#' @param baseline Numeric baseline to use when adjusting the computed Shapley
#' values to achieve *local accuracy*. Adjusted Shapley values for a single
#' prediction (`fx`) will sum to the difference `fx - baseline`. Defaults to
#' `NULL`, which corresponds to the average predictions computed from `X`, and
#' zero otherwise (i.e., no additional predictions will be computed and the
#' baseline attribute of the output will be set to zero).
#'
#' @param shap_only Logical indicating whether or not to include additional
#' output useful for plotting (i.e., `newdata` and the `baseline` value.). This
#' is convenient, for example, when using [shapviz::shapviz()] for plotting.
#' Default is `TRUE`.
#'
#' @param parallel Logical indicating whether or not to compute the approximate
#' Shapley values in parallel across features; default is `FALSE`. **NOTE:**
#' setting `parallel = TRUE` requires the
#' [foreach](https://cran.r-project.org/package=foreach) package (in
#' Suggests) and an appropriate (i.e., system-specific) *parallel backend*
#' (e.g., via the doParallel package). For reproducible parallel results, use
#' the doRNG package to register the backend. Note that the internal batching
#' of predictions (see `batch_size`) means models whose `predict()` methods
#' are already multithreaded (e.g., ranger or xgboost) often gain little from
#' `parallel = TRUE`. The fastshap namespace is always added to `.packages`
#' automatically (socket-based clusters, the default on Windows, start with
#' no packages attached), so any *additional* packages needed by
#' `pred_wrapper` should be supplied via `.packages` as usual.
#'
#' @param seed Integer specifying a random seed for reproducibility; passed to
#' [base::set.seed()]. Default is `NULL` (no seed). **NOTE:** the Monte Carlo
#' loop was restructured (vectorized) in version 0.2.0, so seeded results
#' differ from those produced by fastshap (<= 0.1.5).
#'
#' @param raw Logical indicating whether or not to return the raw per-simulation
#' Shapley values from each Monte Carlo replication. If `TRUE`, a 3-D array
#' of dimensions `n x p x nsim` is returned, where `n` is the number of
#' observations, `p` is the number of features, and `nsim` is the number of
#' Monte Carlo replications; for example, `apply(result, 1:2, sd)` computes
#' standard errors for each (observation, feature) pair. Only supported when
#' `adjust = FALSE`. Default is `FALSE`.
#'
#' @param batch_size Optional positive integer giving the maximum number of
#' rows to pass to `pred_wrapper()` per call. By default (`NULL`), all
#' `nsim` Monte Carlo replications for a feature are stacked and evaluated in
#' a single pair of prediction calls, which is fastest but requires
#' `2 * nrow(newdata) * nsim * ncol(X)` values of working memory per feature;
#' set `batch_size` to bound the size of each prediction call instead. All of
#' the randomness is drawn up front, so for a fixed seed the results are
#' identical regardless of `batch_size`.
#'
#' @param ... Additional optional arguments to be passed on to
#' [foreach::foreach()] whenever `parallel = TRUE` (e.g., `.packages` for
#' loading packages on the workers); ignored otherwise.
#'
#' @return If `shap_only = TRUE` (the default), a matrix is returned with one
#' column for each feature specified in `feature_names` (if
#' `feature_names = NULL`, the default, there will
#' be one column for each feature in `X`) and one row for each observation
#' in `newdata` (if `newdata = NULL`, the default, there will be one
#' row for each observation in `X`). Additionally, the returned matrix will
#' have an attribute called `"baseline"` containing the baseline value. If
#' `shap_only = FALSE`, then a list is returned with three components:
#' * `shapley_values` - a matrix of Shapley values (as described above);
#' * `feature_values` - the corresponding feature values (for plotting with
#' [shapviz::shapviz()]);
#' * `baseline` - the corresponding baseline value (for plotting with
#' [shapviz::shapviz()]).
#'
#' @seealso You can find more examples (with larger and more realistic data
#' sets) on the **fastshap** GitHub repository:
#' <https://github.com/bgreenwell/fastshap>.
#'
#' @note
#' Setting `exact = TRUE` with a linear model (i.e., an
#' [stats::lm()] or [stats::glm()] object) assumes that the
#' input features are independent. Also, setting `adjust = TRUE` is
#' experimental and we follow the same approach as in
#' [shap](https://github.com/shap/shap).
#'
#' @references
#' Strumbelj, E., and Igor K. (2014). Explaining prediction models and
#' individual predictions with feature contributions. Knowledge and information
#' systems, 41(3), 647-665.
#'
#' Lundberg, S. M., Erion, G., Chen, H., DeGrave, A., Prutkin, J. M., Nair, B.,
#' Katz, R., Himmelfarb, J., Bansal, N., and Lee, Su-In (2020). From local
#' explanations to global understanding with explainable AI for trees.
#' Nature Machine Intelligence, 2(1), 2522–5839.
#'
#' @rdname explain
#'
#' @export
#'
#' @examples
#' #
#' # A projection pursuit regression (PPR) example
#' #
#'
#' # Load the sample data; see ?datasets::mtcars for details
#' data(mtcars)
#'
#' # Fit a projection pursuit regression model
#' fit <- ppr(mpg ~ ., data = mtcars, nterms = 5)
#'
#' # Prediction wrapper
#' pfun <- function(object, newdata) {  # needs to return a numeric vector
#'   predict(object, newdata = newdata)
#' }
#'
#' # Compute approximate Shapley values using 10 Monte Carlo simulations
#' set.seed(101)  # for reproducibility
#' shap <- explain(fit, X = subset(mtcars, select = -mpg), nsim = 10,
#'                 pred_wrapper = pfun)
#' head(shap)
explain <- function(object, ...) {
  UseMethod("explain")
}


#' @rdname explain
#'
#' @export
explain.default <- function(object, feature_names = NULL, X = NULL, nsim = 1,
                            pred_wrapper = NULL, newdata = NULL, adjust = FALSE,
                            baseline = NULL, shap_only = TRUE, parallel = FALSE,
                            raw = FALSE, seed = NULL, exact = FALSE,
                            batch_size = NULL, ...) {

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # exact = TRUE is only supported for lm, xgboost, and lightgbm objects
  if (isTRUE(exact)) {
    warning("Setting `exact = TRUE` is only supported for `lm`, `xgboost`, and ",
            "`lightgbm` objects; falling back to Monte Carlo approximation.",
            call. = FALSE)
  }

  # Validate required arguments before any use
  if (nsim < 1L) {
    stop("`nsim` must be a positive integer.", call. = FALSE)
  }
  if (!is.null(batch_size) && (!is.numeric(batch_size) ||
      length(batch_size) != 1L || batch_size < 1)) {
    stop("`batch_size` must be a single positive integer.", call. = FALSE)
  }
  if (is.null(X)) {
    stop("Training features required for approximate Shapley values. Please ",
         "supply them via the `X` argument; see `?fastshap::explain` for ",
         "details.", call. = FALSE)
  }
  if (is.null(pred_wrapper)) {
    stop("Prediction function required for approximate Shapley values. Please ",
         "supply one via the `pred_wrapper` argument; see ",
         "`?fastshap::explain` for details.", call. = FALSE)
  }

  if (inherits(X, what = "tbl_df")) {
    X <- as.data.frame(X)
  }
  if (inherits(newdata, what = "tbl_df")) {
    newdata <- as.data.frame(newdata)
  }
  if (is.null(newdata)) {
    newdata <- X  # explain all rows of background data set
  }

  # Check types
  if (!identical(class(X), class(newdata))) {
    stop("Arguments `X` and `newdata` do not inherit from the same class: ",
         "\n   * `X` inherits from class \"", class(X), "\"\n   * `newdata` ",
         "inherits from class \"", class(newdata), "\"", call. = FALSE)
  }

  if (isTRUE(adjust)) {
    # Need nsim > 1 to compute variances for adjustment
    if (nsim < 2) {
      stop("Need `nsim > 1` whenever `adjust = TRUE`.", call. = FALSE)
    }
    if (isTRUE(raw)) {
      warning("`raw = TRUE` is not supported when `adjust = TRUE`; ignoring ",
              "`raw`.", call. = FALSE)
      raw <- FALSE
    }
  }

  # Compute baseline/average training prediction (fnull) and predictions
  # associated with each explanation (fx); if `adjust = FALSE`, then the
  # baseline is not needed and defaults to NA.
  if (isTRUE(adjust)) {
    fx <- pred_wrapper(object, newdata = newdata)
  }
  fnull <- if (isTRUE(adjust)) {
    if (is.null(baseline)) {  # baseline value (i.e., avg training prediction)
      mean(pred_wrapper(object, newdata = X))
    } else {
      baseline
    }
  } else {
    NA_real_
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

  # Preallocate results; each feature contributes an n x nsim matrix of
  # Monte Carlo differences that is summarized on the fly.
  phis <- matrix(NA_real_, nrow = n, ncol = nfeat)
  if (isTRUE(adjust)) {
    phi_var <- matrix(NA_real_, nrow = n, ncol = nfeat)
  }
  if (isTRUE(raw)) {
    raw_arr <- array(NA_real_, dim = c(n, nfeat, nsim))
  }
  summarize <- function(j, D) {
    m <- rowMeans(D)
    phis[, j] <<- m
    if (isTRUE(adjust)) {
      Dc <- D - m
      phi_var[, j] <<- rowSums(Dc * Dc) / (nsim - 1)
    }
    if (isTRUE(raw)) {
      raw_arr[, j, ] <<- D
    }
    invisible(NULL)
  }
  compute_D <- function(col) {
    explain_feature(object, col = col, X = X, nsim = nsim,
                    pred_wrapper = pred_wrapper, newdata = newdata,
                    batch_size = batch_size)
  }

  if (isTRUE(parallel)) {
    if (!requireNamespace("foreach", quietly = TRUE)) {
      stop("Package \"foreach\" is required whenever `parallel = TRUE`. ",
           "Install it and register a parallel backend (e.g., via the ",
           "doParallel package), or use the default `parallel = FALSE`.",
           call. = FALSE)
    }
    `%dopar%` <- foreach::`%dopar%`
    col.i <- NULL  # avoid R CMD check note about undefined global
    # `compute_D()` is a closure over `explain_feature()`, an internal
    # (non-exported) function one level removed from the loop body itself.
    # foreach's automatic export only inspects the loop body's own free
    # variables (here, just `compute_D`), so on socket-based clusters (the
    # default on Windows, where workers are fresh R sessions with no
    # packages attached) `explain_feature()` would not be found unless the
    # fastshap namespace is explicitly loaded on each worker.
    dots <- list(...)
    dots$.packages <- unique(c("fastshap", dots$.packages))
    Ds <- do.call(foreach::foreach, c(list(col.i = cols), dots)) %dopar% {
      compute_D(col.i)
    }
    for (j in seq_len(nfeat)) {
      summarize(j, Ds[[j]])
      Ds[j] <- list(NULL)  # release as we go
    }
  } else {
    for (j in seq_len(nfeat)) {
      summarize(j, compute_D(cols[j]))
    }
  }

  if (isTRUE(raw)) {
    dimnames(raw_arr) <- list(rownames(newdata), feature_names, NULL)
    return(raw_arr)
  }

  if (isTRUE(adjust)) {
    # Adjust the sum of the approximate Shapley values to satisfy local
    # accuracy using the variance-weighted technique from the Python {shap}
    # library; for details, see
    # https://github.com/slundberg/shap/blob/master/shap/explainers/_sampling.py
    for (i in seq_len(n)) {
      err <- fx[i] - sum(phis[i, ]) - fnull
      max_var <- max(phi_var[i, ])
      if (max_var == 0) {
        # All per-feature variances are zero (degenerate draws); skip
        # adjustment to avoid 0/0 = NaN poisoning the result.
        next
      }
      v <- (phi_var[i, ] / max_var) * 1e6
      phis[i, ] <- phis[i, ] + err * (v - (v * sum(v)) / (1 + sum(v)))
    }
  }

  colnames(phis) <- feature_names

  if (isFALSE(shap_only)) {
    structure(list(
      "shapley_values" = phis,
      "feature_values" = newdata[, feature_names, drop = FALSE],
      "baseline" = fnull
    ), class = "explain")
  } else {
    # Add baseline and class attributes
    attr(phis, which = "baseline") <- fnull
    class(phis) <- c("explain", class(phis))
    phis
  }

}


#' Assemble the standard output structure from a matrix of Tree SHAP
#' contributions that includes a bias column
#'
#' @keywords internal
#'
#' @noRd
tree_shap_result <- function(phis, X, bias_col, shap_only) {
  feature_cols <- setdiff(colnames(phis), bias_col)
  baseline <- phis[, bias_col]
  shap <- phis[, feature_cols, drop = FALSE]
  if (isFALSE(shap_only)) {
    structure(list(
      "shapley_values" = shap,
      "feature_values" = X[, feature_cols, drop = FALSE],
      "baseline" = baseline
    ), class = "explain")
  } else {
    attr(shap, which = "baseline") <- baseline
    class(shap) <- c("explain", class(shap))
    shap
  }
}


#' @rdname explain
#'
#' @export
explain.lm <- function(object, feature_names = NULL, X, nsim = 1,
                       pred_wrapper, newdata = NULL, adjust = FALSE,
                       exact = FALSE, baseline = NULL, shap_only = TRUE,
                       parallel = FALSE, ...) {
  if (isTRUE(exact)) {  # use Linear SHAP
    phis <- if (is.null(newdata)) {
      stats::predict(object, type = "terms", ...)
    } else {
      stats::predict(object, newdata = newdata, type = "terms", ...)
    }
    baseline <- attr(phis, which = "constant")  # mean response for all training data
    data_to_explain <- if (is.null(newdata)) X else newdata
    fn <- if (is.null(feature_names)) colnames(phis) else feature_names
    if (isFALSE(shap_only)) {
      return(structure(list(
        "shapley_values" = phis,
        "feature_values" = data_to_explain[, fn[fn %in% colnames(data_to_explain)], drop = FALSE],
        "baseline" = baseline
      ), class = "explain"))
    } else {
      attr(phis, which = "baseline") <- baseline
      class(phis) <- c("explain", class(phis))
      return(phis)
    }
  } else {
    explain.default(object, feature_names = feature_names, X = X, nsim = nsim,
                    pred_wrapper = pred_wrapper, newdata = newdata,
                    adjust = adjust, baseline = baseline, shap_only = shap_only,
                    parallel = parallel, ...)
  }
}


#' @rdname explain
#'
#' @export
explain.xgb.Booster <- function(object, feature_names = NULL, X = NULL, nsim = 1,
                                pred_wrapper, newdata = NULL, adjust = FALSE,
                                exact = FALSE, baseline = NULL,
                                shap_only = TRUE, parallel = FALSE, ...) {
  if (isTRUE(exact)) {  # use Tree SHAP
    if (is.null(X) && is.null(newdata)) {
      stop("Must supply `X` or `newdata` argument (but not both).",
           call. = FALSE)
    }
    X <- if (is.null(X)) newdata else X
    # Strip any subclasses (e.g., "xgboost" in xgboost >= 2.0) so that
    # predict.xgb.Booster is dispatched, which supports predcontrib = TRUE.
    phis <- stats::predict(structure(object, class = "xgb.Booster"),
                           newdata = X, predcontrib = TRUE,
                           approxcontrib = FALSE, ...)
    # Multiclass xgboost returns a 3-D array [n, p+1, num_class]; detect and error.
    if (length(dim(phis)) > 2L || is.list(phis)) {
      stop("Exact Shapley values (`exact = TRUE`) are not yet supported for ",
           "multiclass `xgboost` models. Use the Monte Carlo approximation ",
           "(default) with a prediction wrapper that returns the class score(s) ",
           "of interest instead.", call. = FALSE)
    }
    if (!is.matrix(phis)) {
      phis <- matrix(phis, nrow = nrow(X), dimnames = list(rownames(X), names(phis)))
    }
    # The bias column was named "BIAS" in older xgboost and "(Intercept)" in newer.
    tree_shap_result(phis, X = X, bias_col = colnames(phis)[ncol(phis)],
                     shap_only = shap_only)
  } else {
    explain.default(object, feature_names = feature_names, X = X, nsim = nsim,
                    pred_wrapper = pred_wrapper, newdata = newdata,
                    adjust = adjust, baseline = baseline, shap_only = shap_only,
                    parallel = parallel, ...)
  }
}


#' @rdname explain
#'
#' @export
explain.lgb.Booster <- function(object, feature_names = NULL, X = NULL, nsim = 1,
                                pred_wrapper, newdata = NULL, adjust = FALSE,
                                exact = FALSE, baseline = NULL,
                                shap_only = TRUE, parallel = FALSE, ...) {
  if (isTRUE(exact)) {  # use Tree SHAP
    if (is.null(X) && is.null(newdata)) {
      stop("Must supply `X` or `newdata` argument (but not both).",
           call. = FALSE)
    }
    X <- if (is.null(X)) newdata else X

    # Adapt LightGBM predict() interface
    if (utils::packageVersion("lightgbm") > package_version("3.3.2")) {
      phis <- stats::predict(object, X, type = "contrib", ...)
    } else {
      phis <- stats::predict(object, X, predcontrib = TRUE, ...)
    }

    # Multiclass lightgbm returns (p+1)*num_class columns; detect and error.
    if (ncol(phis) != ncol(X) + 1L) {
      stop("Exact Shapley values (`exact = TRUE`) are not yet supported for ",
           "multiclass `lightgbm` models. Use the Monte Carlo approximation ",
           "(default) with a prediction wrapper that returns the class score(s) ",
           "of interest instead.", call. = FALSE)
    }
    colnames(phis) <- c(colnames(X), "BIAS")
    tree_shap_result(phis, X = X, bias_col = "BIAS", shap_only = shap_only)
  } else {
    explain.default(object, feature_names = feature_names, X = X, nsim = nsim,
                    pred_wrapper = pred_wrapper, newdata = newdata,
                    adjust = adjust, baseline = baseline, shap_only = shap_only,
                    parallel = parallel, ...)
  }
}
