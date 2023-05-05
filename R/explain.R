#' @keywords internal
#' 
#' @useDynLib fastshap, .registration = TRUE
#' 
#' @importFrom Rcpp sourceCpp
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom stats var
explain_column <- function(object, X, column, pred_wrapper, newdata = NULL) {
  
  # Check types
  if (!is.null(newdata) && !identical(class(X), class(newdata))) {
    stop("Arguments `X` and `newdata` do not inherit from the same class: ",
         "\n   * `X` inherits from class \"", class(X), "\"\n   * `newdata` ",
         "inherits from class \"", class(newdata), "\"", call. = FALSE)
  }
  
  # Find column position if feature name given
  if (is.character(column)) {
    column <- which(column == colnames(X))
  }
  
  # Extract dimensions of X (before possible subsetting)
  n <- nrow(X)  # number of training instances
  p <- ncol(X)  # number of features
  
  # Generate original and sampled feature instances
  if (is.null(newdata)) {  # FIXME: Should sampling be done with replacement?
    W <- X[sample(n, replace = TRUE), ]  
    O <- genOMat(n, p)
  } else {
    W <- X[sample(n, size = nrow(newdata), replace = TRUE), , drop = FALSE]  # randomly sample rows from full X
    O <- genOMat(n, p)[sample(n, size = nrow(newdata), replace = TRUE), , drop = FALSE]
    X <- newdata  # observations of interest
  }
  
  #print(list("W" = W, "O" = O))
  
  # Finish building logical matrix that resembles the random permutation order 
  # to use for each row of X and W (a TRUE indicates that the corresponding
  # feature appeared before the feature of interest in the associated 
  # permutation)
  O <- if (column == 1) {  # case 1
    cbind(TRUE, O)
  } else if (column == p) {  # case 2
    cbind(O, TRUE)
  } else {  # case 3
    cbind(  
      O[, 1:(column - 1), drop = FALSE], 
      TRUE, 
      O[, column:(p - 1), drop = FALSE]
    )
  }

  # Generate list of "Frankenstein" matrices (in one fell swoop). Essentially,
  # each rows in B[[1L]] and B[[2L]] is a combination of feature values from X
  # and W
  if (is.matrix(X)) {
    
    # Use RcppArmadillo for slight performance gain
    B <- genFrankensteinMatrices(X, W, O, feature = column)
    colnames(B[[1L]]) <- colnames(B[[2L]]) <- colnames(X)

  } else {
    
    # Use base R's logical subsetting
    B <- list(X, X)
    B[[1L]][O] <- X[O]
    B[[1L]][!O] <- W[!O]
    O[, column] <- FALSE
    B[[2L]][O] <- X[O]
    B[[2L]][!O] <- W[!O]
    
  }

  # Make sure each component of `B` has the same column classes as `X`
  B[[1L]] <- copy_classes(B[[1L]], y = X)
  B[[2L]] <- copy_classes(B[[2L]], y = X)
  
  # Return differences in predictions
  pred_wrapper(object, newdata = B[[1L]]) - 
    pred_wrapper(object, newdata = B[[2L]])  
  
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
#' Currently only available for [stats::lm()], 
#' [xgboost::xgboost()], and [lightgbm::lightgbm()] objects. 
#' Default is `FALSE`. Note that setting `exact = TRUE` will return 
#' explanations for each of the [stats::terms()] in an 
#' [stats::lm()] object. Default is `FALSE`.
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
#' setting `parallel = TRUE` requires setting up an appropriate (i.e., 
#' system-specific) *parallel backend* as described in the 
#' [foreach](https://cran.r-project.org/package=foreach); for details, see
#' `vignette("foreach", package = "foreach")` in R.
#' 
#' @param ... Additional optional arguments to be passed on to 
#' [foreach::foreach()] whenever `parallel = TRUE`. For example, you may need
#' to supply additional packages that the parallel task depends on via the
#' `.packages` argument to [foreach::foreach()]. **NOTE:** 
#' [foreach::foreach()]'s `.combine` argument is already set internally by 
#' `explain()`, so passing it via the `...` argument would likely result in an
#' error.
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
#' [shap](https://github.com/slundberg/shap).
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
#' fit <- lm(mpg ~ ., data = mtcars)
#' 
#' # Compute approximate Shapley values using 10 Monte Carlo simulations
#' set.seed(101)  # for reproducibility
#' shap <- explain(fit, X = subset(mtcars, select = -mpg), nsim = 10, 
#'                 pred_wrapper = predict)
#' shap
#' 
#' # Compute exact Shapley (i.e., LinearSHAP) values
#' shap <- explain(fit, exact = TRUE)
#' shap
explain <- function(object, ...) {
  UseMethod("explain")
} 


#' @rdname explain
#' 
#' @export
explain.default <- function(object, feature_names = NULL, X = NULL, nsim = 1, 
                            pred_wrapper = NULL, newdata = NULL, adjust = FALSE,
                            baseline = NULL, shap_only = TRUE, parallel = FALSE, 
                            ...) {
  
  # Compute baseline/average training prediction (fnull) and predictions 
  # associated with each explanation (fx); if `adjust = FALSE`, then the 
  # baseline is not needed and defaults to zero.
  if (is.null(X)) {
    stop("Training features required for approximate Shapley values. Please ",
         "supply them via the `X` argument; see `?fastshap::explain` for ",
         "details.", call. = FALSE)
  }
  if (inherits(X, what = "tbl_df")) {
    X <- as.data.frame(X)
  }
  if (is.null(newdata)) { 
    newdata <- X  # explain all rows of background data set
  } 
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
    0  # FIXME: Is it really necessary to return zero?
  }
  
  # Deal with other NULL arguments
  if (is.null(feature_names)) {
    feature_names = colnames(X)
  }
  if (is.null(pred_wrapper)) {
    stop("Prediction function required for approximate Shapley values. Please ",
         "supply one via the `pred_wrapper` argument; see ",
         "`?fastshap::explain` for details.", call. = FALSE)
  }
  
  # Set up the 'foreach' "do" operator
  `%.do%` <- if (isTRUE(parallel)) `%dopar%` else `%do%`

  # Experimental patch for more efficiently computing single-row explanations
  if (!is.null(newdata)) {
    if (nrow(newdata) == 1L && nsim > 1L) {
      newdata.stacked <- newdata[rep(1L, times = nsim), ]  # replicate obs `nsim` times
      phis <- explain.default(object, feature_names = feature_names, X = X, 
                              nsim = 1L, pred_wrapper = pred_wrapper, 
                              newdata = newdata.stacked, adjust = FALSE, ...)
      phi.avg <- t(colMeans(phis))  # transpose to keep as row matrix
      if (isTRUE(adjust)) {
        # Adjust sum of approximate Shapley values using the same technique from 
        # the Python {shap} library. For details, see the explanation at
        # https://github.com/slundberg/shap/blob/master/shap/explainers/_sampling.py.
        phi.var <- apply(phis, MARGIN = 2, FUN = stats::var)
        err <- fx - sum(phi.avg) - fnull
        v <- (phi.var / max(phi.var)) * 1e6
        adj <- err * (v - (v * sum(v)) / (1 + sum(v)))
        phi.avg <- phi.avg + adj
      }
      # if (isTRUE(variance)) {
      #   attr(phi.avg, which = "variance") <- phi.var
      # }
      if (isFALSE(shap_only)) {
        return(structure(list(  # convenient structure for shapviz
          "shapley_values" = phi.avg,
          "feature_values" = newdata[, feature_names, drop = FALSE],
          "baseline" = fnull
        ), class = "explain"))
      } else {
        attr(phi.avg, which = "baseline") <- fnull
        class(phi.avg) <- c("explain", class(phi.avg))
        return(phi.avg)
      }
    }
  }
  
  if (isTRUE(adjust)) {  # compute variances and adjust the sum
    
    # Need nsim > 1 to compute variances for adjustment
    if (nsim < 2) {
      stop("Need `nsim > 1` whenever `adjust = TRUE`.", call. = FALSE)
    }
    
    # Compute approximate Shapley value averages and variances for each feature
    # across all nsim replications. Output will be an array with dimensions
    # nrow(newdata) x 2 x length(feature_names); the second dimension holds the
    # averages (i.e., unadjusted Shapley estimates) and variances.
    acomb <- function(...) abind(..., along = 3)
    phis.stats <- foreach(i = feature_names, .combine = "acomb", ...) %.do% {
      reps <- replicate(nsim, {  # replace later with vapply()
        explain_column(object, X = X, column = i, pred_wrapper = pred_wrapper,
                       newdata = newdata)
      })
      if (is.matrix(reps)) {
        # abind(rowMeans(reps), apply(reps, MARGIN = 1, FUN = var), along = 2)
        cbind(rowMeans(reps), apply(reps, MARGIN = 1, FUN = var))
      } else {
        cbind(mean.default(reps), stats::var(reps))
      }
    }
    
    # Adjust sum of approximate Shapley values using the same technique 
    # described above
    if (length(dim(phis.stats)) == 3L) {  # multiple explanations
      for (i in seq_len(dim(phis.stats)[1L])) {  # loop through each observation
        err <- fx - sum(phis.stats[i, 1L, ]) - fnull
        v <- (phis.stats[i, 2L, ] / max(phis.stats[i, 2L, ])) * 1e6
        adj <- err[i] * (v - (v * sum(v)) / (1 + sum(v)))
        phis.stats[i, 1L, ] <- phis.stats[i, 1L, ] + adj  # adjust Shapley estimate
      }
      phis <- phis.stats[, 1L, ]
      # if (isTRUE(variance)) {
      #   attr(phis, which = "variance") <- phis.stats[, 2L, ]
      # }
    } else {  # single explanation
      err <- fx - sum(phis[, 1L]) - fnull
      v <- (phis.stats[, 2L] / max(phis.stats[, 2L])) * 1e6
      adj <- err * (v - (v * sum(v)) / (1 + sum(v)))
      phis.stats[, 1L] <- phis.stats[, 1L] + adj
      phis <- phis.stats[, 1L]  # adjust Shapley estimate
      # if (isTRUE(variance)) {
      #   attr(phis, which = "variance") <- phis.stats[, 2L]
      # }
    }
    
  } else {  # don't adjust
    
    # Compute approximate Shapley values
    phis <- foreach(i = feature_names, .combine = 'cbind', ...) %.do% {
      reps <- replicate(nsim, {  # replace later with vapply()
        explain_column(object, X = X, column = i, pred_wrapper = pred_wrapper,
                       newdata = newdata)
      })
      if (is.matrix(reps)) {
        rowMeans(reps)
      } else {
        mean.default(reps)
      }
    }
    
  }
  
  # Reformat if necessary and fix column names
  if (length(feature_names) == 1L) {
    phis <- as.matrix(phis) 
  } 
  colnames(phis) <- feature_names
  
  if (isFALSE(shap_only)) {
    return(structure(list(
      "shapley_values" = phis,
      "feature_values" = newdata[, feature_names, drop = FALSE],
      "baseline" = fnull
    ), class = "explain"))
  } else {
    # Add baseline and class attributes
    attr(phis, which = "baseline") <- fnull
    class(phis) <- c("explain", class(phis))
    return(phis)
  }

}


#' @rdname explain
#' 
#' @export
explain.lm <- function(object, feature_names = NULL, X, nsim = 1, 
                       pred_wrapper, newdata = NULL, adjust = FALSE, 
                       exact = FALSE, shap_only = TRUE, ...) {
  if (isTRUE(exact)) {  # use Linear SHAP
    phis <- if (is.null(newdata)) {
      stats::predict(object, type = "terms", ...)
    } else {
      stats::predict(object, newdata = newdata, type = "terms", ...)
    }
    baseline <- attr(phis, which = "constant")  # mean response for all training data
    if (isFALSE(shap_only)) {
      return(structure(list(
        "shapley_values" = phis,
        "feature_values" = newdata[, feature_names, drop = FALSE]
      ), class = "explain"))
    } else {
      attr(phis, which = "baseline") <- baseline
      class(phis) <- c("explain", class(phis))
      return(phis)
    }
  } else {
    explain.default(object, feature_names = feature_names, X = X, nsim = nsim,
                    pred_wrapper = pred_wrapper, newdata = newdata, 
                    adjust = adjust, ...)
  }
}


#' @rdname explain
#' 
#' @export
explain.xgb.Booster <- function(object, feature_names = NULL, X = NULL, nsim = 1, 
                                pred_wrapper, newdata = NULL, adjust = FALSE, 
                                exact = FALSE, shap_only = TRUE, ...) {
  if (isTRUE(exact)) {  # use Tree SHAP
    if (is.null(X) && is.null(newdata)) {
      stop("Must supply `X` or `newdata` argument (but not both).", 
           call. = FALSE)
    }
    X <- if (is.null(X)) newdata else X
    phis <- stats::predict(object, newdata = X, predcontrib = TRUE, 
                           approxcontrib = FALSE, ...)
    if (isFALSE(shap_only)) {
      return(structure(list(
        "shapley_values" = phis,
        "feature_values" = newdata[, feature_names, drop = FALSE],
        "baseline" = phis[, "BIAS"]
      ), class = "explain"))
    } else {
      attr(phis, which = "baseline") <- phis[, "BIAS"]
      class(phis) <- c("explain", class(phis))
      phis <- phis[, colnames(phis) != "BIAS", drop = FALSE]
      return(phis)
    }
  } else {
    explain.default(object, feature_names = feature_names, X = X, nsim = nsim,
                    pred_wrapper = pred_wrapper, newdata = newdata, 
                    adjust = adjust, ...)
  }
}


#' @rdname explain
#' 
#' @export
explain.lgb.Booster <- function(object, feature_names = NULL, X = NULL, nsim = 1, 
                                pred_wrapper, newdata = NULL, adjust = FALSE, 
                                exact = FALSE, shap_only = TRUE, ...) {
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
    
    colnames(phis) <- c(colnames(X), "BIAS")
    if (isFALSE(shap_only)) {
      return(structure(list(
        "shapley_values" = phis,
        "feature_values" = newdata[, feature_names, drop = FALSE],
        "baseline" = phis[, "BIAS"]
      ), class = "explain"))
    } else {
      attr(phis, which = "baseline") <- phis[, "BIAS"]
      phis <- phis[, colnames(phis) != "BIAS", drop = FALSE]
      class(phis) <- c("explain", class(phis))
      return(phis)
    }
  } else {
    explain.default(object, feature_names = feature_names, X = X, nsim = nsim,
                    pred_wrapper = pred_wrapper, newdata = newdata, 
                    adjust = adjust, shap_only = shap_only, ...)
  }
}
