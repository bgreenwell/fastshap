#' @keywords internal
#' 
#' @useDynLib fastshap, .registration = TRUE
#' 
#' @importFrom Rcpp sourceCpp
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
#' Compute fast (approximate) Shapley values for a set of features.
#' 
#' @param object A fitted model object (e.g., a \code{\link[ranger]{ranger}},
#' \code{\link[xgboost]{xgboost}}, or \code{\link[earth]{earth}} object, to name
#' a few).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest. If \code{NULL} (default) they will be
#' taken from the column names of \code{X}.
#'
#' @param X A matrix-like R object (e.g., a data frame or matrix) containing 
#' ONLY the feature columns from the training data. \strong{NOTE:} This argument
#' is required whenever \code{exact = FALSE}.
#'
#' @param pred_wrapper Prediction function that requires two arguments,
#' \code{object} and \code{newdata}. \strong{NOTE:} This argument is required 
#' whenever \code{exact = FALSE}. The output of this function should be 
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
#' Shapley value (only used when \code{exact = FALSE}). Default is 1. 
#' \strong{NOTE:} To obtain the most accurate results, \code{nsim} should be set 
#' as large as feasibly possible.
#' 
#' @param newdata A matrix-like R object (e.g., a data frame or matrix) 
#' containing ONLY the feature columns for the observation(s) of interest; that 
#' is, the observation(s) you want to compute explanations for. Default is 
#' \code{NULL} which will produce approximate Shapley values for all the rows in 
#' \code{X} (i.e., the training data).
#' 
#' @param adjust Logical indicating whether or not to adjust the sum of the 
#' estimated Shapley values to satisfy the \emph{additivity} (or 
#' \emph{local accuracy}) property; that is, to equal the difference between the 
#' model's prediction for that sample and the average prediction over all the 
#' training data (i.e., \code{X}).
#' 
#' @param exact Logical indicating whether to compute exact Shapley values. 
#' Currently only available for \code{\link[stats]{lm}}, 
#' \code{\link[xgboost]{xgboost}}, and \code{\link[lightgbm]{lightgbm}} objects. 
#' Default is \code{FALSE}. Note that setting \code{exact = TRUE} will return 
#' explanations for each of the \code{\link[stats]{terms}} in an 
#' \code{\link[stats]{lm}} object.
#' 
#' @param baseline Numeric baseline to use in determining the additive property 
#' of the adjust Shapley estimates. Adjust Shapley values for a single 
#' prediction (\code{fx}) sum to the difference \code{fx - baseline}.
#' Defaults to \code{NULL} which corresponds to the average training prediction.
#' 
#' @param ... Additional optional arguments to be passed on to
#' \code{\link[plyr]{laply}}.
#' 
#' @return A matrix with one column for each feature specified in 
#' \code{feature_names} (if \code{feature_names = NULL}, the default, there will
#' be one column for each feature in \code{X}) and one row for each observation
#' in \code{newdata} (if \code{newdata = NULL}, the default, there will be one
#' row for each observation in \code{X}). 
#' 
#' @seealso You can find more examples (with larger and more realistic data 
#' sets) on the \strong{fastshap} GitHub repository: 
#' \url{https://github.com/bgreenwell/fastshap}.
#' 
#' @note 
#' Setting \code{exact = TRUE} with a linear model (i.e., an 
#' \code{\link[stats]{lm}} or \code{\link[stats]{glm}} object) assumes that the
#' input features are independent. Also, setting \code{adjust = TRUE} is 
#' experimental and we follow the same approach as in
#' \href{https://github.com/slundberg/shap}{shap}.
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
                            baseline = NULL, ...) {

  # Set up the foreach "do" operator
  `%.do%` <- `%do%`
  
  # Experimental patch for more efficiently computing single-row explanations
  if (!is.null(newdata)) {
    if (nrow(newdata) == 1L && nsim > 1L) {
      newdata <- newdata[rep(1L, times = nsim), ]  # replicate row `nsim` times
      phis <- explain.default(object, feature_names = feature_names, X = X, 
                              nsim = 1L, pred_wrapper = pred_wrapper, 
                              newdata = newdata, adjust = FALSE, ...)
      phi.avg <- t(colMeans(phis))  # transpose to keep as row matrix
      if (isTRUE(adjust)) {
        # Adjust sum of approximate Shapley values using the same technique from 
        # the Python {shap} library. For details, see the explanation at
        # https://github.com/slundberg/shap/blob/master/shap/explainers/_sampling.py.
        fx <- pred_wrapper(object, newdata = newdata[1L, , drop = FALSE])
        fnull <-  if (is.null(baseline)) {
          mean(pred_wrapper(object, newdata = X))
        } else {
          baseline
        }
        phi.var <- apply(phis, MARGIN = 2, FUN = stats::var)
        err <- fx - sum(phi.avg) - fnull
        v <- (phi.var / max(phi.var)) * 1e6
        adj <- err * (v - (v * sum(v)) / (1 + sum(v)))
        phi.avg <- phi.avg + adj
      }
      return(phi.avg)
    }
  }
  
  # Deal with NULL arguments
  if (is.null(feature_names)) {
    feature_names = colnames(X)
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
    phis <- foreach(i = feature_names, .combine = "acomb") %.do% {
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

    # Compute average training prediction (fnull) and predictions associated
    # with each explanation (fx)
    if (is.null(newdata)) {  # explain all training rows
      fx <- pred_wrapper(object, newdata = X)
      fnull <- if (is.null(baseline)) {
        mean(fx) 
      } else {
        baseline
      }
    } else {  # explain new observation(s)
      fx <- pred_wrapper(object, newdata = newdata)
      fnull <-  if (is.null(baseline)) {
        mean(pred_wrapper(object, newdata = X))
      } else {
        baseline
      }
    }
    
    # Adjust sum of approximate Shapley values using the same technique 
    # described above
    if (length(dim(phis)) == 3L) {  # multiple explanations
      for (i in seq_len(dim(phis)[1L])) {  # loop through each observation
        err <- fx - sum(phis[i, 1L, ]) - fnull
        v <- (phis[i, 2L, ] / max(phis[i, 2L, ])) * 1e6
        adj <- err[i] * (v - (v * sum(v)) / (1 + sum(v)))
        phis[i, 1L, ] <- phis[i, 1L, ] + adj  # adjust Shapley estimate
      }
      phis <- phis[, 1L, ]
    } else {  # single explanation
      err <- fx - sum(phis[, 1L]) - fnull
      v <- (phis[, 2L] / max(phis[, 2L])) * 1e6
      adj <- err * (v - (v * sum(v)) / (1 + sum(v)))
      phis[, 1L] <- phis[, 1L] + adj
      phis <- phis[, 1L]  # adjust Shapley estimate
    }
    
  } else {  # don't adjust
    
    # Compute approximate Shapley values
    phis <- foreach(i = feature_names, .combine = 'cbind') %.do% {
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
  
  return(phis)

}


#' @rdname explain
#' 
#' @export
explain.lm <- function(object, feature_names = NULL, X, nsim = 1, 
                       pred_wrapper, newdata = NULL, adjust = FALSE, 
                       exact = FALSE, ...) {
  if (isTRUE(exact)) {  # use Linear SHAP
    phis <- if (is.null(newdata)) {
      stats::predict(object, type = "terms", ...)
    } else {
      stats::predict(object, newdata = newdata, type = "terms", ...)
    }
    baseline <- attr(phis, which = "constant")  # mean response for all training data
    attr(phis, which = "baseline") <- baseline
    return(phis)
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
                                exact = FALSE, ...) {
  if (isTRUE(exact)) {  # use Tree SHAP
    if (is.null(X) && is.null(newdata)) {
      stop("Must supply `X` or `newdata` argument (but not both).", 
           call. = FALSE)
    }
    X <- if (is.null(X)) newdata else X
    phis <- stats::predict(object, newdata = X, predcontrib = TRUE, 
                           approxcontrib = FALSE, ...)
    attr(phis, which = "baseline") <- phis[, "BIAS"]
    phis <- phis[, colnames(phis) != "BIAS", drop = FALSE]
    return(phis)
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
                                exact = FALSE, ...) {
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
    attr(phis, which = "baseline") <- phis[, "BIAS"]
    phis <- phis[, colnames(phis) != "BIAS", drop = FALSE]
    return(phis)
  } else {
    explain.default(object, feature_names = feature_names, X = X, nsim = nsim,
                    pred_wrapper = pred_wrapper, newdata = newdata, 
                    adjust = adjust, ...)
  }
}
