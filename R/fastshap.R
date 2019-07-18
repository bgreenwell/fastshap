#' @keywords internal
#' 
#' @useDynLib fastshap, .registration = TRUE
shapley_column <- function(object, X, column, pred_wrapper, newdata = NULL) {
  
  # Find column position if feature name given
  if (is.character(column)) {
    column <- which(column == colnames(X))
  }
  
  # Extract dimensions of X (before possible subsetting)
  n <- nrow(X)  # number of training instances
  p <- ncol(X)  # number of features
  
  # Generate original and sampled feature instances
  if (is.null(newdata)) {
    W <- X[sample(n), ]  # shuffle rows
    O <- genOMat(X)
  } else {
    W <- X[sample(n, size = nrow(newdata)), , drop = FALSE]  # randomly sample rows from full X
    O <- genOMat(X)[sample(n, size = nrow(newdata)), , drop = FALSE]
    X <- newdata  # observations of interest
  }

  # Generate "Frankenstein" matrix; same dimension as X and W. Here we expand O, 
  # which currently has p-1 columns, to have p columns and set the feature 
  # column of interest to `TRUE`
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
  
  # Generate "Frankenstein" instances from X and W (in one fell swoop)
  B1 <- B2 <- X
  B1[O] <- X[O]
  B1[!O] <- W[!O]
  O[, column] <- FALSE
  B2[O] <- X[O]
  B2[!O] <- W[!O]
  
  # Return differences in predictions
  pred_wrapper(object, newdata = B1) - pred_wrapper(object, newdata = B2)
  
}


#' Fast approximate Shapley values
#' 
#' Compute fast (approximate) Shapley values for a set of features.
#' 
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest. If \code{NULL} (default) they will be
#' taken from the column names of \code{X}.
#'
#' @param X A matrix-like R object (e.g., a data frame or matrix) containing 
#' ONLY the feature columns from the training data.
#'
#' @param pred_wrapper Prediction function that requires two arguments,
#' \code{object} and \code{newdata}. The output of this function should be 
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
#' @param nsim The number of Monte Carlo repititions to use for estimating each 
#' Shapley value. Default is 1.
#' 
#' @param newdata A matrix-like R object (e.g., a data frame or matrix) 
#' containing ONLY the feature columns for the observation(s) of interest. 
#' Default is \code{NULL} which will produce approximate Shapley values for all 
#' the rows in \code{X}.
#' 
#' @param ... Additional optional arguments to be passed onto
#' \code{\link[plyr]{laply}}.
#' 
#' @return A tibble with one column for each feature specified in 
#' \code{feature_names} (if \code{feature_names = NULL}, the default, there will
#' be one column for each feature in \code{X}) and one row for each observation
#' in \code{newdata} (if \code{newdata = NULL}, the default, there will be one
#' row for each observation in \code{X}). 
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' #
#' # A projection pursuit regression example
#' #
#' 
#' # Load the sample data
#' data(mtcars)
#' 
#' # Fit a projection pursuit regression model
#' mtcars.ppr <- ppr(mpg ~ ., data = mtcars, nterms = 1)
#' 
#' # Compute approximate Shapley values using 10 Monte Carlo simulations
#' set.seed(101)  # for reproducibility
#' shap <- fastshap(mtcars.ppr, X = subset(mtcars, select = -mpg), nsim = 10, 
#'                  pred_wrapper = predict)
#' shap
#' 
#' # Shapley-based plots
#' library(ggplot2)
#' autoplot(shap)  # Shapley-based importance plot
#' autoplot(shap, type = "dependence", feature = "wt", X = mtcars)
#' }
fastshap <- function(object, feature_names = NULL, X, nsim = 1, pred_wrapper,
                     newdata = NULL, ...) {
  if (is.null(feature_names)) {
    feature_names = colnames(X)
  }
  res <- plyr::laply(feature_names, .fun = function(x) {
    # apply(replicate(nsim, {
    #   shapley_column(object, X = X, column = x, pred_wrapper = pred_wrapper,
    #                  newdata = newdata)
    # }), MARGIN = 1, FUN = mean)
    reps <- replicate(nsim, {
      shapley_column(object, X = X, column = x, pred_wrapper = pred_wrapper,
                     newdata = newdata)
    })
    res <- if (is.matrix(reps)) {
      apply(reps, MARGIN = 1, FUN = mean)
    } else {
      mean(reps)
    }
  }, ...)
  res <- if (is.null(dim(res))) {
    tibble::enframe(res, name = NULL)
  } else {
    res <- tibble::as_tibble(t(res))
  }
  names(res) <- feature_names
  class(res) <- c(class(res), "fastshap")
  res
}


#' Plotting Shapley values
#' 
#' Construct Shap-based importance plots or Shap-based dependence plots.
#' 
#' @param object An object of class \code{"fastshap"}.
#' 
#' @param type Character string specifying which type of plot to construct. 
#' Current options are \code{"importance"} (for Shapley-based variable 
#' importance plots) and \code{"dependence"} (for Shapley-based dependence 
#' plots).
#' 
#' @param feature Character string specifying which feature to use when 
#' \code{type = "dependence"}. If \code{NULL} (default) the first feature will
#' be used to construct the plot.
#' 
#' @param X A matrix-like R object (e.g., a data frame or matrix) containing 
#' ONLY the feature columns from the training data.
#' 
#' @param ... Additional optional arguemnts. (Currently ignored.)
#' 
#' @importFrom ggplot2 aes_string autoplot coord_flip geom_col geom_point
#' 
#' @importFrom ggplot2 geom_smooth ggplot xlab ylab
#' 
#' @importFrom stats reorder
#'
#' @export
autoplot.fastshap <- function(object, type = c("importance", "dependence"),
                              feature = NULL, X, ...) {
  type <- match.arg(type)
  if (type == "importance") {
    shap_imp <- data.frame(
      Variable = names(object),
      Importance = apply(object, MARGIN = 2, FUN = function(x) sum(abs(x)))
    )
    ggplot(shap_imp, aes_string("reorder(Variable, Importance)", "Importance")) +
      geom_col() +
      coord_flip() +
      xlab("") +
      ylab("mean(|Shapley value|)")
  } else {
    if (is.null(feature)) {
      feature <- names(object)[1L]
    }
    shap_dep <- data.frame(
      x = X[, feature, drop = TRUE], 
      y = object[, feature, drop = TRUE]
    )
    ggplot(shap_dep, aes_string(x = "x", y = "y")) +
      geom_point(alpha = 0.3) +
      geom_smooth() +
      xlab(feature) +
      ylab("Shapley value")
  }
}