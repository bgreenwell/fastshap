#' @keywords internal
shapley_column <- function(object, X, column, pred_wrapper) {
  
  # Find column position if feature name given
  if (is.character(column)) {
    column <- which(column == colnames(X))
  }
  
  # Generate original and sampled feature instances
  W <- X[sample(nrow(X)), ]  # shuffle rows
  p <- ncol(X)  # number of features
  
  # Generate Frankenstein matrix; same dimension as X and W
  O <- genOMat(X)
  O <- if (column == 1) {
    cbind(TRUE, O)
  } else if (column == p) {
    cbind(O, TRUE)
  } else {
    cbind(O[, 1:(column - 1)], TRUE, O[, column:(p - 1)])
  }
  
  # Generate "Frankenstein" instances from X and W (in one fell swoop)
  #
  # The first data frame (B1) are the instances of interest, but all values in 
  # the order before and including value of feature j are replaced by feature 
  # values from the shuffled observations in W.
  B1 <- B2 <- X
  B1[O] <- X[O]
  B1[!O] <- W[!O]
  O[, column] <- FALSE
  B2[O] <- X[O]
  B2[!O] <- W[!O]
  
  # Return difference in predictions
  pred_wrapper(object, newdata = B1) - pred_wrapper(object, newdata = B2)
  
}


#' Shapley values
#' 
#' Compute fast (approximate) Shapley values for a set of features.
#' 
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest.
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
#' @param ... Additional optional arguments to be passed onto
#' \code{\link[plyr]{laply}}.
#' 
#' @export
fastshap <- function(object, feature_names, X, pred_wrapper, nsim = 1, ...) {
  res <- plyr::laply(feature_names, .fun = function(x) {
    apply(replicate(nsim, {
      shapley_column(object, X = X, column = x, pred_wrapper = pred_wrapper)
    }), MARGIN = 1, FUN = mean)
  }, ...)
  res <- tibble::as_tibble(t(res))
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
#' @importFrom ggplot2 aes autoplot coord_flip geom_col geom_point geom_smooth
#' 
#' @importFrom ggplot2 xlab ylab
#'
#' @export
autoplot.fastshap <- function(object, type = c("importance", "dependence"),
                              feature = NULL, X, ...) {
  type <- match.arg(type)
  if (type == "importance") {
    shap_imp <- data.frame(
      Variable = names(shap),
      Importance = apply(shap, MARGIN = 2, FUN = function(x) sum(abs(x)))
    )
    ggplot(shap_imp, aes(reorder(Variable, Importance), Importance)) +
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
    ggplot(shap_dep, aes(x = x, y = y)) +
      geom_point(alpha = 0.3) +
      geom_smooth() +
      xlab(feature) +
      ylab("Shapley value")
  }
}