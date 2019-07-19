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
