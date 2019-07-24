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
#' @param color_by Character string specifying an optional feature column in 
#' \code{X} to use for coloring whenever \code{type = "dependence"}.
#' 
#' @param smooth Logical indicating whether or not to add a smoother to the
#' scatterplot whenever \code{type = "dependence"}. Default is \code{TRUE}.
#' 
#' @param smooth_color The color to use for the smoother whenever 
#' \code{smooth = TRUE}. The default is \code{"black"}; see 
#' \code{\link[ggplot2]{geom_smooth}} for details.
#' 
#' @param smooth_linetype The type of line to use for the smoother whenever 
#' \code{smooth = TRUE}. The default is \code{"solid"}; see 
#' \code{\link[ggplot2]{geom_smooth}} for details.
#' 
#' @param smooth_size The size to use for the smoother whenever 
#' \code{smooth = TRUE}. The default is \code{1}; see 
#' \code{\link[ggplot2]{geom_smooth}} for details.
#' 
#' @param smooth_alpha The transparency to use for the smoother whenever 
#' \code{smooth = TRUE}. The default is \code{1}; see 
#' \code{\link[ggplot2]{geom_smooth}} for details.
#' 
#' @param ... Additional optional arguments to be passed onto 
#' \code{\link[ggplot2]{geom_col}} (if \code{type = "importance"}) or 
#' \code{\link[ggplot2]{geom_point}} (if \code{type = "dependence"}).
#' 
#' @importFrom ggplot2 aes_string autoplot coord_flip geom_col geom_point
#' 
#' @importFrom ggplot2 geom_smooth ggplot xlab ylab
#' 
#' @importFrom stats reorder
#'
#' @export
autoplot.fastshap <- function(
  object, 
  type = c("importance", "dependence", "contribution"),
  feature = NULL, 
  X, 
  color_by = NULL, 
  smooth = FALSE, 
  smooth_color = "red", 
  smooth_linetype = "solid", 
  smooth_size = 1, 
  smooth_alpha = 1, 
  row_num = NULL,
  ...
) {

  type <- match.arg(type)
  if (type == "importance") {
    
    # Construct data to plot
    shap_imp <- data.frame(
      Variable = names(object),
      Importance = apply(object, MARGIN = 2, FUN = function(x) mean(abs(x)))
    )
    
    # Construct plot
    x_string <- "reorder(Variable, Importance)"
    p <- ggplot(shap_imp, aes_string(x_string, "Importance")) +
      geom_col(...) +
      coord_flip() +
      xlab("") +
      ylab("mean(|Shapley value|)")
    
  } else if (type == "dependence") {
    
    # Construct data to plot
    if (is.null(feature)) {
      feature <- names(object)[1L]
    }
    shap_dep <- data.frame(
      x = X[, feature, drop = TRUE], 
      y = object[, feature, drop = TRUE]
    )
    if (!is.null(color_by)) {
      shap_dep[[color_by]] <- X[[color_by]]
    }

    # Construct plot
    p <- if (!is.null(color_by)) {
      ggplot(shap_dep, aes_string(x = "x", y = "y", color = color_by))
    } else {
      ggplot(shap_dep, aes_string(x = "x", y = "y"))
    }
    p <- p +
      geom_point(...) +
      xlab(feature) +
      ylab("Shapley value")
    if (smooth) {
      p <- p + geom_smooth(se = FALSE, color = smooth_color,
                           linetype = smooth_linetype, size = smooth_size,
                           alpha = smooth_alpha)
    }
  
  } else {
    
    # Construct data to plot
    if (is.null(row_num)) {
      row_num <- 1L
    }
    shap_con <- data.frame(
      Variable = names(object),
      Shapley = t(object[row_num, , drop = TRUE])
    )
    
    # Construct plot
    x_string <- "reorder(Variable, Shapley)"
    p <- ggplot(shap_con, aes_string(x = x_string, y = "Shapley", 
                                     color = "Shapley", fill = "Shapley")) +
      geom_col(...) +
      coord_flip() +
      xlab("") +
      ylab("Shapley value")
    
  }
  
  # Return plot
  p

}
