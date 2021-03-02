# TODO: 
#
#   1. Add \code{type = "beeswarm"}.

#' Plotting Shapley values
#' 
#' Construct Shapley-based importance plots or Shap-based dependence plots.
#' 
#' @param object An object of class \code{"explain"}.
#' 
#' @param type Character string specifying which type of plot to construct. 
#' Current options are \code{"importance"} (for Shapley-based variable 
#' importance plots), \code{"dependence"} (for Shapley-based dependence 
#' plots), and \code{"contribution"} (for visualizing the feature contributions
#' to an individual prediction).
#' 
#' @param feature Character string specifying which feature to use when 
#' \code{type = "dependence"}. If \code{NULL} (default) the first feature will
#' be used to construct the plot.
#' 
#' @param num_features Integer specifying the number of variables to plot. 
#' Default is \code{NULL} which will cause all variables to be displayed.
#' 
#' @param X A matrix-like R object (e.g., a data frame or matrix) containing 
#' ONLY the feature columns from the training data.
#' 
#' @param feature_values A matrix-like R object (e.g., a data frame or matrix) 
#' containing the feature values correposnding to the instance being explained.
#' Only used when \code{type = "dependence"}. \strong{NOTE:} Must contain the 
#' same column structure (e.g., column names, order, etc.) as \code{X}.
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
#' @param row_num Integer specifying a single row/instance in \code{object} to
#' plot the explanation when \code{type = "contribution"}. If \code{NULL} (the 
#' default) the explanation for the first row/instance will be used.
#' 
#' @param ... Additional optional arguments to be passed on to 
#' \code{\link[ggplot2]{geom_col}} (if \code{type = "importance"}) or 
#' \code{\link[ggplot2]{geom_point}} (if \code{type = "dependence"}).
#' 
#' @return A \code{"ggplot"} object; see 
#' \code{\link[ggplot2]{ggplot2-package}} for details.
#' 
#' @importFrom ggplot2 aes_string autoplot coord_flip geom_col geom_point
#' 
#' @importFrom ggplot2 geom_smooth ggplot guides xlab ylab
#' 
#' @importFrom stats reorder
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
#' mtcars.ppr <- ppr(mpg ~ ., data = mtcars, nterms = 1)
#' 
#' # Compute approximate Shapley values using 10 Monte Carlo simulations
#' set.seed(101)  # for reproducibility
#' shap <- explain(mtcars.ppr, X = subset(mtcars, select = -mpg), nsim = 10, 
#'                 pred_wrapper = predict)
#' shap
#' 
#' # Shapley-based plots
#' library(ggplot2)
#' autoplot(shap)  # Shapley-based importance plot
#' autoplot(shap, type = "dependence", feature = "wt", X = mtcars)
#' autoplot(shap, type = "contribution", row_num = 1)  # explain first row of X
autoplot.explain <- function(
  object, 
  type = c("importance", "dependence", "contribution"),
  feature = NULL, 
  num_features = NULL,
  X = NULL, 
  feature_values = NULL,
  color_by = NULL, 
  smooth = FALSE, 
  smooth_color = "red", 
  smooth_linetype = "solid", 
  smooth_size = 1, 
  smooth_alpha = 1, 
  row_num = NULL,
  ...
) {

  # Number of features to show for "importance" and "contribution" plots
  if (is.null(num_features)) {
    num_features <- ncol(object)
  } else {
    num_features <- as.integer(num_features)[1L]  # make sure num_features is a single integer
    if (num_features > ncol(object) || num_features < 1L) {
      warning("Argument `num_features` should be between 1 and ", ncol(object), 
              ". Displaying all features instead.", call. = FALSE)
      num_features <- ncol(object)
    }
  }
  
  type <- match.arg(type)
  if (type == "importance") {
    
    # Construct data to plot
    shap_imp <- data.frame(
      Variable = names(object),
      Importance = apply(object, MARGIN = 2, FUN = function(x) mean(abs(x)))
    )
    shap_imp <- shap_imp[order(shap_imp$Importance, decreasing = TRUE), ]  # sort in descending order
    shap_imp <- shap_imp[seq_len(num_features), ]  # only retain num_features variable importance scores
    
    # Construct plot
    x_string <- "reorder(Variable, Importance)"
    p <- ggplot(shap_imp, aes_string(x_string, "Importance")) +
      geom_col(...) +
      coord_flip() +
      xlab("") +
      ylab("mean(|Shapley value|)")
    
  } else if (type == "dependence") {
    
    # Check for X
    if (is.null(X)) {
      stop("The training set is required for SHAP-dependence plots. Please",
           " specify it via the `X` argument in the call to `autoplot()`.",
           call. = FALSE)
    }
    
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
    if (!is.null(feature_values)) {
      if (ncol(feature_values) != ncol(object)) {
        stop("`feature_values` should contain the same number of columns as `X`.",
             call. = FALSE)
      }
      if (!identical(names(feature_values), names(object))) {
        stop("`feature_values` should contain the same column names as `X`.",
             call. = FALSE)
      }
      if (nrow(feature_values) > 1) {
        stop("`feature_values` must contain a single row.", call. = FALSE)
      }
      names(object) <- paste0(names(feature_values), " = ", 
                              # data.matrix(feature_values))
                              as.matrix(feature_values))
    }
    shap_con <- data.frame(
      Variable = names(object),
      Shapley = t(object[row_num, , drop = TRUE])
    )
    shap_con <- shap_con[order(abs(shap_con$Shapley), decreasing = TRUE), ]  # sort in descending order
    shap_con <- shap_con[seq_len(num_features), ]  # only retain num_features variable importance scores
    
    # Construct plot
    x_string <- "reorder(Variable, abs(Shapley))"
    p <- ggplot(shap_con, aes_string(x = x_string, y = "Shapley", 
                                     color = "Shapley", fill = "Shapley")) +
      geom_col(...) +
      coord_flip() +
      xlab("") +
      ylab("Shapley value") +
      guides(fill = FALSE, color = FALSE)

  }
  
  # Return plot
  p

}
