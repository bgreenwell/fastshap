#' Additive force plots
#' 
#' Visualize Shapley values with additive force style layouts from the Python
#' \href{https://github.com/slundberg/shap}{shap} package.
#' 
#' @param object An object of class \code{"explain"}.
#' 
#' @param baseline Numeric giving the average prediction across all the training
#' observations. \strong{NOTE:} It is recommended to provide this argument 
#' whenever \code{object} contains approximate Shapley values.
#' 
#' @param feature_values A matrix-like R object (e.g., a data frame or matrix) 
#' containing the corresponding feature values for the explanations in 
#' \code{object}.
#' 
#' @param display Character string specifying how to display the results. 
#' Current options are \code{"viewer"} (default) and \code{"html"}. The latter
#' is necessary for viewing the display inside of an \strong{rmarkdown} 
#' document.
#' 
#' @param ... Additional optional arguments. (Currently ignored.)
#' 
#' @details The resulting plot shows how each feature contributes to push the 
#' model output from the baseline prediction (i.e., the average predicted 
#' outcome over the entire training set `X`) to the corresponding model output. 
#' Features pushing the prediction higher are shown in red, while those pushing 
#' the prediction lower are shown in blue.
#' 
#' @note It should be noted that only exact Shapley explanations (i.e., calling 
#' \code{fastshap::explain()} with \code{exact = TRUE}) satisfy the so-called 
#' \emph{additivity property} where the sum of the feature contributions for 
#' \emph{x} must add up to the difference between the corresponding prediction 
#' for \emph{x} and the average of all the training predictions (i.e., the 
#' baseline). Consequently, if you don't set \code{adjust = TRUE} in the call to 
#' \code{\link{explain}} before using \code{fastshap::force_plot()}, the 
#' output value displayed on the plot will not make much sense.
#' 
#' @references 
#' Lundberg, Scott M, Bala Nair, Monica S Vavilala, Mayumi Horibe, Michael J 
#' Eisses, Trevor Adams, David E Liston, et al. 2018. "Explainable 
#' Machine-Learning Predictions for the Prevention of Hypoxaemia During 
#' Surgery." Nature Biomedical Engineering 2 (10). Nature Publishing Group: 749.
#' 
#' @rdname forceplot
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
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
#'                 pred_wrapper = predict, adjust = TRUE)
#' 
#' # Visualize first explanation
#' preds <- predict(mtcars.ppr, newdata = mtcars)
#' x <- subset(mtcars, select = -mpg)[1L, ]  # take first row of feature values
#' force_plot(shap[1L, ], baseline = mean(preds), feature_values = x)
#' }
force_plot <- function(object, ...) {
  UseMethod("force_plot")
}


#' @rdname forceplot
#' 
#' @export
force_plot.explain <- function(object, baseline = NULL, feature_values = NULL, 
                               display = c("viewer", "html"), ...) {
  
  # Check for dependencies
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package \"reticulate\" needed for this function to work. Please ",
         "install it.", call. = FALSE)
  }
  if (!reticulate::py_module_available("shap")) {
    stop("The Python package \"shap\" is needed for this function to work. ",
         "Please install it; visit https://github.com/slundberg/shap for ",
         "details.", call. = FALSE)
  }
  
  # # Check dimension of explanations (for now, only a single row is allowed)
  # if (nrow(object) != 1) {
  #   stop("Currently, only a single explanation is allowed to be plotted.",
  #        call. = FALSE)
  # }
  
  # Deal with optional baseline argument
  if (is.null(baseline)) {
    baseline <- attr(object, which = "baseline")  # only for exact values
    if (length(unique(baseline)) == 1) {  # FIXME: Will this ever not be the 
      baseline <- unique(baseline)        # case for xgboost models?
    }
  } 
  
  # Deal with optional feature_values argument
  if (!is.null(feature_values)) {
    feature_values <- data.matrix(feature_values)  # coerce to matrix
  }
  
  # Import shap module and construct HTML for plot
  shap <- reticulate::import("shap")
  # shap$initjs()
  fp <- shap$force_plot(
    base_value = if (is.null(baseline)) 0 else baseline, 
    shap_values = data.matrix(object),  # coerce to matrix
    features = feature_values,
    feature_names = names(object),
    matplotlib = FALSE,
    ...
  )

  # Display results
  # FIXME: Is this the best way to determine/compare Python package versions?
  shap_version <- reticulate::py_get_attr(shap, name = "__version__")
  shap_version <- package_version(as.character(shap_version))
  message("Using shap version ", shap_version, ".")
  tfile <- tempfile(fileext = ".html")
  if (shap_version < "0.36.0") {
    shap$save_html(tfile, plot_html = fp)
  } else {
    shap$save_html(tfile, plot = fp)
  }
  display <- match.arg(display)
  
  # Check for dependencies
  if (display == "viewer") {
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      if (rstudioapi::isAvailable()) {  # make sure RStudio is running
        rstudioapi::viewer(tfile) 
      }
    } else if (requireNamespace("utils", quietly = TRUE)) {
      utils::browseURL(tfile)
    } else {
      stop("Packages \"rstudioapi\" or \"utils\" needed for this function to ",
           "work. Please install one of them.", call. = FALSE)
    }
  } else {
    if (!requireNamespace("htmltools", quietly = TRUE)) {
      stop("Package \"htmltools\" needed for this function to work. Please ",
           "install it.", call. = FALSE)
    }
    htmltools::includeHTML(tfile)
  }

}
