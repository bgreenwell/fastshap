#' Additive force plot
#' 
#' Visualize Shapley values with additive force style layouts from the Python
#' \href{https://github.com/slundberg/shap}{shap} package.
#' 
#' @param object An object of class \code{"explain"}.
#' 
#' @param prediction Numeric vector containing the corresponding prediction for 
#' each row in \code{object}.
#' 
#' @param baseline Numeric giving the average prediction across all the training
#' observations. 
#' 
#' @param feature_values A matrix-like R object (e.g., a data frame or matrix) 
#' containing the corresponding feature values for the explanations in 
#' \code{object}.
#' 
#' @param ... Additional optional arguments. (Currently ignored.)
#' 
#' @rdname forceplot
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
#' 
#' # Visualize first explanation
#' preds <- predict(mtcars.ppr, newdata = mtcars)
#' x <- subset(mtcars, select = -mpg)[1L, ]  # take first row of feature values
#' force_plot(shap[1L, ], prediction = preds[1L], baseline = mean(preds),
#'            feature_values = x)
force_plot <- function(object, ...) {
  UseMethod("force_plot")
}


#' @rdname forceplot
#' 
#' @export
force_plot.explain <- function(object, prediction = NULL, baseline = NULL, 
                               feature_values = NULL,...) {
  
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
  # if (nrow(ex) != 1) {
  #   stop("Currently, only a single explanation is allowed to be plotted.",
  #        call. = FALSE)
  # }
  
  # Rescale explanations so they add up to `prediction - baseline`
  #
  # FIXME: Is there a better way to do this? (Cf. lines 140--151 from
  # https://github.com/slundberg/shap/blob/master/shap/explainers/sampling.py.)
  if (!is.null(prediction) && !is.null(baseline)) {
    for (i in seq_len(nrow(object))) {
      object[i, ] <- (object[i, ] / sum(object[i, ])) * 
        (prediction[i] - baseline)
    }
  }
  
  # Deal with optional feature_values argument
  if (!is.null(feature_values)) {
    feature_values <- data.matrix(feature_values)  # coerce to matrix
  }
  
  # Import shap module and construct HTML for plot
  shap <- reticulate::import("shap")
  fp <- shap$force_plot(
    base_value = if (is.null(baseline)) 0 else baseline, 
    shap_values = data.matrix(object),  # coerce to matrix
    features = feature_values,
    feature_names = names(object),
    matplotlib = FALSE,
    ...
  )
  
  # View plot by writing HTML out to a temporary file and reading it back into 
  # an appropriate viewer
  tfile <- tempfile(fileext = ".html")
  shap$save_html(tfile, plot_html = fp)
  # Check for dependency
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    rstudioapi::viewer(tfile)
  } else if (requireNamespace("utils", quietly = TRUE)) {
    utils::browseURL(tfile)
  } else {
    stop("Packages \"rstudioapi\" or \"utils\" needed for this function to ",
         "work. Please install one of them.", call. = FALSE)
  }
  
}