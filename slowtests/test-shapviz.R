# Exits
if (!requireNamespace("shapviz", quietly = TRUE)) {
  exit_file("Package shapviz missing")
}
if (!requireNamespace("ranger", quietly = TRUE)) {
  exit_file("Package ranger missing")
}

# Read in the data and clean it up a bit
set.seed(2220)  # for reproducibility
trn <- gen_friedman(500)
tst <- gen_friedman(10)

# Features only
X <- subset(trn, select = -y)
newX <- subset(tst, select = -y)

# Fit a default random forest
set.seed(2222)  # for reproducibility
rfo <- ranger::ranger(y ~ ., data = trn)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Generate explanations for test set
set.seed(2024)  # for reproducibility
ex1 <- explain(rfo, X = X, newdata = newX, pred_wrapper = pfun, adjust = TRUE,
               nsim = 50)

# Same, but set `shap_only = FALSE` for convenience with shapviz
set.seed(2024)  # for reproducibility
ex2 <- explain(rfo, X = X, newdata = newX, pred_wrapper = pfun, adjust = TRUE,
               nsim = 50, shap_only = FALSE)

# Create "shapviz" objects
shv1 <- shapviz.explain(ex1, X = newX)
shv2 <- shapviz.explain(ex2)
shv3 <- shapviz.explain(ex2$shapley_values, X = newX, baseline = ex2$baseline)

# Expectations
expect_error(shapviz.explain(ex1))
expect_warning(shapviz.explain(ex2$shapley_values, X = newX))
expect_identical(ex2$baseline, mean(pfun(rfo, X)))
expect_identical(shv1$X, shv2$X)
expect_identical(shv1$X, shv3$X)
expect_identical(shv1$baseline, shv2$baseline)
expect_identical(shv1$baseline, shv3$baseline)

# SHAP waterfall plots
sv_waterfall(shv1, row_id = 1)
sv_waterfall(shv2, row_id = 1)
sv_waterfall(shv3, row_id = 1)

# shapviz.explain <- function (object, X, baseline = NULL, collapse = NULL, ...) {
#   if (is.matrix(object)) {  # explain() called with shap_only=TRUE
#     if (is.null(baseline)) {  # try to extract baseline attribute
#       baseline <- attr(object, which = "baseline")
#     }
#     if (is.null(baseline)) {  # will still be NULL is missing, so check again
#       warning("No baseline attribute found in ", deparse(substitute(object)),
#               "; setting baseline to zero.", call. = FALSE)
#       baseline <- 0
#     }
#     shapviz.matrix(object, X = X, baseline = baseline, collapse = collapse)
#   } else {  # explain() called with shap_only=FALSE
#     shapviz.matrix(object$shapley_values, X = object$feature_values,
#                    baseline = object$baseline, collapse = collapse)
#   }
# }

shapviz.explain <- function(object, X = NULL, baseline = NULL, 
                            collapse = NULL, ...) {
  if (inherits(object, "tibble")) {  # packageVersion("fastshap") <= "0.0.7"
    object <- as.matrix(object)
  }
  if (is.matrix(object)) {   # explain() called with shap_only=TRUE
    if (is.null(baseline)) {  # try to extract baseline attribute
      baseline <- attr(object, which = "baseline")
    }
    if (is.null(baseline)) {  # will still be NULL is missing, so check again
      warning("No baseline attribute found in ", deparse(substitute(object)),
              "; setting baseline to zero.", call. = FALSE)
      baseline <- 0
    }
    if (is.null(X)) {
      stop("No featue values found. Pass feature values via 'X' or use ",
           "'fastshap::explain(..., shap_only = FALSE)'", call. = FALSE)
    }
    shapviz.matrix(object, X = X, baseline = baseline, collapse = collapse)
  } else {  # explain() called with shap_only=FALSE
    # if (!is.null(baseline)) {  
    #   baseline <- object[["baseline"]]
    # }
    # if (!is.null(X)) {  
    #   X <- object[["feature_values"]]
    # }
    shapviz.matrix(object[["shapley_values"]], X = object[["feature_values"]], 
                   baseline = object[["baseline"]], collapse = collapse)
  }
}
