# Exits
if (!requireNamespace("reticulate", quietly = TRUE)) {
  exit_file("Package \"reticulate\" is missing.")
}
if (!reticulate::py_module_available("shap")) {
  exit_file("Python package \"shap\" is missing.")
}
if (!rstudioapi::isAvailable()) {
  exit_file("RStudio is not running.")
}

# # Load required packages
# suppressMessages({
#   library(earth)
# })

# Generate training data from the Friedman 1 benchmark problem
trn <- gen_friedman(500, seed = 101)
X <- subset(trn, select = -y)

# Fit a MARS model to the simulated Friedman benchmark data
mars <- eartth::earth(y ~ ., data = trn, degree = 2)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)[, 1L, drop = TRUE]
}

# Generate approximate Shapley values for entire training set
set.seed(102)  # for reproducibility
shap_all <- explain(mars, X = X, pred_wrapper = pfun, nsim = 1)

# Construct force plots
preds <- pfun(mars, newdata =  X)
expect_null(
  force_plot(
    object = shap_all[1L, ],
    prediction = preds[1L],
    baseline = mean(preds),
    feature_values = X[1L, ]
  )
)
expect_null(force_plot(shap_all[1L, ]))
