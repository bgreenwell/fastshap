library(fastshap)
# library(reticulate)

# Set up a Conda environment for new version of shap
# reticulate::conda_create("r-reticulate-shap-0.37.0")
# reticulate::conda_install(
#   envname = "r-reticulate-shap-0.37.0",
#   packages = c("shap==0.37.0"),
#   pip = FALSE
# )
# reticulate::use_condaenv("r-reticulate-shap-0.37.0", required = TRUE)

# Set up a Conda environment for old version of shap
# reticulate::conda_create("r-reticulate-shap-0.35.0")
# reticulate::conda_install(
#   envname = "r-reticulate-shap-0.35.0",
#   packages = c("shap==0.35.0"),
#   pip = FALSE
# )
#reticulate::use_condaenv("r-reticulate-shap-0.35.0", required =  TRUE)

# Check version
# shap <- import("shap")
# py_get_attr(shap, name = "__version__")

# Generate training data from the Friedman 1 benchmark problem
trn <- gen_friedman(500, seed = 101)
X <- subset(trn, select = -y)

# Fit a MARS model to the simulated Friedman benchmark data
mars <- earth::earth(y ~ ., data = trn, degree = 2)

# Prediction wrapper
pfun <- function(object, newdata) {
  unname(predict(object, newdata = newdata)[, 1L, drop = TRUE])
}
preds <- pfun(mars, newdata = trn)

# Generate approximate Shapley values for entire training set
set.seed(102)  # for reproducibility
shap_all <- explain(mars, X = X, pred_wrapper = pfun, nsim = 1)

# Try calling shap.forceplot()
force_plot(
  object = shap_all[1L, ],
  baseline = mean(preds),
  feature_values = X[1L, ]
)
