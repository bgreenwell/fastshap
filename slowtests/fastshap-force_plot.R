library(fastshap)
# library(reticulate)

# Set up a Conda environment for new version of shap
version <- "0.37.0"
env <- paste0("r-reticulate-shap-", version)
# reticulate::conda_create(env)
# reticulate::conda_install(
#   envname = env,
#   packages = paste0("shap==", version),
#   pip = FALSE
# )
reticulate::use_condaenv(env, required = TRUE)

# Check version
shap <- reticulate::import("shap")
reticulate::py_get_attr(shap, name = "__version__")

# Generate training data from the Friedman 1 benchmark problem
trn <- gen_friedman(500, seed = 101)
X <- subset(trn, select = -y)

# Fit a MARS model to the simulated Friedman benchmark data
mars <- earth::earth(y ~ ., data = trn, degree = 2)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)[, 1L, drop = TRUE]
}
preds <- pfun(mars, newdata = trn)

# Generate approximate Shapley values for entire training set
set.seed(102)  # for reproducibility
ex <- explain(mars, X = X, pred_wrapper = pfun, nsim = 1)

# Try calling shap.forceplot()
force_plot(
  object = ex[1L, ],
  baseline = mean(preds),
  feature_values = X[1L, ]
)
