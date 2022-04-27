# The purpose of this script is to compare the computating time and output
# between fastshap and the Python shap library (using the kernel explainer and
# the sampling explainer).

# Load required packages
library(doParallel)
library(fastshap)
library(ggplot2)
library(ranger)
library(reticulate)

# Set up reticulate
use_python("/Users/b780620/anaconda3/bin/python3")

# Set up the Boston housing data (R side)
boston <- pdp::boston
boston$chas <- as.integer(boston$chas) - 1
X <- data.matrix(subset(boston, select = -cmedv))


# Random forest ----------------------------------------------------------------

# Train a random forest
set.seed(944)  # for reproducibility
rfo <- ranger(cmedv ~ ., data = boston)
 
# Prediction wrappers
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}


# Comput approximate Shapley values
set.seed(945)
system.time(
  shap <- fastshap(rfo, X = X, nsim = 50, pred_wrapper = pfun)
)
#   user  system elapsed 
# 81.318   7.091  19.778

# Run Python script
py_run_file("slowtests/shap-boston-rf.py")
rfo_shap_sampling <- py$rfo_shap_sampling
colnames(rfo_shap_sampling) <- names(py$X)
paste0("shap runtime: ", round(py$t1, digits = 2), " seconds.")
py$t2
# [1] 572.3229

# Compare results


# XGBoost: compare FastSHAP results with TreeSHAP ------------------------------

# Load required packages
library(xgboost)

# Train an XGBoost model
set.seed(2134)
xgb <- xgboost(data = X, label = boston$cmedv, nrounds = 100, eta = 0.01)

# fastshap
set.seed(2136)
system.time(
  xgb_fastshap <- fastshap(xgb, X = X, nsim = 1000, pred_wrapper = predict, 
                           .progress = "text")
)

# TreeSHAP
xgb_treeshap <- predict

# Compare results
plot(xgb_fastshap$lstat, xgb_treeshap[, "lstat"])
abline(0, 1, col = "red2", lty = 2)
cor(xgb_fastshap$lstat, xgb_treeshap[, "lstat"])

# Plot results
par(mfrow = c(1, 2))
plot(X[, "lstat", drop = TRUE], xgb_fastshap[, "lstat", drop = TRUE],
     main = "FastSHAP", xlab = "lstat", ylab = "Shapley value (approximate)")
plot(X[, "lstat", drop = TRUE], xgb_treeshap[, "lstat", drop = TRUE],
     main = "TreeSHAP", xlab = "lstat", ylab = "Shapley value (exact)")
