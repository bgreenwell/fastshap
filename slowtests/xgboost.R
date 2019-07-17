# Load required packages
library(fastshap)
library(xgboost)

# Ames housing data
set.seed(101)
trn <- as.data.frame(mlbench::mlbench.friedman1(3000))
X <- subset(trn, select = -y)  # feature columns only

# # Use 5-fold CV to tune an XGBoost model
# set.seed(102)
# bst.cv <- xgb.cv(data = data.matrix(X), label = trn$y,
#                  max_depth = 2, eta = 0.3, nround = 1000, 
#                  nfold = 5, objective = "reg:linear")
# plot(test_rmse_mean ~ iter, data = bst.cv$evaluation_log)
# best_iter <- which.min(bst.cv$evaluation_log$test_rmse_mean)

# Fit an XGBoost model
set.seed(103)
bst <- xgboost(data = data.matrix(X), label = trn$y,
               max_depth = 2, eta = 0.3, nround = 300, 
               objective = "reg:linear")

# Use built-in SHAP functionality
time1 <- system.time({
  set.seed(104)
  shap_xgb <- predict(bst, newdata = data.matrix(X), predcontrib = TRUE,
                      approxcontrib = TRUE)
})

# Remove BIAS column
shap_xgb <- shap_xgb[, setdiff(colnames(shap_xgb), "BIAS")]

# USe fastshap package
pfun <- function(object, newdata) {
  predict(object, newdata = data.matrix(newdata))
}
time2 <- system.time({
  set.seed(105)
  shap1 <- fastshap(bst, X = X, pred_wrapper = pfun)
})
time3 <- system.time({
  set.seed(107)
  shap10 <- fastshap(bst, X = X, pred_wrapper = pfun, nsim = 10)
})
time4 <- system.time({
  set.seed(108)
  shap50 <- fastshap(bst, X = X, pred_wrapper = pfun, nsim = 50)
})

# Compare plots
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2) + 0.1) 
plot(X[, 3], shap_xgb[, 3, drop = TRUE], main = "TreeSHAP (approx); ~ 0.024 sec",
     xlab = expression(X[3]), ylab = "Shapley value")
lines(lowess(X[, 3], shap_xgb[, 3, drop = TRUE]), lwd = 2, col = "red2")
plot(X[, 3], shap1[, 3, drop = TRUE], main = "fastshap; ~ 0.468 sec",
     xlab = expression(X[3]), ylab = "Shapley value")
lines(lowess(X[, 3], shap1[, 3, drop = TRUE]), lwd = 2, col = "red2")
plot(X[, 3], shap10[, 3, drop = TRUE], main = "fastshap (nsim = 10); ~ 3.820 sec",
     xlab = expression(X[3]), ylab = "Shapley value")
lines(lowess(X[, 3], shap10[, 3, drop = TRUE]), lwd = 2, col = "red2")
plot(X[, 3], shap50[, 3, drop = TRUE], main = "fastshap (nsim = 50); ~ 18.378 sec",
     xlab = expression(X[3]), ylab = "Shapley value")
lines(lowess(X[, 3], shap50[, 3, drop = TRUE]), lwd = 2, col = "red2")
