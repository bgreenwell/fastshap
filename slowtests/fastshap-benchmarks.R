# Setup ------------------------------------------------------------------------

# Load required packages
library(fastshap)
library(iml)
library(mlbench)
library(ranger)

# Simulate training data
set.seed(101)
trn <- as.data.frame(mlbench.friedman1(3000))
X <- subset(trn, select = -y)

# Fit a random forest
set.seed(102)
rfo <- ranger(y ~ ., data =  trn)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Compute fast (approximate) Shapley values using 10 Monte Carlo repititions
system.time({  # estimate run time
  set.seed(5038)
  shap <- fastshap(rfo, feature_names = names(X), X = X, pred_wrapper = pfun, 
                   nsim = 10, .progress = "text")
})

# Compute fast (approximate) Shapley values using 10 Monte Carlo repititions
library(doParallel)
registerDoParallel(cores = 5)
system.time({  # estimate run time
  set.seed(5038)
  shap <- fastshap(rfo, feature_names = names(X), X = X, pred_wrapper = pfun, 
                   nsim = 10, .parallel = TRUE)
})

# Shap-based feature importance
barplot(apply(shap, MARGIN = 2, FUN = function(x) sum(abs(x))))

# Shap dependence plots
par(mfrow = c(2, 5))
for (feature in names(X)) {
  plot(X[, feature], shap[, feature], col = adjustcolor(1, alpha.f = 0.5),
       xlab = feature, ylab = "Shap")
  lines(lowess(X[, feature], shap[, feature]), lwd = 2, col = "red2")
}


# iml --------------------------------------------------------------------------

# Construct new predictor
mod <- Predictor$new(rfo, data = X, predict.fun = pfun)

# Compute (approximate) Shapley values (uses 10 Monte Carlo repititions)
shapley <- Shapley$new(mod, x.interest = X[1, ], sample.size = 10)
system.time({
  shap_iml <- plyr::ldply(
    .data = seq_len(nrow(X)), 
    .progress = "text", 
    .fun = function(i) {
      shapley$explain(X[i, ]); shapley$results$phi
  })
})



# # Compute (approximate) Shapley values (uses 10 Monte Carlo repititions)
# system.time({
#   shapley_iml <- NULL
#   # for (i in 1:10) {
#   for (i in seq_len(nrow(X))) {
#     message("Computing Shapley values for row ", i, "...")
#     shapley_iml <- rbind(
#       shapley_iml,
#       Shapley$new(mod, x.interest = X[i, ], sample.size = 10)$results$phi
#     )
#   }
# })

par(mfrow = c(2, 5))
for (i in 1:10) {
  plot(X[, i], shapley_iml[, i])
}
