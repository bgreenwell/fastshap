# The purpose of this script is to test the functioning of the beeswarm plot

# Load required packages
library(doParallel)
library(fastshap)
library(ggplot2)
library(ranger)

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
  shap <- fastshap::explain(rfo, X = X, nsim = 50, pred_wrapper = pfun)
)
#   user  system elapsed 
# 81.318   7.091  19.778

p <- autoplot(object = shap, type = "beeswarm", X = boston)
p