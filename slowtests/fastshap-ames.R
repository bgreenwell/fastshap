# Load required packages
library(doParallel)
# library(fastshap)
library(ggplot2)
library(pdp)

# Load the Ames housing data
ames <- as.data.frame(AmesHousing::make_ames())
X <- subset(ames, select = -Sale_Price)  # feature columns only

# Fit a random forest model
set.seed(101)  # for reproducibility
rfo <- ranger::ranger(Sale_Price ~ ., data = ames, importance = "permutation")

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(102)  # for reproducibility
time1 <- system.time(
  shap1 <- fastshap::explain(rfo, X = X, pred_wrapper = pfun, nsim = 10, 
                             .progress = "text")
)
# > time1  # version 0.0.5
#     user   system  elapsed 
# 1069.390   14.840  418.975
#
# Version 0.0.3 reported 347.988 seconds

# Parallel version
registerDoParallel(cores = parallel::detectCores())
set.seed(102)  # for reproducibility
time2 <- system.time(
  shap2 <- fastshap::explain(rfo, X = X, pred_wrapper = pfun, nsim = 10,
                             .parallel = TRUE)
)
# > time2  # version 0.0.5
#     user   system  elapsed 
# 1267.633   14.429  173.389 

# Shapley-based dependence plot
autoplot(shap2, type = "dependence", feature = "Gr_Liv_Area", 
         X = X, alpha = 0.3) + geom_smooth()
