# Load required packages
library(doParallel)
library(fastshap)
library(pdp)
library(ranger)
library(vip)

# Load the Ames housing data
ames <- as.data.frame(AmesHousing::make_ames())
X <- subset(ames, select = -Sale_Price)  # feature columns only

# Fit a random forest model
set.seed(101)  # for reproducibility
rfo <- ranger(Sale_Price ~ ., data = ames)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(102)  # for reproducibility
time1 <- system.time(
  shap1 <- fastshap(rfo, X = X, pred_wrapper = pfun, nsim = 10, 
                    .progress = "text")
)

# Parallel version
registerCores(cores = parallel::detectCores())
set.seed(102)  # for reproducibility
time2 <- system.time(
  shap2 <- fastshap(rfo, X = X, pred_wrapper = pfun, nsim = 10,
                    .parallel = TRUE)
)
