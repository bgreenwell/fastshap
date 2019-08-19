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
rfo <- ranger(Sale_Price ~ ., data = ames, importance = "permutation")

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
registerDoParallel(cores = parallel::detectCores())
set.seed(102)  # for reproducibility
time2 <- system.time(
  shap2 <- fastshap(rfo, X = X, pred_wrapper = pfun, nsim = 10,
                    .parallel = TRUE)
)

library(ggplot2)

shap3 <- reshape2::melt(shap2, measure.vars = names(shap2),
                        variable.name = "Variable", value.name = "SHAP")


shap_vi <- data.frame(
  Variable = names(shap2),
  Importance = apply(shap2, MARGIN = 2, FUN = function(x) mean(abs(x)))
)

shap4 <- merge(shap3, shap_vi, by = "Variable")

ggplot(shap4, aes(x = SHAP, y = reorder(Variable, Importance))) +
  ggbeeswarm::geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, 
                               size = 0.4, alpha = 0.25) +
  xlab("SHAP value") +
  ylab(NULL)
