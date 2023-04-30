# Install required packages
pkgs <- c("AmesHousing", "doParallel", "ranger")
required <- setdiff(pkgs, installed.packages()[, "Package"])
install.packages(required)

# Load required packages
library(doParallel)
library(fastshap)
library(ranger)

# Load Ames housing data
ames <- as.data.frame(AmesHousing::make_ames())
X <- subset(ames, select = -Sale_Price)

# Fit a random forest
set.seed(102)
rfo <- ranger(Sale_Price ~ ., data =  ames, write.forest = TRUE)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Without parallelism
set.seed(1706)
system.time({  # estimate run time
  ex.par <- explain(rfo, X = X, pred_wrapper = pfun, nsim = 10)
})
#    user  system elapsed 
# 483.029  39.370 179.009 

# With parallelism
registerDoParallel(cores = 12)
set.seed(5038)
system.time({  # estimate run time
  ex.par <- explain(rfo, X = X, pred_wrapper = pfun, nsim = 10, parallel = TRUE)
})
#    user  system elapsed 
# 576.077  17.517  53.234
