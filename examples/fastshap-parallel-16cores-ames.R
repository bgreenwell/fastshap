# Load required packages
# library(AmesHousing)
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

# Use forking approach
library(doParallel)
registerDoParallel(cores = 16)
set.seed(5038)
system.time({  # estimate run time
  shap <- fastshap(rfo, X = X, pred_wrapper = pfun, nsim = 10, .parallel = TRUE)
})
#     user   system  elapsed 
# 1369.551   56.341  101.875


library(parallel)
cl <- makeCluster(5, type = "PSOCK")  
clusterExport(cl, c("fastshap", "X", "pfun", "rfo"))
clusterEvalQ(cl, {
  library(ranger)
})
set.seed(5038)
system.time({
  res <- parLapply(cl,X = names(X), fun = function(x) {
    fastshap(rfo, feature_names = x, X = X, pred_wrapper = pfun, nsim = 10)
  })
})
stopCluster(cl)
#  user  system elapsed 
# 1.974   0.085 161.037
