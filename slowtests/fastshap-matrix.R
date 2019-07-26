# Load required packages
library(fastshap)
library(microbenchmark)
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
  shap <- fastshap(rfo, X = X, pred_wrapper = pfun, 
                   nsim = 10)
})

# Compute fast (approximate) Shapley values using 10 Monte Carlo repititions
X2 <- data.matrix(X)
system.time({  # estimate run time
  set.seed(5038)
  shap2 <- fastshap(rfo, X = X2, pred_wrapper = pfun, 
                    nsim = 10)
})

# Use forking approach
library(doParallel)
registerDoParallel(cores = 8)
system.time({  # estimate run time
  set.seed(5038)
  shap3 <- fastshap(rfo, feature_names = names(X), X = X2, pred_wrapper = pfun, 
                    nsim = 10, .parallel = TRUE)
})

# Use socket approach
cl <- makeCluster(8)
registerDoParallel(cl)
system.time({  # estimate run time
  set.seed(5038)
  shap4 <- fastshap(rfo, feature_names = names(X), X = X2, pred_wrapper = pfun, 
                    nsim = 10, .parallel = TRUE,
                    .paropts = list(.packages = "ranger", .export = c("X2", "pfun")))
})
stopCluster(cl)

library(parallel)
cl <- makeCluster(5, type = "PSOCK")  
clusterExport(cl, c("fastshap", "X2", "pfun", "rfo"))
clusterEvalQ(cl, {
  library(ranger)
})
system.time({
  res <- parLapply(
    cl,
    X = names(X),
    fun = function(x) {
      fastshap(rfo, feature_names = x, X = X2, pred_wrapper = pfun, nsim = 10)
    }
  )
})
stopCluster(cl)

# Benchmark
fun1 <- function() {
  fastshap(rfo, feature_names = "x.3", X = X, pred_wrapper = pfun)
}
fun2 <- function() {
  fastshap(rfo, feature_names = "x.3", X = X2, pred_wrapper = pfun)
}
mb <- microbenchmark(fun1(), fun2(), times = 100L)
  