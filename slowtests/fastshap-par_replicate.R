# Load required packages
library(ggplot2)
library(microbenchmark)
library(mlbench)
library(parallel)
library(ranger)

# Simulate training data from the Friedman 1 benchmark problem
set.seed(101)  # for reproducibility
friedman1 <- as.data.frame(mlbench.friedman1(3000))
X <- subset(friedman1, select = -y)  # feature columns only

# Fit a randomf forest to the training data
set.seed(102)  # for reproducibility
rfo <- ranger(y ~ ., data = friedman1)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Benchmark function for base::replicate()
fun1 <- function() {
  replicate(24, {
    fastshap:::shapley_column(rfo, X = X, column = "x.3", pred_wrapper = pfun)
  })
}

# Benchmark function for fastshap::par_replicate() (multicore)
fun2 <- function() {
  par_replicate(24, {
      fastshap:::shapley_column(rfo, X = X, column = "x.3", pred_wrapper = pfun)
  }, type = "multicore", mc.cores = 8)
}

# Set up cluster
cl <- makeCluster(8)
clusterExport(cl, varlist = c("X", "pfun", "rfo", "ranger"))

# Benchmark function for fastshap::par_replicate() (multicore)
fun3 <- function() {
  par_replicate(24, {
    fastshap:::shapley_column(rfo, X = X, column = "x.3", pred_wrapper = pfun)
  }, type = "cluster", cl = cl)
}

# Run benchmark
set.seed(103)  # for reproducibility
mb <- microbenchmark(
  fun1(),
  fun2(),
  fun3(),
  times = 100
)

# Print results
mb

# Plot results
autoplot(mb)

# Good practice
stopCluster(cl)
