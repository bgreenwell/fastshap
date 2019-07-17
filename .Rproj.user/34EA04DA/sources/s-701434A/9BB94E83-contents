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
system.time({
  shapley_iml <- NULL
  # for (i in 1:10) {
  for (i in seq_len(nrow(X))) {
    message("Computing Shapley values for row ", i, "...")
    shapley_iml <- rbind(
      shapley_iml,
      Shapley$new(mod, x.interest = X[i, ], sample.size = 10)$results$phi
    )
  }
})

par(mfrow = c(2, 5))
for (i in 1:10) {
  plot(X[, i], shapley_iml[, i])
}


# C++/R ------------------------------------------------------------------------

# Function to compute (approximate) Shapley values for a particular feature 
# column
shapley_column <- function(object, X, column, pred_wrapper) {

  # Generate original and sampled feature instances
  W <- X[sample(nrow(X)), ]  # shuffle rows
  p <- ncol(X)  # number of features
  
  # Generate Frankenstein matrix; same dimension as X and W
  O <- genOMat(X)
  O <- if (column == 1) {
    cbind(TRUE, O)
  } else if (column == p) {
    cbind(O, TRUE)
  } else {
    cbind(O[, 1:(column - 1)], TRUE, O[, column:(p - 1)])
  }
  
  # Generate "Frankenstein" instances from X and W (in one fell swoop)
  #
  # The first data frame (B1) are the instances of interest, but all values in 
  # the order before and including value of feature j are replaced by feature 
  # values from the shuffled observations in W.
  B1 <- B2 <- X
  B1[O] <- X[O]
  B1[!O] <- W[!O]
  O[, column] <- FALSE
  B2[O] <- X[O]
  B2[!O] <- W[!O]
  
  # Return difference in predictions
  pred_wrapper(object, newdata = B1) - pred_wrapper(object, newdata = B2)

}

# Compute (approximate) Shapley values (uses 10 Monte Carlo repititions)
system.time({
  shapley_fast <- NULL
  for (j in seq_len(ncol(X))) {
    message("Computing Shapley values for column ", j, "...")
    phis <- apply(replicate(10, {
      shapley_column(rfo, X = X, column = j, pred_wrapper = pfun)
    }), MARGIN = 1, FUN = mean)
    shapley_fast <- cbind(shapley_fast, phis)
  }
})


par(mfrow = c(2, 5))
for (j in seq_len(ncol(X))) {
  plot(X[, j], shapley_fast[, j], col = adjustcolor(1, alpha.f = 0.5))
}


library(doParallel)
library(foreach)

registerDoParallel(cores = 8)

# Compute (approximate) Shapley values (uses 10 Monte Carlo repititions)
system.time({
  shapley_fast_parallel <- foreach(i = seq_len(ncol(X))) %dopar% {
    apply(replicate(10, {
      shapley_column(rfo, X = X, column = i, pred_wrapper = pfun)
    }), MARGIN = 1, FUN = mean)
  }
})

res <- plyr::laply(seq_len(ncol(X)), .fun = function(x) {
  apply(replicate(10, {
    shapley_column(rfo, X = X, column = x, pred_wrapper = pfun)
  }), MARGIN = 1, FUN = mean)
}, .progress = "text")
