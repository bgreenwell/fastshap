# Exits
if (!requireNamespace("iml", quietly = TRUE)) {
  exit_file("Package 'iml' missing")
}
if (!requireNamespace("lightgbm", quietly = TRUE)) {
  exit_file("Package 'lightgbm' missing")
}

library(fastshap)
library(iml)
library(lightgbm)

# Use one of the available (imputed) versions of the Titanic data
titanic <- titanic_mice[[1L]]

# Package 'lightgbm' requires numeric values
titanic$survived <- ifelse(titanic$survived == "yes", 1, 0)
titanic$sex <- ifelse(titanic$sex == "male", 1, 0)
    
# Matrix of only predictor values
X <- data.matrix(subset(titanic, select = -survived))

params <- list(
  num_leaves = 10L,
  learning_rate = 0.1,
  objective = "binary"
)

set.seed(1420)  # for reproducibility
bst <- lightgbm(X, label = titanic$survived, params = params, nrounds = 45,
                verbose = 0)


# Fit a default random forest
set.seed(1250)  # for reproducibility
rfo <- ranger(survived ~ ., data = titanic, probability = TRUE, 
              respect.unordered.factors = "partition")

# Prediction wrapper for computing predicted probability of surviving
pfun <- function(object, newdata) {  # prediction wrapper
  predict(object, data = data.matrix(newdata), rawscore = TRUE)
}

# Passenger who's survival prediction we want to estimate and explain
jack.dawson <- data.frame(
  #survived = 0L,  # in case you haven't seen the movie
  pclass = 3L,  # using `3L` instead of `3` to treat as integer
  age = 20.0,
  # sex = factor("male", levels = c("female", "male")),
  sex = 1L,
  sibsp = 0L,  
  parch = 0L  
)
jack.dawson <- data.matrix(jack.dawson)

# Estimates Jack's survival probability
(jack.prob <- pfun(bst, newdata = jack.dawson))

# Compute per-feature contributions using Tree SHAP
(ex.lightgbm <- predict(bst, data = jack.dawson, predcontrib = TRUE))

# Compute feature contributions using MC SHAP using the fastshap package
set.seed(1306)  # for reproducibility
ex.fastshap <- explain(bst, X = X, nsim = 100, pred_wrapper = pfun,
                       newdata = x, adjust = FALSE)

# Compute feature contributions using MC SHAP using the iml package
pred <- Predictor$new(bst, data = as.data.frame(X), predict.fun = pfun)
set.seed(1316)  # for reproducibility
ex.iml <- Shapley$new(pred, x.interest = data.frame(jack.dawson), 
                      sample.size = 100)

# Compare results
res <- cbind(
  "lightgbm" = t(ex.lightgbm)[1:5, ],
  "fastshap" = t(ex.fastshap)[, , drop = TRUE],
  "iml" = ex.iml$results$phi
)

# Expectations
expect_true(all(cor(res) > 0.99))

# # Does 'fastshap' seem to converge on 'lightgbm'?
# set.seed(1543)  # for reproducibility
# ex.age <- sapply(1:1000, FUN = function(n) {
#   as.numeric(explain(bst, X = X, nsim = n, pred_wrapper = pfun,
#                      newdata = x, adjust = FALSE, feature_names = "age"))
# })
# palette("Okabe-Ito")
# plot(ex.age, xlab = "MC repetitions", ylab = "Age contribution", las = 1)
# abline(h = ex.lightgbm[, 2L], lwd = 1, col = 2)
# palette("default")
