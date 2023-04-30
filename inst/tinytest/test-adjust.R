# Exits
if (!requireNamespace("lightgbm", quietly = TRUE)) {
  exit_file("Package 'lightgbm' missing")
}
if (!requireNamespace("xgboost", quietly = TRUE)) {
  exit_file("Package 'xgboost' missing")
}

# Use one of the available (imputed) versions of the Titanic data
titanic <- titanic_mice[[1L]]

# Packages 'lightgbm' and 'xgboost' require numeric values
titanic$survived <- ifelse(titanic$survived == "yes", 1, 0)
titanic$sex <- ifelse(titanic$sex == "male", 1, 0)
    
# Matrix of only predictor values
X <- data.matrix(subset(titanic, select = -survived))

# Passenger who's survival prediction we want to estimate and explain
jack.dawson <- data.matrix(data.frame(
  #survived = 0L,  # in case you haven't seen the movie
  pclass = 3L,  # using `3L` instead of `3` to treat as integer
  age = 20.0,
  # sex = factor("male", levels = c("female", "male")),
  sex = 1L,
  sibsp = 0L,  
  parch = 0L  
))


################################################################################
# Package: lightgbm
################################################################################

params.lgb <- list(
  num_leaves = 4L,
  learning_rate = 0.1,
  objective = "binary"
)

set.seed(1420)  # for reproducibility
bst.lgb <- lightgbm::lightgbm(X, label = titanic$survived, params = params.lgb, 
                              nrounds = 50, verbose = 0)

# Prediction wrapper for computing predicted probability of surviving
pfun.lgb <- function(object, newdata) {  # prediction wrapper
  predict(object, data = newdata, rawscore = TRUE)
}

# Estimates Jack's survival probability
jack.logit.lgb <- pfun.lgb(bst.lgb, newdata = jack.dawson)

# Compute baseline prediction
baseline.lgb <- mean(pfun.lgb(bst.lgb, newdata = X))

diff.lgb <- jack.logit.lgb - baseline.lgb

# Compute per-feature contributions using Tree SHAP
(ex.lgb <- predict(bst.lgb, data = jack.dawson, predcontrib = TRUE))

# Compute feature contributions using MC SHAP using the fastshap package
set.seed(1306)  # for reproducibility
ex.fastshap <- explain(bst.lgb, X = X, nsim = 1000, pred_wrapper = pfun.lgb,
                       newdata = jack.dawson, adjust = TRUE)

# Expectations
expect_equal(sum(ex.fastshap), jack.logit.lgb - baseline.lgb, tolerance = 1e-06)

# Explain a few rows of the training data
X.new <- X[1L:5L, ]
set.seed(2033)  # for reproducibility
ex.new <- explain(bst.lgb, X = X, nsim = 2, pred_wrapper = pfun.lgb,
                  newdata = X.new, adjust = TRUE)  # nsim = 2 here ONLY for speed

# Expectations
expect_equal(rowSums(ex.new), pfun.lgb(bst.lgb, newdata = X.new) - baseline.lgb,
             tolerance = 1e-06)


################################################################################
# Package: xgboost
################################################################################

for (obj in c("binary:logistic", "binary:logitraw")) {
  
  # Set task parameters
  params.xgb <- list(
    max_depth = 2L,
    eta = 0.1,
    objective = obj,
    eval_metric = "logloss"
  )
  
  # Fit model
  set.seed(2020)  # for reproducibility
  bst.xgb <- xgboost::xgboost(X, label = titanic$survived, params = params.xgb, 
                              nrounds = 50, verbose = 0)
  
  # Prediction wrapper for ''xgboost'; output depend on user-specified objective
  pfun.xgb <- function(object, newdata) {  # prediction wrapper
    predict(object, newdata = newdata)
  }
  
  # Estimates Jack's survival probability
  jack.logit.xgb <- pfun.xgb(bst.xgb, newdata = jack.dawson)
  
  # Compute baseline prediction
  baseline.xgb <- mean(pfun.xgb(bst.xgb, newdata = X))
  
  # Compute per-feature contributions using Tree SHAP
  (ex.xgb <- predict(bst.xgb, newdata = jack.dawson, predcontrib = TRUE))
  
  # Compute feature contributions using MC SHAP using the fastshap package
  set.seed(2026)  # for reproducibility
  ex.fastshap <- explain(bst.xgb, X = X, nsim = 1000, pred_wrapper = pfun.xgb,
                         newdata = jack.dawson, adjust = TRUE)
  
  # Expectations
  expect_equal(sum(ex.fastshap), jack.logit.xgb - baseline.xgb, 
               tolerance = 1e-06)
  
}

