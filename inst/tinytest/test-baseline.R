exit_if_not(requireNamespace("lightgbm", quietly = TRUE))

# Use one of the available (imputed) versions of the Titanic data
titanic <- fastshap::titanic_mice[[1L]]

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

# lightgbm paramaters
params.lgb <- list(
  num_leaves = 4L,
  learning_rate = 0.1,
  objective = "binary",
  force_row_wise = TRUE
)

# Fit a LightGBM model
set.seed(1420)  # for reproducibility
bst.lgb <- lightgbm::lightgbm(
  data = X, 
  label = titanic$survived, 
  params = params.lgb, 
  nrounds = 50, 
  verbose = 0, 
  save_name = "lightgbm.model"
)

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
ex.fastshap <- fastshap::explain(bst.lgb, X = X, nsim = 1000, 
                                 pred_wrapper = pfun.lgb, ewdata = jack.dawson, 
                                 adjust = TRUE, shap_only = FALSE)

# Expect Shapley values to have additivity property
expect_equal(sum(ex.fastshap$shapley_values), jack.logit.lgb - baseline.lgb, 
             tolerance = 1e-06)

# Expect baselines to be the same
expect_equal(ex.fastshap$baseline, ex.lgb[1L, 6L])

# Compute feature contributions with a different baseline
set.seed(1308)  # for reproducibility
ex.fastshap.baseline <- fastshap::explain(
  object = bst.lgb, 
  X = X, 
  nsim = 1000, 
  pred_wrapper = pfun.lgb,
  baseline = -0.6,
  newdata = jack.dawson, 
  adjust = TRUE, 
  shap_only = FALSE
)

# Expect Shapley values to have additive property with specified baseline
expect_equal(sum(ex.fastshap.baseline$shapley_values), jack.logit.lgb + 0.6, 
             tolerance = 1e-06)

# Expect baseline to equal what was given
expect_equal(ex.fastshap.baseline$baseline, -0.6)
