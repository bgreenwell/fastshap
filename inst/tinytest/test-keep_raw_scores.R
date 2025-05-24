# Load necessary packages
library(tinytest)
library(fastshap)
library(stats) # For lm

# If testing interactively, you might need to load fastshap manually
# if (!"package:fastshap" %in% search()) {
#   devtools::load_all(".")
# }

# Setup a simple model and prediction wrapper
# Using a subset of mtcars for speed and simplicity
data(mtcars)
mtcars_small <- mtcars[1:5, c("mpg", "wt", "hp")]
features_to_explain <- c("wt", "hp")
X_train <- mtcars_small[, features_to_explain]
X_explain <- mtcars_small[1:2, features_to_explain] # Explain first two rows
X_explain_single_row <- mtcars_small[1, features_to_explain, drop = FALSE]

# Fit a simple linear model
fit <- lm(mpg ~ wt + hp, data = mtcars_small)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)
}

# --- Test Case 1: keep_raw_scores = TRUE, nsim > 1, adjust = FALSE ---
message("Running Test Case 1: keep_raw_scores = TRUE, nsim > 1, adjust = FALSE")
nsim_val <- 3
shap_raw <- explain(fit, 
                    X = X_train, 
                    newdata = X_explain, 
                    pred_wrapper = pfun, 
                    feature_names = features_to_explain,
                    nsim = nsim_val, 
                    adjust = FALSE, 
                    keep_raw_scores = TRUE)

# Assertions for Test Case 1
expect_true(is.list(shap_raw), info = "TC1: shap_raw should be a list")
expect_equal(length(shap_raw), length(features_to_explain), 
             info = "TC1: Length of list should match number of features")
expect_names(names(shap_raw), "identical.to", features_to_explain, 
             info = "TC1: List element names should match feature names")

for (feature in features_to_explain) {
  expect_true(is.matrix(shap_raw[[feature]]), 
              info = paste("TC1: Element for feature", feature, "should be a matrix"))
  expect_equal(nrow(shap_raw[[feature]]), nrow(X_explain), 
               info = paste("TC1: Rows in matrix for", feature, "should match newdata rows"))
  expect_equal(ncol(shap_raw[[feature]]), nsim_val, 
               info = paste("TC1: Cols in matrix for", feature, "should match nsim"))
}
expect_true(!is.null(attr(shap_raw, "baseline")), info = "TC1: Baseline attribute should be present")
# As per documentation, when keep_raw_scores = TRUE, shap_only is effectively TRUE in terms of output format
expect_true(isTRUE(attr(shap_raw, "shap_only")), info = "TC1: shap_only attribute should be TRUE") 
expect_inherits(shap_raw, "explain", info = "TC1: Output should inherit from 'explain'")


# --- Test Case 2: keep_raw_scores = FALSE (default behavior) ---
message("Running Test Case 2: keep_raw_scores = FALSE")
shap_mean <- explain(fit, 
                     X = X_train, 
                     newdata = X_explain, 
                     pred_wrapper = pfun, 
                     feature_names = features_to_explain,
                     nsim = nsim_val, 
                     adjust = FALSE, 
                     keep_raw_scores = FALSE,
                     shap_only = TRUE) # Explicitly TRUE for matrix output

# Assertions for Test Case 2
expect_true(is.matrix(shap_mean), info = "TC2: shap_mean should be a matrix")
expect_dims(shap_mean, c(nrow(X_explain), length(features_to_explain)),
            info = "TC2: Dimensions should be observations x features")
expect_names(colnames(shap_mean), "identical.to", features_to_explain,
             info = "TC2: Column names should match feature names")
expect_true(!is.null(attr(shap_mean, "baseline")), info = "TC2: Baseline attribute should be present")
expect_inherits(shap_mean, "explain", info = "TC2: Output should inherit from 'explain'")

# --- Test Case 3: keep_raw_scores = TRUE but nsim = 1 ---
message("Running Test Case 3: keep_raw_scores = TRUE, nsim = 1")
shap_nsim1 <- explain(fit, 
                      X = X_train, 
                      newdata = X_explain, 
                      pred_wrapper = pfun, 
                      feature_names = features_to_explain,
                      nsim = 1, 
                      adjust = FALSE, 
                      keep_raw_scores = TRUE,
                      shap_only = TRUE)

# Assertions for Test Case 3
expect_true(is.matrix(shap_nsim1), info = "TC3: shap_nsim1 should be a matrix when nsim = 1")
expect_dims(shap_nsim1, c(nrow(X_explain), length(features_to_explain)),
            info = "TC3: Dimensions should be observations x features")
expect_names(colnames(shap_nsim1), "identical.to", features_to_explain,
             info = "TC3: Column names should match feature names")
expect_true(!is.null(attr(shap_nsim1, "baseline")), info = "TC3: Baseline attribute should be present")
expect_inherits(shap_nsim1, "explain", info = "TC3: Output should inherit from 'explain'")

# --- Test Case 4: keep_raw_scores = TRUE but adjust = TRUE ---
message("Running Test Case 4: keep_raw_scores = TRUE, adjust = TRUE")
# Need nsim > 1 for adjust = TRUE to be meaningful and not error
shap_adjust <- explain(fit, 
                       X = X_train, 
                       newdata = X_explain, 
                       pred_wrapper = pfun, 
                       feature_names = features_to_explain,
                       nsim = nsim_val, # nsim > 1
                       adjust = TRUE, 
                       keep_raw_scores = TRUE, # This should be ignored
                       shap_only = TRUE)

# Assertions for Test Case 4
expect_true(is.matrix(shap_adjust), info = "TC4: shap_adjust should be a matrix when adjust = TRUE")
expect_dims(shap_adjust, c(nrow(X_explain), length(features_to_explain)),
            info = "TC4: Dimensions should be observations x features")
expect_names(colnames(shap_adjust), "identical.to", features_to_explain,
             info = "TC4: Column names should match feature names")
expect_true(!is.null(attr(shap_adjust, "baseline")), info = "TC4: Baseline attribute should be present")
expect_inherits(shap_adjust, "explain", info = "TC4: Output should inherit from 'explain'")

# --- Test Case 5: Single observation in newdata with keep_raw_scores = TRUE ---
message("Running Test Case 5: Single observation, keep_raw_scores = TRUE, nsim > 1, adjust = FALSE")
shap_raw_single_row <- explain(fit, 
                               X = X_train, 
                               newdata = X_explain_single_row, 
                               pred_wrapper = pfun, 
                               feature_names = features_to_explain,
                               nsim = nsim_val, 
                               adjust = FALSE, 
                               keep_raw_scores = TRUE)

# Assertions for Test Case 5
expect_true(is.list(shap_raw_single_row), info = "TC5: shap_raw_single_row should be a list")
expect_equal(length(shap_raw_single_row), length(features_to_explain), 
             info = "TC5: Length of list should match number of features")
expect_names(names(shap_raw_single_row), "identical.to", features_to_explain, 
             info = "TC5: List element names should match feature names")

for (feature in features_to_explain) {
  expect_true(is.matrix(shap_raw_single_row[[feature]]), 
              info = paste("TC5: Element for feature", feature, "should be a matrix"))
  # Based on implementation: matrix(reps, ncol = 1) when is.vector(reps) is true (single row newdata)
  # This means nsim rows and 1 column for each feature's matrix.
  expect_equal(nrow(shap_raw_single_row[[feature]]), nsim_val, 
               info = paste("TC5: Rows in matrix for", feature, "should match nsim for single obs case"))
  expect_equal(ncol(shap_raw_single_row[[feature]]), 1, 
               info = paste("TC5: Cols in matrix for", feature, "should be 1 for single obs case"))
}
expect_true(!is.null(attr(shap_raw_single_row, "baseline")), info = "TC5: Baseline attribute should be present")
expect_true(isTRUE(attr(shap_raw_single_row, "shap_only")), info = "TC5: shap_only attribute should be TRUE")
expect_inherits(shap_raw_single_row, "explain", info = "TC5: Output should inherit from 'explain'")

message("All test cases for keep_raw_scores completed.")

# To run these tests:
# tinytest::run_tests_file("inst/tinytest/test-keep_raw_scores.R")
# Or using devtools:
# devtools::test(filter = "keep_raw_scores")
# Or if the package is installed:
# tinytest::test_package("fastshap")
