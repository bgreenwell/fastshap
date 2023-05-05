# Exits
exit_file("Test not ready")

library(fastshap)
    
# Generate some training data
set.seed(1318)
trn <- gen_friedman(1000)
X <- subset(trn, select = -y)

# Fit an additive ML
fit <- lm(y ~ ., data = trn)

# Prediction wrapper for computing predicted probability of surviving
pfun <- function(object, newdata) {  # prediction wrapper
  predict(object, newdata = newdata)
}

# Generate a new instance to explain
set.seed(1319)
newx <- subset(gen_friedman(1), select = -y)

# Exact Shapley values
ex.exact <- explain(fit, newdata = newx, exact = TRUE)

# Approximate Shapley values
set.seed(1306)  # for reproducibility
ex.fastshap <- explain(fit, X = X, nsim = 10, pred_wrapper = pfun,
                       newdata = newx, adjust = FALSE)

# plot(ex.exact, ex.fastshap)
# abline(0, 1)

