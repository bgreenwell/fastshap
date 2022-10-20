# Exits
if (!requireNamespace("AmesHousing", quietly = TRUE)) {
  exit_file("Package AmesHousing missing")
}

# Read in the data and clean it up a bit
ames <- as.data.frame(AmesHousing::make_ames())

# Features only
X <- subset(ames, select = -Sale_Price)

# Check column classes
table(unlist(sapply(X, class)))

# Fit a linear regression model
fit <- lm(Sale_Price ~ ., data = ames)

# Define prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)
}

# Compute predictions
preds <- pfun(fit, newdata = X)

# Observation to explain
x <- X[which.max(preds), , drop = FALSE]

# Explain x using a single replication
set.seed(954)  # for reproducibility
ex0 <- fastshap::explain(fit, X = X, newdata = x, pred_wrapper = pfun)

# Explain x using 10000 replications with adjustment
set.seed(955)  # for reproducibility
ex1 <- fastshap::explain(fit, X = X, newdata = x, pred_wrapper = pfun,
                         nsim = 100, adjust = TRUE)

# Explain x exactly 
ex2 <- fastshap::explain(fit, exact = TRUE, newdata = x)

# Compare results
plot(as.numeric(ex1), as.numeric(ex2)); abline(0, 1)

# Expectations
expect_true(cor(as.numeric(ex1), as.numeric(ex2)) > 0.999)

# predictor <- iml::Predictor$new(
#   model = fit, 
#   data = ames, 
#   y = "Sale_Price", 
#   predict.fun = pfun
# )
# system.time({
#   set.seed(837)
#   ex_iml <- iml::Shapley$new(predictor, x.interest = x, sample.size = 1000)
# })
# #    user  system elapsed 
# # 407.262  10.841 122.310 
# 
# system.time({
#   set.seed(843)
#   ex_fastshap <- fastshap::explain(fit, X = X, newdata = x, pred_wrapper = pfun,
#                                    nsim = 1000, adjust = TRUE)
# })
# #   user  system elapsed 
# # 11.430   0.501  11.977 
