# TODO:
#
# * Convert to a {tinytest} script.


library(fastshap)
library(ggplot2)
library(lightgbm)


# Simulate (numeric) training data
trn <- gen_friedman(seed = 1548)

# Create feature-only matrix
X <- data.matrix(subset(trn, select = -y))

# Fit a LightGBM model
set.seed(1615)  # for reproducibility
bst <- lightgbm(X, label = trn$y, num_leaves = 4L, learning_rate = 0.3, 
                nrounds = 200L, objective = "regression")

# Compute feature contributions from LightGBM directly
p <- predict(bst, data = X, predcontrib = TRUE)

# Extract feature contributions using fastshap
ex <- explain(bst, X = X, exact = TRUE)

# Some plots
p1 <- autoplot(ex)
p2 <- autoplot(ex, type = "dependence", feature = "x1", color_by = "x2",
               X = as.data.frame(X))
p3 <- autoplot(ex, type = "contribution", row_num = 1)
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
