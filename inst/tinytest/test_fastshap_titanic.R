# Exits
if (!requireNamespace("titanic", quietly = TRUE)) {
  exit_file("Package titanic missing")
}

# Read in the data and clean it up a bit
titanic <- titanic::titanic_train
features <- c(
  "Survived",  # passenger survival indicator
  "Pclass",    # passenger class
  "Sex",       # gender
  "Age",       # age
  "SibSp",     # number of siblings/spouses aboard
  "Parch",     # number of parents/children aboard
  "Fare",      # passenger fare
  "Embarked"   # port of embarkation
)
titanic <- titanic[, features]
titanic$Survived <- as.factor(titanic$Survived)
titanic <- na.omit(titanic)

# Features only
X <- subset(titanic, select = -Survived)

# Fit a logistic regression model
fit <- glm(Survived ~ ., data = titanic, family = binomial)
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)
}
x <- X[1L, , drop = FALSE]
ex0 <- fastshap::explain(fit, X = X, newdata = x, pred_wrapper = pfun)
ex1 <- fastshap::explain(fit, X = X, newdata = x, pred_wrapper = pfun,
                         nsim = 10000, adjust = TRUE)
ex2 <- fastshap::explain(fit, exact = TRUE, newdata = x)
plot(as.numeric(ex1), as.numeric(ex2)); abline(0, 1)

# Expectations
expect_true(cor(as.numeric(ex1), as.numeric(ex2)) > 0.999)
