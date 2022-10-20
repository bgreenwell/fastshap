library(fastshap)
library(iml)
library(iBreakDown)
library(ranger)

# Use one of fastshap's imputed versions of the Titanic data
head(titanic <- titanic_mice[[1L]])

# Matrix of only predictor values
X <- subset(titanic, select = -survived)

# Fit a default probability forest
set.seed(1420)  # for reproducibility
rfo <- ranger(survived ~ ., data = titanic, probability = TRUE)

jack.dawson <- data.frame(
  pclass = 3L,     # third-class passenger
  age = 20.0,      # twenty years old
  sex = factor("male", levels = c("female", "male")),
  sibsp = 0L,      # no siblings/spouses aboard
  parch = 0L       # no parents/children aboard
)

# Prediction wrapper to compute predcited probability of survival
pfun <- function(object, newdata) {  # prediction wrapper
  predict(object, data = newdata)$predictions[, "yes"]
}

# Compute Jack's predicted likelihood of survival
(jack.prob <- pfun(rfo, newdata = jack.dawson))  # probability scale

# DALEX-based helper for iBreakDown
explainer <- DALEX::explain(rfo, data = X, y = titanic$survived, 
                            predict_function = pfun, verbose = FALSE)

# Helper for iml
predictor <- iml::Predictor$new(rfo, data = titanic, y = "survived",
                                predict.fun = pfun)

nsims <- c(1, 5, 10, 25, 50, 75, 100, seq(from = 200, to = 1000, by = 100))
times1 <- times2 <- times3 <- numeric(length(nsims))
set.seed(904)
for (i in seq_along(nsims)) {
  message("nsim = ", nsims[i], "...")
  times1[i] <- system.time({
    iBreakDown::shap(explainer, B = nsims[i], new_observation = jack.dawson)
  })["elapsed"]
  times2[i] <- system.time({
    iml::Shapley$new(predictor, x.interest = jack.dawson, sample.size = nsims[i])
  })["elapsed"]
  times3[i] <- system.time({
    fastshap::explain(rfo, X = X, newdata = jack.dawson, pred_wrapper = pfun, 
                      nsim = nsims[i])
  })["elapsed"]
}
benchmark <- data.frame(
  "nreps" = nsims,
  "iBreakDown" = times1,
  "iml" = times2,
  "fastshap" = times3
)
saveRDS(benchmark, file = "rjarticle/data/benchmark.rds")
plot(nsims, times1, type = "b", xlab = "Number of Monte Carlo repetitions",
     ylab = "Time (in seconds)", las = 1,
     xlim = c(0, max(nsims)), ylim = c(0, max(times1, times2, times3)))
lines(nsims, times2, type = "b", col = "red")
lines(nsims, times3, type = "b", col = "blue")
legend("topleft",
       legend = c("iBreakDown", "iml", "fastshap"),
       lty = c(1, 1, 1), col = c("black", "red", "blue"), inset = 0.02)