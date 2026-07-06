# fastshap ![](reference/figures/logo-fastshap.png)

**fastshap** computes fast, approximate Shapley values for any
supervised learning model in R, using ideas from game theory to
attribute a prediction to its input features.

- **Model-agnostic** — works with any fitted model via a user-supplied
  prediction function (`pred_wrapper`)
- **Exact Shapley values** for linear models
  ([`lm()`](https://rdrr.io/r/stats/lm.html)), and Tree SHAP for
  [xgboost](https://cran.r-project.org/package=xgboost) and
  [lightgbm](https://cran.r-project.org/package=lightgbm)
  (`exact = TRUE`)
- **Main/total/interaction effect decomposition**
  ([`explain_effects()`](https://bgreenwell.github.io/fastshap/reference/explain_effects.md))
  — a deterministic, Monte-Carlo-free diagnostic that is exact for
  models with at most pairwise interactions
- **Fast**: batched predictions (few large `pred_wrapper()` calls rather
  than many small ones), an efficient C++ (Rcpp/RcppArmadillo) core, and
  optional parallel execution across features via
  [foreach](https://cran.r-project.org/package=foreach)
- Plays nicely with
  [shapviz](https://cran.r-project.org/package=shapviz) for waterfall,
  force, importance, and dependence plots

## Installation

**fastshap** is no longer available on CRAN due to CRAN’s stringent and
ever-changing policies. It is now hosted on
[r-universe](https://bgreenwell.r-universe.dev/fastshap), which provides
a reliable alternative for distributing R packages.

``` r

# Latest stable release (recommended)
install.packages("fastshap", repos = c("https://bgreenwell.r-universe.dev", "https://cloud.r-project.org"))

# Or with pak
pak::pak("bgreenwell/fastshap@main")  # latest stable release
pak::pak("bgreenwell/fastshap")       # development version (devel branch)
```

## Quick start

``` r

library(fastshap)

# Fit a projection pursuit regression model
fit <- ppr(mpg ~ ., data = mtcars, nterms = 5)

# Prediction wrapper: takes `object` and `newdata`, returns a numeric vector
pfun <- function(object, newdata) predict(object, newdata = newdata)

# Approximate Shapley values via Monte Carlo simulation
set.seed(101)
shap <- explain(fit, X = subset(mtcars, select = -mpg), nsim = 50,
                pred_wrapper = pfun)
head(shap)

# Deterministic main/total/interaction effect decomposition (no simulation)
eff <- explain_effects(fit, X = subset(mtcars, select = -mpg),
                       pred_wrapper = pfun)
head(eff$shapley_values)
```

## Documentation

- [Package website](https://bgreenwell.github.io/fastshap/) — function
  reference and the [introductory
  vignette](https://bgreenwell.github.io/fastshap/articles/fastshap.html)
- Štrumbelj, E., and Kononenko, I. (2014). “Explaining prediction models
  and individual predictions with feature contributions.” *Knowledge and
  Information Systems*, 41(3), 647–665.
- Lundberg, S. M., et al. (2020). “From local explanations to global
  understanding with explainable AI for trees.” *Nature Machine
  Intelligence*, 2(1), 56–67.

## Development

Development happens on the
[`devel`](https://github.com/bgreenwell/fastshap/tree/devel) branch (the
repository default); `main` holds stable releases, which is what
r-universe builds and the website documents. Please open pull requests
against `devel` and report bugs via the [issue
tracker](https://github.com/bgreenwell/fastshap/issues).
