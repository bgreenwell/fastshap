
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fastshap <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/bgreenwell/fastshap.svg?branch=master)](https://travis-ci.org/bgreenwell/fastshap)
[![Codecov test
coverage](https://codecov.io/gh/bgreenwell/fastshap/branch/master/graph/badge.svg)](https://codecov.io/gh/bgreenwell/fastshap?branch=master)
[![Launch Rstudio
Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/bgreenwell/fastshap/master?urlpath=rstudio)
<!-- badges: end -->

The goal of **fastshap** is to provide an efficient way to compute the
approximate Shapley values discussed in [Molnar
(2019)](https://christophm.github.io/interpretable-ml-book/shapley.html).

![](https://media.giphy.com/media/26AHLNr8en8J3ovOo/giphy.gif)

**WARNING:** This package is a work in progress and the speed is likely
to improve further over time (🤞).

## Installation

You can install the development version of **fastshap** from GitHub:

``` r
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("bgreenwell/fastshap")
```

## Background

The approach in this package is similar to what’s described in
**Algorithm 1** in Strumbelj and Kononenko (2014) which is reproduced
below:

<img src="man/figures/algorithm1.png" width="100%" style="display: block; margin: auto;" />

The problem with this approach is that it requires many calls to the
scoring function *f()*. In particular, if we have *N* training records
and *p* features, than this algorithm would require *2mnp* calls to
*f()* in order to obtain approximate Shapley values for the entire
training set. The approach we take is similar, but rather than computing
each Monte Carlo estimate one at a time, we construct all the data
required to compute the approximate Shapley values for a single feature
at once and only use two calls to *f()*. Using this approach only
requires *2mp* calls to *f()* to obtain approximate Shapley values for
all *p* features for the entire training set. Hence, this approach will
scale better to larger training sets. Also, the data instances
**b<sub>1</sub>** and **b<sub>2</sub>** are built efficiently for each
row in the training set (or subset thereof) all at once and stacked on
top of each other in a data frame or matrix using C++ and logical
subsetting. We can also parallelize the algorithm across *m* or *p*,
depending on which one is more beneficial.

## General comments

  - The `explain()` function was built for efficiency column-wise (in
    other words, it is not currently optimized if all you need are the
    Shapley values for a few rows)

## Example

The following example demonstrates the basic usage of the **fastshap**
package.

``` r
# Load required packages
library(fastshap)  # for fast (approximate) Shapley values
library(mlbench)   # for Friedman 1 benchmark data set
library(ranger)    # for fast random forest algorithm

# Simulate training data
set.seed(101)
trn <- as.data.frame(mlbench.friedman1(3000))
X <- subset(trn, select = -y)  # feature columns only

# Fit a random forest
set.seed(102)
rfo <- ranger(y ~ ., data =  trn)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Compute fast (approximate) Shapley values using 10 Monte Carlo repetitions
system.time({  # estimate run time
  set.seed(5038)
  shap <- explain(rfo, X = X, pred_wrapper = pfun, nsim = 10)
})
#>    user  system elapsed 
#>  96.470   4.071  20.284

# Results are returned as a tibble (with the additional "shap" class)
shap
#> # A tibble: 3,000 x 10
#>       x.1    x.2     x.3    x.4    x.5      x.6      x.7      x.8     x.9
#>     <dbl>  <dbl>   <dbl>  <dbl>  <dbl>    <dbl>    <dbl>    <dbl>   <dbl>
#>  1 -0.545  1.12  -1.68   -1.70  -0.281 -0.0953  -1.55e-2 -0.00274 -0.0435
#>  2 -3.20   1.05   1.03   -0.815  0.786 -0.00719  1.02e-1  0.114    0.0788
#>  3  2.07   2.36  -0.823  -2.92  -1.53  -0.0402   7.70e-3  0.00655  0.0435
#>  4  0.623 -2.78   0.0688  3.17   0.753  0.0816   9.27e-4  0.0741   0.0564
#>  5 -0.644 -0.258 -0.207   3.12   0.918 -0.0159   3.83e-2  0.0155   0.0315
#>  6 -0.929  0.506 -1.05   -1.70   1.51   0.0299   5.41e-2  0.0118  -0.234 
#>  7  2.99   2.48   1.23   -3.46  -0.398  0.0639  -6.17e-3 -0.0237  -0.0353
#>  8 -0.388 -1.84   0.532   4.64  -0.487 -0.0215   2.77e-2 -0.0346   0.0496
#>  9  0.647  0.783 -0.769   3.35  -1.13   0.0171   3.70e-3 -0.0348   0.0866
#> 10  0.832 -1.22  -0.403  -4.19   0.560 -0.0247  -1.82e-2 -0.00990 -0.0256
#> # … with 2,990 more rows, and 1 more variable: x.10 <dbl>
```

You can use the results to interpret the model in many different ways.
For example, in the code chunk below we take the sum of the absolute
value of the Shapley values within each feature to construct a
Shap-based feature variable importance plot:

``` r
# Load required packages
library(ggplot2)
theme_set(theme_bw())

# Aggregate Shapley values
shap_imp <- data.frame(
  Variable = names(shap),
  Importance = apply(shap, MARGIN = 2, FUN = function(x) sum(abs(x)))
)

# Plot Shap-based variable importance
ggplot(shap_imp, aes(reorder(Variable, Importance), Importance)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("mean(|Shapley value|)")
```

<img src="man/figures/README-shap-importance-1.png" width="70%" style="display: block; margin: auto;" />

We can also plot the Shapley values for each feature to construct
Shap-based dependence plots:

``` r
shap_dep_x3 <- data.frame(x3 = X[["x.3"]], shap = shap[["x.3"]])
ggplot(shap_dep_x3, aes(x3, shap)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  ylab("Shapley value")
```

<img src="man/figures/README-shap-dependence-1.png" width="70%" style="display: block; margin: auto;" />

You can also use `autoplot()` to construct simple plots:

``` r
p1 <- autoplot(shap)
p2 <- autoplot(shap, type = "dependence", feature = "x.3", X = X, alpha = 0.5,
               color_by = "x.2", smooth = TRUE, smooth_color = "black") +
        scale_color_viridis_c()
gridExtra::grid.arrange(p1, p2, nrow = 1)
```

<img src="man/figures/README-shap-autoplot-1.png" width="70%" style="display: block; margin: auto;" />

By default, `explain()` computes approximate Shapley values for all rows
in the training data. If you want Shapley values for new instances (or a
subset of the training set), they must be supplied via the `newdata`
argument. This functionality is demonstrated in the code chunk below.
(**Note:** `explain()` is not yet optimized for this case; that is,
computing only a handful of Shapley values for a few instances (in this
case, at least for now, consider using the **iml** function
`Shapley()`).)

``` r
# Explanations for first observation; technically `drop = FALSE` isn't necessary 
# here since X is a data frame
explain(rfo, X = X, pred_wrapper = pfun, nsim = 10,
        newdata = X[1, , drop = FALSE])
#> # A tibble: 1 x 10
#>       x.1   x.2    x.3   x.4    x.5     x.6     x.7     x.8     x.9
#>     <dbl> <dbl>  <dbl> <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1 -0.0314  1.37 -0.757 -1.83 -0.230 -0.0351 -0.0225 -0.0148 -0.0415
#> # … with 1 more variable: x.10 <dbl>

# Explanations for first three observations
explain(rfo, X = X, feature_names = c("x.1", "x.10"), pred_wrapper = pfun, 
        nsim = 10, newdata = X[1:3, ])
#> # A tibble: 3 x 2
#>      x.1    x.10
#>    <dbl>   <dbl>
#> 1  0.467  0.0384
#> 2 -4.05  -0.0469
#> 3  1.72   0.0630
```

### Parallel execution

Since **fastshap** uses the **plyr** package under the hood, you can use
any parallel backend supported by the **foreach** package. This is
illustrated in the code chunk below.

``` r
# Load required packages
library(doParallel)

# Set up parallel backend
registerDoParallel(5)

# Compute Shapley values in parallel
explain(rfo, X = X, pred_wrapper = pfun, nsim = 10, .parallel = TRUE)
#> # A tibble: 3,000 x 10
#>       x.1    x.2    x.3    x.4    x.5      x.6      x.7      x.8      x.9
#>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1 -0.787  0.428 -0.390 -3.75  -0.343 -7.96e-2 -0.0843  -0.0787   0.0178 
#>  2 -4.38   1.01   0.848  0.961  1.61  -5.13e-2 -0.0157  -0.0540   0.141  
#>  3  1.60   2.94  -1.16  -1.64  -1.55   1.63e-2  0.0340  -0.0177   0.0923 
#>  4  1.25  -0.795 -0.207  4.53   1.16  -2.13e-2  0.0321   0.00310 -0.0711 
#>  5 -1.11  -0.631  0.126  1.67   1.30   2.30e-3  0.0346   0.0193   0.0174 
#>  6  0.434  1.86  -0.816 -1.67   1.17   1.49e-4  0.00866 -0.0882  -0.0904 
#>  7  1.14   1.47   1.47  -2.83  -0.742  9.73e-2  0.0208  -0.0577   0.00289
#>  8 -0.677 -1.58   0.112  3.28   0.488  3.40e-2 -0.00592 -0.00665  0.0887 
#>  9  2.09   3.85  -0.770  4.78  -1.71   6.73e-4 -0.104   -0.0392   0.0104 
#> 10  0.666 -3.48  -1.23  -4.26   1.13  -6.78e-2 -0.0342  -0.0742  -0.0317 
#> # … with 2,990 more rows, and 1 more variable: x.10 <dbl>
```

## Comparison with TreeSHAP/TreeExplainer for XGBoost models

You can compute the contributions of each feature for XGBoost models in
an efficient way using the methods described in
[(Lundberg 2017)](https://arxiv.org/abs/1705.07874). These are available
through the `predict()` function for **xgboost** models; see
`?xgboost::predict.xgb.Booster` for details. Below we compute the
contributions for each feature using both methods and compare the
results using a Shapley-based dependence plot on feature `x.3`, the
results are quite surprising (**no parallel processing was used to
obtain the **fastshap** results**). And remember, **fastshap** can be
used with any prediction model in
R.

<img src="man/figures/README-fastshap-comparison-1.png" width="70%" style="display: block; margin: auto;" />

We can also check that **fastshap** converges to the true Shapley values
by comparing the results to TreeSHAP while varying the number of Monte
Carlo
repetitions:

<img src="man/figures/README-fastshap-convergence-1.png" width="70%" style="display: block; margin: auto;" />

## References

Scott M. Lundberg, Su-In Lee, “A Unified Approach to Interpreting Model
Predictions”, NIPS Proceedings 2017, <https://arxiv.org/abs/1705.07874>.

Scott M. Lundberg, Su-In Lee, “Consistent feature attribution for tree
ensembles”, <https://arxiv.org/abs/1706.06060>.

Christoph Molnar, *Interpretable Machine Learning*. 2019.
<https://christophm.github.io/interpretable-ml-> book/.
