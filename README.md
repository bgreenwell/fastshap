
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fastshap <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

<!-- badges: end -->

The goal of **fastshap** is to provide an efficient way to compute the
approximate Shapley values discussed in [Section 5.8.3.3 of Christoph
Molnar’s IML
book](https://christophm.github.io/interpretable-ml-book/shapley.html).

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

## General comments

  - The `fastshap()` function was built for efficiency column-wise (in
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
#> Warning: package 'ranger' was built under R version 3.5.2

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

# Compute fast (approximate) Shapley values using 10 Monte Carlo repititions
system.time({  # estimate run time
  set.seed(5038)
  shap <- fastshap(rfo, X = X, pred_wrapper = pfun, nsim = 10)
})
#>    user  system elapsed 
#>  94.672   4.059  20.177

# Results are returned as a tibble (with the additional "shap" class)
shap
#> # A tibble: 3,000 x 10
#>        x.1    x.2     x.3    x.4     x.5      x.6      x.7     x.8      x.9
#>      <dbl>  <dbl>   <dbl>  <dbl>   <dbl>    <dbl>    <dbl>   <dbl>    <dbl>
#>  1  1.44    1.72  -0.641  -2.61  -0.622  -0.116   -3.46e-2  0.132  -0.0323 
#>  2 -2.50    1.32   0.931   0.152  1.48   -0.0499  -7.64e-2  0.0978  0.00546
#>  3  1.27    1.13  -0.647  -3.26  -0.684   0.0865   6.70e-3  0.0186  0.0290 
#>  4  0.331  -2.27  -0.0842  4.12   1.71   -0.0572  -4.23e-4 -0.0348 -0.0191 
#>  5 -1.06   -1.47  -0.537   3.85   1.23   -0.0519  -6.37e-3  0.102   0.0698 
#>  6 -1.32    1.82  -0.994  -2.35   1.12   -0.0366  -1.10e-2 -0.0795 -0.284  
#>  7  1.70    1.85   1.33   -4.95  -0.0251  0.00369  1.11e-1 -0.0315 -0.113  
#>  8  0.0995 -1.97   0.279   3.80  -0.278  -0.0995   4.03e-2 -0.0653  0.0301 
#>  9  2.09    0.847 -1.16    4.55  -0.923   0.0280  -9.44e-2 -0.0471  0.0301 
#> 10  0.865  -2.56  -0.971  -4.84   0.813  -0.0323   1.08e-2 -0.0415 -0.101  
#> # … with 2,990 more rows, and 1 more variable: x.10 <dbl>
```

You can use the results to interpret the model in many different ways.
For example, in the code chunk below we take the sum of the absolute
value of the Shapley values within each feature to construct a
Shap-based feature variable importance plot:

``` r
# Load required packages
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 3.5.2
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

<img src="man/figures/README-shap-importance-1.png" width="70%" />

We can also plot the Shapley values for each feature to construct
Shap-based dependence plots:

``` r
shap_dep_x3 <- data.frame(x3 = X[["x.3"]], shap = shap[["x.3"]])
ggplot(shap_dep_x3, aes(x3, shap)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  ylab("Shapley value")
#> `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

<img src="man/figures/README-shap-dependence-1.png" width="70%" />

You can also use `autoplot()` to construct simple plots:

``` r
p1 <- autoplot(shap)
p2 <- autoplot(shap, type = "dependence", feature = "x.3", X = X)
gridExtra::grid.arrange(p1, p2, nrow = 1)
#> `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

<img src="man/figures/README-shap-autoplot-1.png" width="70%" />

By default, `fastshap()` computes approximate Shapley values for all
rows in the training data. If you want Shapley values for new instances
(or a subset of the training set), they must be supplied via the
`newdata` argument. This functionalaity is demonstrated in the code
chunk below. (**Note:** `fastshap()` is not yet optimized for this case;
that is, computing only a handful of Shapley values for a few instances
(in this case, at least for now, consider using the **iml** function
`Shapley()`).)

``` r
# Explanations for first observation; technically `drop = FALSE` isn't necessary 
# here since X is a data frame
fastshap(rfo, X = X, pred_wrapper = pfun, nsim = 10,
         newdata = X[1, , drop = FALSE])
#> # A tibble: 1 x 10
#>     x.1   x.2    x.3   x.4    x.5    x.6   x.7     x.8     x.9    x.10
#>   <dbl> <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl>   <dbl>   <dbl>   <dbl>
#> 1 0.856  1.28 -0.722 -1.67 -0.140 -0.124 0.105 -0.0352 -0.0433 -0.0395

# Explanations for first three observations
fastshap(rfo, X = X, feature_names = c("x.1", "x.10"), pred_wrapper = pfun, 
         nsim = 10, newdata = X[1:3, ])
#> # A tibble: 3 x 2
#>      x.1    x.10
#>    <dbl>   <dbl>
#> 1 -0.137 -0.0866
#> 2 -3.06  -0.0477
#> 3  1.24  -0.0379
```

### Parallel execution

Since **fastshap** uses the **plyr** package under the hood, you can use
any parallel backend supported by the **foreach** package. This is
illustrated in the code chunk below.

``` r
# Load required packages
library(doParallel)
#> Loading required package: foreach
#> Loading required package: iterators
#> Loading required package: parallel

# Set up parallel backend
registerDoParallel(5)

# Compute Shapley values in parallel
fastshap(rfo, X = X, pred_wrapper = pfun, nsim = 10, .parallel = TRUE)
#> # A tibble: 3,000 x 10
#>       x.1    x.2    x.3    x.4    x.5     x.6      x.7      x.8      x.9
#>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>    <dbl>    <dbl>    <dbl>
#>  1  1.59   1.20  -1.03  -2.56   0.307 -0.0612 -0.0759   0.0206  -0.0149 
#>  2 -3.17   1.09   0.197  0.254  1.06  -0.0196  0.0382  -0.00437  0.0919 
#>  3  1.06   1.92  -0.760 -2.47  -1.38   0.0597  0.00346  0.0108  -0.0188 
#>  4  1.22  -3.40  -0.455  4.27   1.56  -0.0313 -0.0215   0.0561   0.00103
#>  5 -1.48  -0.650 -0.637  2.38   1.46  -0.0159  0.175    0.00629  0.0535 
#>  6 -2.18   1.49  -0.943 -1.54   1.27  -0.0965 -0.112   -0.0719  -0.0921 
#>  7  0.755  1.49   0.811 -4.17  -0.684 -0.0396  0.0209   0.0215   0.0135 
#>  8  0.455 -2.71  -0.129  2.42  -0.155  0.0391  0.0957  -0.0353  -0.0139 
#>  9  2.03   1.02  -0.314  3.39  -1.16  -0.0620 -0.0222   0.0614  -0.0327 
#> 10  1.89  -1.56  -0.935 -2.68   0.821 -0.0363  0.0218  -0.0361  -0.0885 
#> # … with 2,990 more rows, and 1 more variable: x.10 <dbl>
```

## Comparison with TreeSHAP/TreeExplainer for XGBoost models

You can compute the contributions of each feature for XGBoost models in
an efficient way using the methods described in (Lundberg 2017). These
are available through the `predict()` function for **xgboost** models;
see `?xgboost::predict.xgb.Booster` for details. Below we compute the
contributions for each feature using both methods and compare the
results using a Shapley-based dependence plot on feature `x.3`, the
results are quite surprising (**no parallel processing was used to
obtain the **fastshap** results**). And remember, **fastshap** can be
used with any prediction model in R. You can see the code that generated
these benchmarks in the `slowtests/xgboost.R` file
[here](https://github.com/bgreenwell/fastshap/blob/master/slowtests/xgboost.R).

![](slowtests/treeshap-comparison.png)

## References

Scott M. Lundberg, Su-In Lee, “A Unified Approach to Interpreting Model
Predictions”, NIPS Proceedings 2017, <https://arxiv.org/abs/1705.07874>

Scott M. Lundberg, Su-In Lee, “Consistent feature attribution for tree
ensembles”, <https://arxiv.org/abs/1706.06060>
