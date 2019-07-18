
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fastshap

<!-- badges: start -->

<!-- badges: end -->

The goal of **fastshap** is to provide an efficient way to compute the
approximate Shapley values discussed in [Section 5.8.3.3 of Christoph
Molnar’s IML
book](https://christophm.github.io/interpretable-ml-book/shapley.html).

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
  shap <- fastshap(rfo, feature_names = names(X), X = X, pred_wrapper = pfun, 
                   nsim = 10)
})
#>    user  system elapsed 
#>  93.127   3.955  19.635

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
fastshap(rfo, feature_names = names(X), X = X, pred_wrapper = pfun, nsim = 10,
         newdata = X[1, , drop = FALSE])
#> # A tibble: 1 x 10
#>     x.1   x.2    x.3   x.4    x.5    x.6   x.7     x.8     x.9    x.10
#>   <dbl> <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl>   <dbl>   <dbl>   <dbl>
#> 1 0.856  1.28 -0.722 -1.67 -0.140 -0.124 0.105 -0.0352 -0.0433 -0.0395

# Explanations for first three observations
fastshap(rfo, feature_names = names(X), X = X, pred_wrapper = pfun, nsim = 10,
         newdata = X[1:3, ])
#> # A tibble: 3 x 10
#>      x.1   x.2    x.3    x.4    x.5     x.6     x.7     x.8     x.9
#>    <dbl> <dbl>  <dbl>  <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1 -0.137 0.846 -1.15  -2.36  -0.592 -0.0762 -0.0130 -0.0213 -0.0420
#> 2 -3.06  2.00   1.31  -0.475  0.846 -0.0266  0.0127 -0.0183 -0.0557
#> 3  1.24  0.894 -0.690 -3.28  -0.829  0.0215  0.0197 -0.0204  0.0337
#> # … with 1 more variable: x.10 <dbl>
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
fastshap(rfo, feature_names = names(X), X = X, pred_wrapper = pfun, nsim = 10, 
         .parallel = TRUE)
#> # A tibble: 3,000 x 10
#>       x.1    x.2    x.3    x.4    x.5      x.6      x.7      x.8      x.9
#>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1 -0.221  0.734 -1.06  -2.50   0.232  0.0866  -0.0749  -0.0514  -0.0395 
#>  2 -2.08   0.162  1.03  -0.937  1.60   0.0621   0.0103  -0.0407   0.0889 
#>  3  2.03   2.02  -0.398 -1.74  -1.13   0.0225  -0.0104  -0.0427  -0.0669 
#>  4  1.09  -3.41  -0.221  6.33   0.819  0.0381   0.119    0.0191  -0.0749 
#>  5 -3.19  -1.39  -0.561  2.31   1.39   0.0173   0.0186   0.00871 -0.0242 
#>  6 -0.626  1.75  -0.693 -1.01   1.13  -0.0249  -0.00738 -0.0245  -0.127  
#>  7  2.07   2.95   1.41  -4.99  -0.982  0.00295  0.0539  -0.0432  -0.0377 
#>  8  0.916 -1.46   0.171  4.57  -1.08  -0.0274   0.0441   0.0461  -0.0883 
#>  9  2.08   1.13  -0.724  4.84  -1.39   0.0156  -0.0392   0.0116   0.00519
#> 10  2.82  -1.90  -0.139 -4.25   0.270 -0.0638  -0.0437  -0.0137  -0.0455 
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
