
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fastshap

<!-- badges: start -->

<!-- badges: end -->

The goal of **fastshap** is to provide an efficient way to compute the
approximate Shapley values discussed in [Section 5.8.3.3 of Christoph
Molnar’s IML
book](https://christophm.github.io/interpretable-ml-book/shapley.html).

**WARNING:** This package is a work in progress and the speed is likely
to improve further over time.

## Installation

You can install the development version of **fastshap** from GitHub:

``` r
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("bgreenwell/fastshap")
```

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
#>  99.448   4.989  22.108

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
#>       x.1   x.2    x.3    x.4    x.5      x.6      x.7     x.8      x.9
#>     <dbl> <dbl>  <dbl>  <dbl>  <dbl>    <dbl>    <dbl>   <dbl>    <dbl>
#>  1 -0.521  1.00 -1.06  -0.744 -0.175 -0.0377  -0.0339  -0.0625 -0.0218 
#>  2 -5.74   1.99  0.882  0.349  1.21  -0.0121   0.0201   0.0180  0.0376 
#>  3  1.64   1.44 -1.17  -3.40  -2.14   0.00256  0.0401  -0.113   0.0238 
#>  4  0.181 -2.76 -0.478  3.83   0.781  0.0178  -0.0570  -0.0364  0.0539 
#>  5 -2.33  -2.32 -0.625  2.02   0.408  0.0283   0.101   -0.0446 -0.0316 
#>  6 -0.830  1.35 -0.514 -2.73   1.01   0.0133  -0.0407  -0.0357  0.0800 
#>  7  2.40   1.37  1.52  -4.14  -0.422 -0.00286  0.0221   0.0821  0.00653
#>  8  0.487 -3.19 -0.223  3.80   0.340  0.0925   0.00127 -0.0265  0.0671 
#>  9  1.43   3.11 -1.46   3.37  -1.56   0.00926 -0.0276  -0.0884 -0.0548 
#> 10  0.623 -3.65 -1.56  -4.53   0.582  0.0120  -0.00302 -0.0505 -0.482  
#> # … with 2,990 more rows, and 1 more variable: x.10 <dbl>
```
