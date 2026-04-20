# Fast approximate Shapley values

Compute fast (approximate) Shapley values for a set of features using
the Monte Carlo algorithm described in Strumbelj and Igor (2014). An
efficient algorithm for tree-based models, commonly referred to as Tree
SHAP, is also supported for
[lightgbm](https://cran.r-project.org/package=lightgbm) and
[xgboost](https://cran.r-project.org/package=xgboost) models; see
Lundberg et. al. (2020) for details.

## Usage

``` r
explain(object, ...)

# Default S3 method
explain(
  object,
  feature_names = NULL,
  X = NULL,
  nsim = 1,
  pred_wrapper = NULL,
  newdata = NULL,
  adjust = FALSE,
  baseline = NULL,
  shap_only = TRUE,
  parallel = FALSE,
  raw = FALSE,
  seed = NULL,
  ...
)

# S3 method for class 'lm'
explain(
  object,
  feature_names = NULL,
  X,
  nsim = 1,
  pred_wrapper,
  newdata = NULL,
  adjust = FALSE,
  exact = FALSE,
  baseline = NULL,
  shap_only = TRUE,
  parallel = FALSE,
  ...
)

# S3 method for class 'xgb.Booster'
explain(
  object,
  feature_names = NULL,
  X = NULL,
  nsim = 1,
  pred_wrapper,
  newdata = NULL,
  adjust = FALSE,
  exact = FALSE,
  baseline = NULL,
  shap_only = TRUE,
  parallel = FALSE,
  ...
)

# S3 method for class 'lgb.Booster'
explain(
  object,
  feature_names = NULL,
  X = NULL,
  nsim = 1,
  pred_wrapper,
  newdata = NULL,
  adjust = FALSE,
  exact = FALSE,
  baseline = NULL,
  shap_only = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- object:

  A fitted model object (e.g., a
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md),
  [`xgboost::xgboost()`](https://rdrr.io/pkg/xgboost/man/xgboost.html),
  or [`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html)
  object, to name a few).

- ...:

  Additional optional arguments to be passed on to
  [`foreach::foreach()`](https://rdrr.io/pkg/foreach/man/foreach.html)
  whenever `parallel = TRUE`. For example, you may need to supply
  additional packages that the parallel task depends on via the
  `.packages` argument to
  [`foreach::foreach()`](https://rdrr.io/pkg/foreach/man/foreach.html).
  **NOTE:**
  [`foreach::foreach()`](https://rdrr.io/pkg/foreach/man/foreach.html)'s
  `.combine` argument is already set internally by `explain()`, so
  passing it via the `...` argument would likely result in an error.

- feature_names:

  Character string giving the names of the predictor variables (i.e.,
  features) of interest. If `NULL` (default) they will be taken from the
  column names of `X`.

- X:

  A matrix-like R object (e.g., a data frame or matrix) containing ONLY
  the feature columns from the training data (or suitable background
  data set). **NOTE:** This argument is required whenever
  `exact = FALSE`.

- nsim:

  The number of Monte Carlo repetitions to use for estimating each
  Shapley value (only used when `exact = FALSE`). Default is 1.
  **NOTE:** To obtain the most accurate results, `nsim` should be set as
  large as feasibly possible.

- pred_wrapper:

  Prediction function that requires two arguments, `object` and
  `newdata`. **NOTE:** This argument is required whenever
  `exact = FALSE`. The output of this function should be determined
  according to:

  Regression

  :   A numeric vector of predicted outcomes.

  Binary classification

  :   A vector of predicted class probabilities for the reference class.

  Multiclass classification

  :   A vector of predicted class probabilities for the reference class.

- newdata:

  A matrix-like R object (e.g., a data frame or matrix) containing ONLY
  the feature columns for the observation(s) of interest; that is, the
  observation(s) you want to compute explanations for. Default is `NULL`
  which will produce approximate Shapley values for all the rows in `X`
  (i.e., the training data).

- adjust:

  Logical indicating whether or not to adjust the sum of the estimated
  Shapley values to satisfy the *local accuracy* property; that is, to
  equal the difference between the model's prediction for that sample
  and the average prediction over all the training data (i.e., `X`).
  Default is `FALSE` and setting to `TRUE` requires `nsim` \> 1.

- baseline:

  Numeric baseline to use when adjusting the computed Shapley values to
  achieve *local accuracy*. Adjusted Shapley values for a single
  prediction (`fx`) will sum to the difference `fx - baseline`. Defaults
  to `NULL`, which corresponds to the average predictions computed from
  `X`, and zero otherwise (i.e., no additional predictions will be
  computed and the baseline attribute of the output will be set to
  zero).

- shap_only:

  Logical indicating whether or not to include additional output useful
  for plotting (i.e., `newdata` and the `baseline` value.). This is
  convenient, for example, when using
  [`shapviz::shapviz()`](https://modeloriented.github.io/shapviz/reference/shapviz.html)
  for plotting. Default is `TRUE`.

- parallel:

  Logical indicating whether or not to compute the approximate Shapley
  values in parallel across features; default is `FALSE`. **NOTE:**
  setting `parallel = TRUE` requires setting up an appropriate (i.e.,
  system-specific) *parallel backend* as described in the
  [foreach](https://cran.r-project.org/package=foreach); for details,
  see
  [`vignette("foreach", package = "foreach")`](https://cran.rstudio.com/web/packages/foreach/vignettes/foreach.html)
  in R.

- raw:

  Logical indicating whether or not to return the raw per-simulation
  Shapley values from each Monte Carlo replication. If `TRUE`, a 3-D
  array of dimensions `n x p x nsim` is returned, where `n` is the
  number of observations, `p` is the number of features, and `nsim` is
  the number of Monte Carlo replications; for example,
  `apply(result, 1:2, sd)` computes standard errors for each
  (observation, feature) pair. Only supported when `adjust = FALSE`.
  Default is `FALSE`.

- seed:

  Integer specifying a random seed for reproducibility; passed to
  [`base::set.seed()`](https://rdrr.io/r/base/Random.html). Default is
  `NULL` (no seed).

- exact:

  Logical indicating whether to compute exact Shapley values. Currently
  only available for [`stats::lm()`](https://rdrr.io/r/stats/lm.html),
  [`xgboost::xgboost()`](https://rdrr.io/pkg/xgboost/man/xgboost.html),
  and
  [`lightgbm::lightgbm()`](https://rdrr.io/pkg/lightgbm/man/lightgbm.html)
  objects. Default is `FALSE`. Note that setting `exact = TRUE` will
  return explanations for each of the
  [`stats::terms()`](https://rdrr.io/r/stats/terms.html) in an
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) object. Default is
  `FALSE`.

## Value

If `shap_only = TRUE` (the default), a matrix is returned with one
column for each feature specified in `feature_names` (if
`feature_names = NULL`, the default, there will be one column for each
feature in `X`) and one row for each observation in `newdata` (if
`newdata = NULL`, the default, there will be one row for each
observation in `X`). Additionally, the returned matrix will have an
attribute called `"baseline"` containing the baseline value. If
`shap_only = FALSE`, then a list is returned with three components:

- `shapley_values` - a matrix of Shapley values (as described above);

- `feature_values` - the corresponding feature values (for plotting with
  [`shapviz::shapviz()`](https://modeloriented.github.io/shapviz/reference/shapviz.html));

- `baseline` - the corresponding baseline value (for plotting with
  [`shapviz::shapviz()`](https://modeloriented.github.io/shapviz/reference/shapviz.html)).

## Note

Setting `exact = TRUE` with a linear model (i.e., an
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) object) assumes that
the input features are independent. Also, setting `adjust = TRUE` is
experimental and we follow the same approach as in
[shap](https://github.com/shap/shap).

## References

Strumbelj, E., and Igor K. (2014). Explaining prediction models and
individual predictions with feature contributions. Knowledge and
information systems, 41(3), 647-665.

Lundberg, S. M., Erion, G., Chen, H., DeGrave, A., Prutkin, J. M., Nair,
B., Katz, R., Himmelfarb, J., Bansal, N., and Lee, Su-In (2020). From
local explanations to global understanding with explainable AI for
trees. Nature Machine Intelligence, 2(1), 2522–5839.

## See also

You can find more examples (with larger and more realistic data sets) on
the **fastshap** GitHub repository:
<https://github.com/bgreenwell/fastshap>.

## Examples

``` r
#
# A projection pursuit regression (PPR) example
#

# Load the sample data; see ?datasets::mtcars for details
data(mtcars)

# Fit a projection pursuit regression model
fit <- ppr(mpg ~ ., data = mtcars, nterms = 5)

# Prediction wrapper
pfun <- function(object, newdata) {  # needs to return a numeric vector
  predict(object, newdata = newdata)  
}

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(101)  # for reproducibility
shap <- explain(fit, X = subset(mtcars, select = -mpg), nsim = 10, 
                pred_wrapper = pfun)
head(shap)
#>                          cyl       disp         hp       drat         wt
#> Mazda RX4          0.6303330 -2.3039474  2.1702595  0.2449483  1.9516975
#> Mazda RX4 Wag      1.7985745 -0.7184875  0.8826874 -0.2529456  1.7067925
#> Datsun 710         0.8703724 -4.4098651  0.8866145 -0.3326103  5.0844730
#> Hornet 4 Drive    -0.1833108 -0.0481235  0.2110508 -0.1326025 -2.1821798
#> Hornet Sportabout -1.2078582  4.4318571 -0.2019058  0.4699908  0.3498138
#> Valiant           -0.2499883 -0.8686477  1.0185185  0.5495139 -3.9389435
#>                         qsec         vs         am       gear        carb
#> Mazda RX4         -0.4850098  0.4390292  0.3331098  0.5118683  0.61782956
#> Mazda RX4 Wag      0.3106207  0.8948709  0.6088751 -0.2614406  0.21693591
#> Datsun 710         0.5053539 -1.1357720 -1.0768200  1.0257002 -0.47275168
#> Hornet 4 Drive     0.2403159  0.5046293 -0.3766626 -2.3918254  0.92641170
#> Hornet Sportabout -1.7288483 -0.3980324 -0.1757677 -2.2603330 -0.64972068
#> Valiant            1.5248120 -0.9858987  0.6753099 -0.5210579 -0.04646088
```
