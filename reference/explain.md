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
  exact = FALSE,
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
  only supported for [`stats::lm()`](https://rdrr.io/r/stats/lm.html),
  [`xgboost::xgboost()`](https://rdrr.io/pkg/xgboost/man/xgboost.html),
  and
  [`lightgbm::lightgbm()`](https://rdrr.io/pkg/lightgbm/man/lightgbm.html)
  objects (binary/regression only — multiclass is not yet supported).
  Passing `exact = TRUE` for any other model type issues a warning and
  falls back to the Monte Carlo approximation. Note that `exact = TRUE`
  for [`stats::lm()`](https://rdrr.io/r/stats/lm.html) returns
  explanations for each of the
  [`stats::terms()`](https://rdrr.io/r/stats/terms.html) in the model.
  Default is `FALSE`.

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
#>                          cyl       disp        hp        drat         wt
#> Mazda RX4         -1.0903775 -0.6274969 3.0832317 -0.07256155  0.1959452
#> Mazda RX4 Wag     -1.0458065  0.2582914 0.9277668  1.17899813  1.6484232
#> Datsun 710         0.3088864 -0.9026665 0.7188540  1.85702718  2.4110529
#> Hornet 4 Drive     0.5242879  0.4007763 0.2824977  1.97033461 -1.9194076
#> Hornet Sportabout  0.7715692  1.6525963 0.4231664  0.41508036  0.3124217
#> Valiant           -0.6537243  0.1031461 0.8710020 -0.16422768 -2.4402055
#>                          qsec         vs          am       gear        carb
#> Mazda RX4         -1.46160496  0.2213145 -0.68947599 -0.3840636  0.95078654
#> Mazda RX4 Wag      0.04892765  0.3310340  0.54495885 -0.5194436 -0.28010809
#> Datsun 710         0.03171852 -0.2160763  1.01495061  0.7111122 -0.06651041
#> Hornet 4 Drive    -0.25539764  1.5820107 -0.97231505 -1.7708919  1.10185304
#> Hornet Sportabout -0.29810781 -0.2203552  0.05188098 -1.3600599  0.95575680
#> Valiant            0.50059940  0.3920500 -1.35786957 -2.2330062  0.01932869
```
