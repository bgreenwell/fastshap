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
  batch_size = NULL,
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
  whenever `parallel = TRUE` (e.g., `.packages` for loading packages on
  the workers); ignored otherwise.

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
  setting `parallel = TRUE` requires the
  [foreach](https://cran.r-project.org/package=foreach) package (in
  Suggests) and an appropriate (i.e., system-specific) *parallel
  backend* (e.g., via the doParallel package). For reproducible parallel
  results, use the doRNG package to register the backend. Note that the
  internal batching of predictions (see `batch_size`) means models whose
  [`predict()`](https://rdrr.io/r/stats/predict.html) methods are
  already multithreaded (e.g., ranger or xgboost) often gain little from
  `parallel = TRUE`. The fastshap namespace is always added to
  `.packages` automatically (socket-based clusters, the default on
  Windows, start with no packages attached), so any *additional*
  packages needed by `pred_wrapper` should be supplied via `.packages`
  as usual. If fastshap (or a dependency) is installed somewhere other
  than the default library path (e.g., a development checkout,
  `renv`/`pak` project library, or during `R CMD check`, which installs
  the package under test into an ephemeral library), fresh workers won't
  see it either; propagate the current session's library path to the
  cluster before registering it, e.g.
  `parallel::clusterCall(cl, function(lp) .libPaths(lp), .libPaths())`.

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
  `NULL` (no seed). **NOTE:** the Monte Carlo loop was restructured
  (vectorized) in version 0.2.0, so seeded results differ from those
  produced by fastshap (\<= 0.1.5).

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

- batch_size:

  Optional positive integer giving the maximum number of rows to pass to
  `pred_wrapper()` per call. By default (`NULL`), all `nsim` Monte Carlo
  replications for a feature are stacked and evaluated in a single pair
  of prediction calls, which is fastest but requires
  `2 * nrow(newdata) * nsim * ncol(X)` values of working memory per
  feature; set `batch_size` to bound the size of each prediction call
  instead. All of the randomness is drawn up front, so for a fixed seed
  the results are identical regardless of `batch_size`.

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
#>              cyl       disp           hp       drat         wt          qsec
#> [1,] -0.97509243 -1.0947377 -0.007900566 -0.3593698  0.1239639 -0.8898017829
#> [2,] -1.78889380  1.6534578 -1.643102230  1.0991704 -0.1044265  0.0001145306
#> [3,]  0.02128044 -1.5145031  3.056046948 -0.1843992  2.0956560  1.4708352050
#> [4,]  0.20294720 -0.5665266  2.673123253  0.1170155 -3.3047768  0.9908682708
#> [5,] -0.34441168  2.2236638 -2.401822557  0.9820953  0.2729297  0.3153070648
#> [6,]  0.36095745  1.1078770 -0.112842677 -0.7187627 -0.6142340  0.0857486015
#>                vs          am       gear        carb
#> [1,] -0.527763170  0.07486571 -0.9789857 -0.22132538
#> [2,]  0.643074719 -0.80416125 -0.1585971 -0.91990224
#> [3,] -1.431546936  1.74951003  0.3225059 -1.16966138
#> [4,]  0.795329172 -0.48489827 -0.1622227  0.68253354
#> [5,]  0.005215444 -0.29994990 -0.5858764  0.08557275
#> [6,] -0.172783523 -0.33312229 -1.5871195  0.74716858
```
