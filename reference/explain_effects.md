# Main, total, and interaction effect decomposition

For each feature, decompose its contribution to a prediction into a
*main effect* (the feature's effect in isolation), a *total effect* (the
feature's effect together with every interaction it participates in),
and the gap between them (an *interaction* diagnostic). Unlike
[`explain()`](https://bgreenwell.github.io/fastshap/reference/explain.md),
which estimates Shapley values by averaging over *randomly sampled*
coalitions, `explain_effects()` uses only the two coalitions with the
highest Shapley weight — the empty coalition and the full coalition — so
by default (`nsim = NULL`) it is deterministic given the background
data, with no Monte Carlo simulation at all.

## Usage

``` r
explain_effects(
  object,
  feature_names = NULL,
  X = NULL,
  pred_wrapper = NULL,
  newdata = NULL,
  nsim = NULL,
  batch_size = NULL,
  seed = NULL,
  ...
)

# S3 method for class 'explain_effects'
print(x, ...)
```

## Arguments

- object:

  A fitted model object (e.g., a
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md),
  [`xgboost::xgboost()`](https://rdrr.io/pkg/xgboost/man/xgboost.html),
  or [`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html)
  object, to name a few).

- feature_names:

  Character string giving the names of the predictor variables (i.e.,
  features) of interest. If `NULL` (default) they will be taken from the
  column names of `X`.

- X:

  A matrix-like R object (e.g., a data frame or matrix) containing ONLY
  the feature columns from the training data (or suitable background
  data set). **NOTE:** This argument is required whenever
  `exact = FALSE`.

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

- nsim:

  Optional positive integer giving the number of background rows to
  sample per observation. Default (`NULL`) averages over *every* row of
  `X` (deterministic, exact given the background data, but requires
  `2 * nrow(newdata) * length(feature_names) * nrow(X)` predictions);
  set `nsim` to average over a random sample of that many background
  rows instead (requires `seed` for reproducibility).

- batch_size:

  Optional positive integer giving the maximum number of rows to pass to
  `pred_wrapper()` per call. By default (`NULL`), all `nsim` Monte Carlo
  replications for a feature are stacked and evaluated in a single pair
  of prediction calls, which is fastest but requires
  `2 * nrow(newdata) * nsim * ncol(X)` values of working memory per
  feature; set `batch_size` to bound the size of each prediction call
  instead. All of the randomness is drawn up front, so for a fixed seed
  the results are identical regardless of `batch_size`.

- seed:

  Integer specifying a random seed for reproducibility; passed to
  [`base::set.seed()`](https://rdrr.io/r/base/Random.html). Default is
  `NULL` (no seed). **NOTE:** the Monte Carlo loop was restructured
  (vectorized) in version 0.2.0, so seeded results differ from those
  produced by fastshap (\<= 0.1.5).

- ...:

  Additional optional arguments to be passed on to
  [`foreach::foreach()`](https://rdrr.io/pkg/foreach/man/foreach.html)
  whenever `parallel = TRUE` (e.g., `.packages` for loading packages on
  the workers); ignored otherwise.

- x:

  An object of class `"explain_effects"`.

## Value

An object of class `"explain_effects"`: a list with components `main`,
`total`, and `interaction` (each an `nrow(newdata)` x
`length(feature_names)` matrix), `shapley_values` (same shape), and
`baseline` (the average prediction over `X`).

## Details

For feature \\j\\ and observation \\x\\, averaging over background draws
\\w\\:

- `main`:

  `E_w[f(x_j, w_{-j})] - E_w[f(w)]`. Since no other feature of `x` is
  present, this is free of interaction effects — it is exactly the
  centered *partial dependence* of feature \\j\\ evaluated at \\x_j\\.

- `total`:

  `f(x) - E_w[f(w_j, x_{-j})]`. This captures the main effect of feature
  \\j\\ together with every interaction it participates in, in the
  context of \\x\\.

- `interaction`:

  `total - main`. Zero exactly when feature \\j\\ enters the model
  additively (no interactions involving \\j\\) at \\x\\; for a two-way
  interaction the gap splits equally between the two participating
  features.

- `shapley_values`:

  `(main + total) / 2`. This equals the exact (background-marginal)
  Shapley value for any model with at most pairwise interactions; for
  higher-order interactions it is a cheap but biased approximation.

As with
[`explain()`](https://bgreenwell.github.io/fastshap/reference/explain.md),
this uses marginal (interventional) sampling of the background, so the
usual caveat applies: with correlated features, the constructed hybrid
observations can be unrealistic combinations of feature values.

## References

Strumbelj, E., and Igor K. (2014). Explaining prediction models and
individual predictions with feature contributions. Knowledge and
information systems, 41(3), 647-665.

## Examples

``` r
data(mtcars)
fit <- ppr(mpg ~ ., data = mtcars, nterms = 5)
pfun <- function(object, newdata) predict(object, newdata = newdata)
eff <- explain_effects(fit, X = subset(mtcars, select = -mpg),
                       pred_wrapper = pfun)
eff
#> Feature effect decomposition (fastshap::explain_effects)
#> 
#> Baseline: 20.09062 
#> 
#> Shapley values (main + total) / 2, first few rows:
#>                          cyl       disp         hp       drat          wt
#> Mazda RX4         -0.9260637  0.4770120  1.0277270  0.6683485  0.20596049
#> Mazda RX4 Wag     -0.5826614  0.4688824  0.6144129  1.1679270 -0.77484265
#> Datsun 710        -0.5864684 -1.5489685  1.7342196 -0.8022045  1.26299173
#> Hornet 4 Drive     0.7177922  1.7341413  1.0786095  1.0317462 -0.32592778
#> Hornet Sportabout  0.6970596  1.8408765 -1.3024910 -0.2988270 -0.02459489
#> Valiant           -0.5378741 -0.1325116  0.5650165 -0.9494690 -1.64131076
#>                          qsec         vs          am        gear       carb
#> Mazda RX4         -0.95252219  0.6263350 -0.36593850  0.95169066 -0.6663305
#> Mazda RX4 Wag     -0.62759422  0.3646993 -0.86684069  1.52692379  0.1535051
#> Datsun 710        -0.03372289 -0.4068865  0.28386874  0.08127277 -1.1858840
#> Hornet 4 Drive     1.95838344  0.7011125  0.06250929 -0.59852720  1.6612071
#> Hornet Sportabout -0.31801246  0.7688738  0.71760723 -1.39815962  0.7968179
#> Valiant            0.83421120 -0.5596863 -0.37283900 -2.10383793  0.5240193
```
