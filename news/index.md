# Changelog

## fastshap 0.1.4

### Changed

- [`explain()`](../reference/explain.md) now returns `NA_real_` as the
  baseline (stored as the `"baseline"` attribute) when `adjust = FALSE`
  and no baseline is provided. This correctly signals that the baseline
  was not computed, rather than implying a default baseline of zero.

- Formalized the single-row performance optimization in
  [`explain()`](../reference/explain.md) and removed the “experimental”
  label.

- Deprecated the `n_bins` argument in
  [`gen_friedman()`](../reference/gen_friedman.md); it will be removed
  in a future release.

### Fixed

- Fixed a typo in the error message in
  [`gen_friedman()`](../reference/gen_friedman.md).

### Cleanup

- Removed the `slowtests/` directory and cleaned up binary artifacts and
  large cache directories in `rjarticle/`. Updated `.gitignore` to
  prevent these from being re-tracked.

## fastshap 0.1.3

### Changed

- [`explain()`](../reference/explain.md) with `raw = TRUE` now returns a
  3-D array of dimensions `n x p x nsim` (observations × features ×
  simulations) instead of a named list. This makes downstream
  computation straightforward: e.g., `apply(result, 1:2, sd)` yields
  per-(observation, feature) standard errors, and `result[, , k]` gives
  the `k`-th simulation’s SHAP matrix in the same `n x p` shape as the
  normal output. Using `raw = TRUE` together with `adjust = TRUE` now
  emits a warning and returns the adjusted means (same behaviour as
  `raw = FALSE`).

## fastshap 0.1.2

### Fixed

- Fixed compatibility with XGBoost 2.x:
  [`explain.xgb.Booster()`](../reference/explain.md) now correctly
  handles the case where `predict(..., predcontrib = TRUE)` returns a
  named vector instead of a matrix for single-row inputs (#GH).

- Updated test suite to use the current LightGBM 4.x prediction API
  (`type = "raw"` and `type = "contrib"` instead of deprecated
  `rawscore` and `predcontrib` arguments; `newdata` instead of `data`).

- Updated test suite to use the current XGBoost 2.x training API
  (`x`/`y` instead of `data`/`label`; `learning_rate` instead of `eta`).

- Fixed a pre-existing typo in a test file where `ewdata` was used
  instead of `newdata`, causing the `newdata` argument to be silently
  ignored.

## fastshap 0.1.1

CRAN release: 2024-02-22

### Changed

- This NEWS file now follows the [Keep a
  Changelog](https://keepachangelog.com/en/1.1.0/) format.

### Fixed

- Removed an unnecessary `.Rd` file to satisfy CRAN policies.
- Fixed a couple of outdated URLs.
- Added [earth](https://CRAN.R-project.org/package=earth) to the list of
  suggested packages since it’s referenced a couple of times in the
  package documentation.

## fastshap 0.1.0

CRAN release: 2023-06-06

### Breaking changes

- The [`explain()`](../reference/explain.md) function now returns a
  matrix, as opposed to a
  [tibble](https://cran.r-project.org/package=tibble), which makes more
  sense since Shapley values values are ALWAYS numeric; data frames (and
  [tibbles](https://cran.r-project.org/package=tibble)’s) are really
  only necessary when the data are heterogeneous. In essence, the output
  from [`explain()`](../reference/explain.md) will act like an R matrix
  but with class structure `c("explain", "matrix", "array")`; you could
  always convert the results to a tibble using
  `tibble::as_tibble(result)`.

- Two new data sets, `titanic` and `titanic_mice`, were added to the
  package; see the corresponding help pages for details.

- **The plotting functions have all been deprecated** in favor of the
  (far superior) [shapviz](https://cran.r-project.org/package=shapviz)
  package by [@Mayer79](https://github.com/Mayer79) (`grid.arrange()` is
  also no longer imported from
  [gridExtra](https://cran.r-project.org/package=gridExtra)).
  Consequently, the output from [`explain()`](../reference/explain.md)
  no longer needs to have its own `"explain"` class (only an ordinary
  `c("matrix", "array")` object is returned).

- The [`explain()`](../reference/explain.md) function gained three new
  arguments:

  - `baseline`, which defaults to `NULL`, containing the baseline to use
    when adjusting Shapley values to meet the efficiency property. If
    `NULL` and `adjust = TRUE`, it will default to the average training
    prediction (i.e., the average prediction over `X`.)

  - `shap_only`, which defaults to `TRUE`, determines whether to return
    a matrix of Shapley values (`TRUE`) containing the baseline as
    aanattribute or a list containing the Shapley values, corresponding
    feature values, and baseline (`FALSE`); setting to `FALSE` is a
    convenience when using the
    [shapviz](https://cran.r-project.org/package=shapviz) package.

  - `parallel`, which defaults to `FALSE` for determining whether or not
    to compute Shapley values in parallel (across features) using any
    suitable parallel backend supported by
    [foreach](https://cran.r-project.org/package=foreach).

### Miscellaneous

- The `X` and `newdata` arguments of
  [`explain()`](../reference/explain.md) should now work with
  [tibble](https://cran.r-project.org/package=tibble)
  [(](https://github.com/bgreenwell/fastshap/issues/20)[\#20](https://github.com/bgreenwell/fastshap/issues/20)).

- Minor change to [`explain.lgb.Booster()`](../reference/explain.md) to
  support breaking changes in
  [lightgbm](https://cran.r-project.org/package=lightgbm) v4.0.0.
  (Thanks to [@jameslamb](https://github.com/jameslamb) and
  [@Mayer79](https://github.com/Mayer79).)

- The dependency on
  [matrixStats](https://cran.r-project.org/package=matrixStats) has been
  removed in favor of using R’s internal
  [`apply()`](https://rdrr.io/r/base/apply.html) and
  [`var()`](https://rdrr.io/r/stats/cor.html) functions.

- The dependency on [plyr](https://cran.r-project.org/package=plyr),
  which has been retired, has been removed in favor of using
  [foreach](https://cran.r-project.org/package=foreach) directly.

- Removed CXX_STD=CXX11 flag, so increased R dependency to R \>= 3.6.

## fastshap 0.0.7

CRAN release: 2021-12-06

### Miscellaneous

- Move [lightgbm](https://cran.r-project.org/package=lightgbm) tests to
  `slowtests/` directory (for now).

## fastshap 0.0.6

CRAN release: 2021-12-03

### Enhancements

- Thanks to Michael Mayer ([@mayer79](https://github.com/mayer79)),
  function [`explain()`](../reference/explain.md) now supports
  [lightgbm](https://cran.r-project.org/package=lightgbm) models
  [(](https://github.com/bgreenwell/fastshap/issues/15)[\#15](https://github.com/bgreenwell/fastshap/issues/15)).

### Bug fixes

- The `force_plot()` function should now be compatible with **shap**
  (\>=0.36.0); thanks to [@hfshr](https://github.com/hfshr) and
  [@jbwoillard](https://github.com/jbwoillard) for reporting
  [(](https://github.com/bgreenwell/fastshap/issues/12)[\#12](https://github.com/bgreenwell/fastshap/issues/12)).

- Fixed minor name repair issue caused by
  [tibble](https://cran.r-project.org/package=tibble).

### Miscellaneous

- Switched from Travis-CI to GitHub Actions for continuous integration.

## fastshap 0.0.5

CRAN release: 2020-02-02

### Bug fixes

- Fixed a bug that occurred with logical columns in older version of R
  (\<= 3.6.0)
  [(](https://github.com/bgreenwell/fastshap/issues/9)[\#9](https://github.com/bgreenwell/fastshap/issues/9)).

## fastshap 0.0.4

CRAN release: 2020-01-26

### Enhancements

- Function [`explain()`](../reference/explain.md) should now be MUCH
  faster at explaining a single observation, especially when `nsim` is
  relatively large (e.g., `nsim >= 1000`).

### Bug fixes

- Fixed a MAJOR bug that occurred whenever explaining data sets with
  non-numeric features.

### New features

- The default method of [`explain()`](../reference/explain.md) gained a
  new logical argument called `adjust`. When `adjust = TRUE` (and
  `nsim > 1`), the algorithm will adjust the sum of the estimated
  Shapley values to satisfy the *efficiency property*; that is, to equal
  the difference between the model’s prediction for that sample and the
  average prediction over all the training data. This option is
  experimental and we follow the same approach as in
  [shap](https://github.com/shap/shap)
  [(](https://github.com/bgreenwell/fastshap/issues/6)[\#6](https://github.com/bgreenwell/fastshap/issues/6)).

- New (experimental) function for constructing [force
  plots](https://github.com/shap/shap)
  [(](https://github.com/bgreenwell/fastshap/issues/7)[\#7](https://github.com/bgreenwell/fastshap/issues/7))
  to help visualize prediction explanations. The function is also a
  generic which means additional methods can be added.

- Function [`explain()`](../reference/explain.md) became a generic and
  gained a new logical argument, `exact`, for computing exact Shapley
  contributions for linear models (Linear SHAP, which assumes
  independent features) and boosted decision trees (Tree SHAP).
  Currently, only `"lm"`, `"glm"`, and `"xgb.Booster"` objects are
  supported
  [(](https://github.com/bgreenwell/fastshap/issues/2)[\#2](https://github.com/bgreenwell/fastshap/issues/2))[(](https://github.com/bgreenwell/fastshap/issues/3)[\#3](https://github.com/bgreenwell/fastshap/issues/3)).

### Minor changes

- Minor improvements to package documentation.

- Removed unnecessary legend from contribution plots.

## fastshap 0.0.3

CRAN release: 2019-12-03

### Minor changes

- Tweak imports (in particular, use `@importFrom Rcpp sourceCpp` tag).

- Fixed a typo in the package description; Shapley was misspelled as
  Shapely (fixed by Dirk Eddelbuettel in
  [(](https://github.com/bgreenwell/fastshap/pull/1)[\#1](https://github.com/bgreenwell/fastshap/issues/1))).

## fastshap 0.0.2

CRAN release: 2019-11-22

### New features

- You can now specify `type = "contribution"` in the call to
  `autoplot.fastshap()` to plot the explanation for a single instance
  (controlled by the `row_num` argument).

- `autoplot.fastshap()` gained some useful new arguments:

  - `color_by` for specifying an additional feature to color by for
    dependence plots (i.e., whenever `type = "dependence"`);

  - `smooth`, `smooth_color`, `smooth_linetype`, `smooth_size`, and
    `smooth_alpha` for adding/controlling a smoother in dependence plots
    (i.e., whenever `type = "dependence"`).

  - `...` which can be used to pass on additional parameters to
    `geom_col()` (when `type = "importance"`) or `geom_point()` (when
    `type = "dependence"`).

### Breaking changes

- Function [`fastshap()`](../reference/fastshap-package.md) was renamed
  to [`explain()`](../reference/explain.md).

- Functions [`explain()`](../reference/explain.md) and
  `explain_column()` (not currently exported) now throw an error
  whenever the inputs `X` and `newdata` do not inherit from the same
  class.

### Bug fixes

- Fixed a bug in the C++ source that gave more weight to extreme
  permutations.

- Fixed a bug in the C++ source that caused doubles to be incorrectly
  converted to integers.

- Fixed a bug in `autoplot.fastshap()` when `type = "importance"`; in
  particular, the function incorrectly used `sum(|Shapley value|)`
  instead of `mean(|Shapley value|)`.

## fastshap 0.0.1

- Initial release.
