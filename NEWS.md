# fastshap 0.1.0

## Breaking changes

* The `explain()` function now returns a matrix, as opposed to a [tibble](https://cran.r-project.org/package=tibble), which makes more sense since Shapley values values are ALWAYS numeric; data frames (and [tibbles](https://cran.r-project.org/package=tibble)'s) are really only necessary when the data are heterogeneous. In essence, the output from `explain()` will act like an R matrix but with class structure `c("explain", "matrix", "array")`; you could always convert the results to a tibble using `tibble::as_tibble(result)`.

* Two new data sets, `titanic` and `titanic_mice`, were added to the package; see the corresponding help pages for details.

* **The plotting functions have all been deprecated** in favor of the (far superior) [shapviz](https://cran.r-project.org/package=shapviz) package by @Mayer79 (`grid.arrange()` is also no longer imported from [gridExtra](https://cran.r-project.org/package=gridExtra)). Consequently, the output from `explain()` no longer needs to have its own `"explain"` class (only an ordinary `c("matrix", "array")` object is returned).

* The `explain()` function gained three new arguments:

  - `baseline`, which defaults to `NULL`, containing the baseline to use when adjusting Shapley values to meet the efficiency property. If `NULL` and `adjust = TRUE`, it will default to the average training prediction (i.e., the average prediction over `X`.)

  - `shap_only`, which defaults to `TRUE`, determines whether to return a matrix of Shapley values (`TRUE`) containing the baseline as aanattribute or a list containing the Shapley values, corresponding feature values, and baseline (`FALSE`); setting to `FALSE` is a convenience when using the [shapviz](https://cran.r-project.org/package=shapviz) package.

  - `parallel`, which defaults to `FALSE` for determining whether or not to compute Shapley values in parallel (across features) using any suitable parallel backend supported by [foreach](https://cran.r-project.org/package=foreach).

## Miscellaneous

* The `X` and `newdata` arguments of `explain()` should now work with [tibble](https://cran.r-project.org/package=tibble) [(#20)](https://github.com/bgreenwell/fastshap/issues/20).

* Minor change to `explain.lgb.Booster()` to support breaking changes in [lightgbm](https://cran.r-project.org/package=lightgbm) v4.0.0. (Thanks to @jameslamb and @Mayer79.)

* The dependency on [matrixStats](https://cran.r-project.org/package=matrixStats) has been removed in favor of using R's internal `apply()` and `var()` functions.

* The dependency on [plyr](https://cran.r-project.org/package=plyr), which has been retired, has been removed in favor of using [foreach](https://cran.r-project.org/package=foreach) directly. 

* Removed CXX_STD=CXX11 flag, so increased R dependency to R >= 3.6.

# fastshap 0.0.7

## Miscellaneous

* Move [lightgbm](https://cran.r-project.org/package=lightgbm) tests to `slowtests/` directory (for now).

# fastshap 0.0.6

## Enhancements

* Thanks to Michael Mayer (@mayer79), function `explain()` now supports [lightgbm](https://cran.r-project.org/package=lightgbm) models [(#15)](https://github.com/bgreenwell/fastshap/issues/15).

## Bug fixes

* The `force_plot()` function should now be compatible with **shap** (>=0.36.0); thanks to @hfshr and @jbwoillard for reporting [(#12)](https://github.com/bgreenwell/fastshap/issues/12).

* Fixed minor name repair issue caused by [tibble](https://cran.r-project.org/package=tibble).

## Miscellaneous

* Switched from Travis-CI to GitHub Actions for continuous integration.

# fastshap 0.0.5

## Bug fixes

* Fixed a bug that occurred with logical columns in older version of R (<= 3.6.0) [(#9)](https://github.com/bgreenwell/fastshap/issues/9).

# fastshap 0.0.4

## Enhancements

* Function `explain()` should now be MUCH faster at explaining a single observation, especially when `nsim` is relatively large (e.g., `nsim >= 1000`).

## Bug fixes

* Fixed a MAJOR bug that occurred whenever explaining data sets with non-numeric
features.

## New features

* The default method of `explain()` gained a new logical argument called `adjust`. When `adjust = TRUE` (and `nsim > 1`), the algorithm will adjust the sum of the estimated Shapley values to satisfy the *efficiency property*; that is, to equal the difference between the model's prediction for that sample and the average prediction over all the training data. This option is experimental and we follow the same approach as in
[shap](https://github.com/slundberg/shap) [(#6)](https://github.com/bgreenwell/fastshap/issues/6).

* New (experimental) function for constructing [force plots](https://github.com/slundberg/shap) [(#7)](https://github.com/bgreenwell/fastshap/issues/7) to help visualize prediction explanations. The function is also a generic which means additional methods can be added.

* Function `explain()` became a generic and gained a new logical argument, `exact`, for computing exact Shapley contributions for linear models (Linear SHAP, which assumes independent features) and boosted decision trees (Tree SHAP). Currently, only `"lm"`, `"glm"`, and `"xgb.Booster"` objects are supported [(#2)](https://github.com/bgreenwell/fastshap/issues/2)[(#3)](https://github.com/bgreenwell/fastshap/issues/3).

## Minor changes

* Minor improvements to package documentation.

* Removed unnecessary legend from contribution plots.

# fastshap 0.0.3

## Minor changes

* Tweak imports (in particular, use `@importFrom Rcpp sourceCpp` tag).

* Fixed a typo in the package description; Shapley was misspelled as Shapely (fixed by Dirk Eddelbuettel in [(#1)](https://github.com/bgreenwell/fastshap/pull/1)).

# fastshap 0.0.2

## New features

* You can now specify `type = "contribution"` in the call to `autoplot.fastshap()` to plot the explanation for a single instance (controlled by the `row_num` argument).

* `autoplot.fastshap()` gained some useful new arguments:

    - `color_by` for specifying an additional feature to color by for dependence plots (i.e., whenever `type = "dependence"`);
   
    - `smooth`, `smooth_color`, `smooth_linetype`, `smooth_size`, and `smooth_alpha` for adding/controlling a smoother in dependence plots (i.e., whenever `type = "dependence"`).
    
    - `...` which can be used to pass on additional parameters to `geom_col()` (when `type = "importance"`) or `geom_point()` (when `type = "dependence"`).

## Breaking changes

* Function `fastshap()` was renamed to `explain()`.

* Functions `explain()` and `explain_column()` (not currently exported) now throw an error whenever the inputs `X` and `newdata` do not inherit from the same class.

## Bug fixes

* Fixed a bug in the C++ source that gave more weight to extreme permutations.

* Fixed a bug in the C++ source that caused doubles to be incorrectly converted to integers.

* Fixed a bug in `autoplot.fastshap()` when `type = "importance"`; in particular, the function incorrectly used `sum(|Shapley value|)` instead of `mean(|Shapley value|)`.

# fastshap 0.0.1

* Initial release.
