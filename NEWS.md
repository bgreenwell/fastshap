# fastshap 0.0.4.9000

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

* Function `explain()` became a generic and gained a new logical argument, `exact`, for computing exact Shapley contributions for linear models (LinearSHAP, which assumes independent features) and boosted decision trees (TreeSHAP). Currently, only `"lm"`, `"glm"`, and `"xgb.Booster"` objects are supported [(#2)](https://github.com/bgreenwell/fastshap/issues/2)[(#3)](https://github.com/bgreenwell/fastshap/issues/3).

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
