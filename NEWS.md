# fastshap 0.0.2.9000

## Minor changes

* Fixed a typo in the package description; Shapley was misspelled as Shapely (fixed by Dirk Eddelbuettel in [#1](https://github.com/bgreenwell/fastshap/pull/1)).


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
