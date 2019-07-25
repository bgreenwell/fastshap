# fastshap 0.0.2

## New features

* `autoplot.fastshap()` gained some useful new arguments:

    - You can now specify `type = "contribution"` to plot the explanation for a single instance (controlled by the `row_num` argument).

    - `color_by` for specifying an additional feature to color by for dependence plots (i.e., whenever `type = "dependence"`);
    - `smooth`, `smooth_color`, `smooth_linetype`, `smooth_size`, and `smooth_alpha` for adding/controlling a smoother in dependence plots (i.e., whenever `type = "dependence"`).
    - `...` which can be used to pass on additional parameters to `geom_col()` (when `type = "importance"`) or `geom_point()` (when `type = "dependence"`).


## Bug fixes

* Fixed a bug in the C++ source that gave more weight to extreme permutations.

* Fixed a bug in the C++ source that caused doubles to be incorrectly converted to integers.

* Fixed a bug in `autoplot.fastshap()` when `type = "importance"`; in particular, the function incorrectly used `sum(|SHapley value|)` instead of `mean(|SHapley value|)`.


# fastshap 0.0.1

* Initial release.
