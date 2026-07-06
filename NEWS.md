# fastshap (development version)

## New features

* New `method = "permutation"` argument to `explain()` (GH #87): an
  alternative Monte Carlo estimator that walks a single random permutation
  through all features per replication instead of drawing independent
  coalitions per feature. This computes every feature's contribution from
  roughly half as many predictions as the default (`method = "strumbelj"`)
  estimator, and each replication's contributions sum *exactly* to
  `f(x) - f(background)` by construction, so `adjust = TRUE` (which corrects
  for the lack of that property under the default estimator) is not
  applicable and errors informatively if requested together with
  `method = "permutation"`. `parallel = TRUE` is not yet supported for this
  method. Because the telescoping property requires walking every feature
  regardless of what's requested, `feature_names` only subsets the *output*
  under `method = "permutation"` (unlike the default estimator, whose cost
  scales down with fewer requested features) — see `?explain` for details.


# fastshap 0.2.0

## New features

* New `explain_effects()` decomposes each feature's contribution into a
  `main` effect (the feature's effect in isolation; equal to the centered
  partial dependence of that feature), a `total` effect (main effect plus
  every interaction the feature participates in), and their gap
  (`interaction`, an interaction-strength diagnostic that is exactly zero for
  additive features). `shapley_values` (the average of `main` and `total`)
  equals the exact background-marginal Shapley value for any model with at
  most pairwise interactions — with no Monte Carlo simulation at all by
  default, since it averages over the entire background rather than sampling
  coalitions. Set `nsim` to sample background rows instead, for large
  backgrounds where full averaging is too expensive.

## Performance

* The Monte Carlo estimation loop was rewritten to batch predictions: all
  `nsim` replications for a feature are now stacked and evaluated in a single
  pair of `pred_wrapper()` calls (previously `2 * nsim` calls per feature),
  letting vectorized/multithreaded `predict()` methods do the heavy lifting.
  Combined with the other changes below, `explain()` is roughly 2.5x faster
  on a typical ranger example (500 x 10 data frame, `nsim = 10`), with much
  larger gains for prediction functions with high per-call overhead.

* New `batch_size` argument for `explain()` bounds the number of rows passed
  to `pred_wrapper()` per call (memory/speed trade-off). All randomness is
  drawn up front, so seeded results are identical regardless of `batch_size`.

* When `newdata` is supplied, the internal permutation matrix is now generated
  at `nrow(newdata)` instead of at the (potentially much larger) background
  size and then subsampled; explaining a few rows against a large background
  no longer wastes time generating discarded random rows.

* Data frame inputs are now processed column-wise instead of via matrix-style
  logical indexing of data frames (one of the slowest operations in base R);
  this also preserves column classes by construction, eliminating two
  `copy_classes()` calls per Monte Carlo evaluation.

* The C++ routine that assembles the hybrid ("Frankenstein") data sets now
  passes inputs by reference, skips a no-op assignment, and constructs the
  second data set from the first via a single column swap (the two differ
  only in the feature of interest).

## Breaking changes

* Seeded results differ from fastshap (<= 0.1.5): the Monte Carlo loop was
  vectorized, which changes how the random number stream is consumed. The
  estimator itself is unchanged.

* **foreach** moved from Imports to Suggests. Sequential execution (the
  default) no longer touches **foreach**; `parallel = TRUE` requires it to be
  installed and errors informatively otherwise. The dead `.options.RNG`
  argument (only honored by `%dorng%`, which fastshap never used) is no longer
  passed; for reproducible parallel results, register a **doRNG** backend.

* `gen_friedman()`'s deprecated `n_bins` argument was removed.

## Fixed

* `explain(..., parallel = TRUE)` on socket-based clusters (the default
  cluster type on Windows, where workers start with no packages attached)
  no longer fails with `could not find function "explain_feature"`. The
  0.2.0 batching rewrite moved the per-feature computation behind an extra
  layer of internal function calls, which broke `foreach`'s automatic
  export of the (non-exported) worker function on such clusters; the
  fastshap namespace is now explicitly added to `.packages`. If fastshap is
  installed to a non-default library path (a development checkout, an
  `renv`/`pak` project library, or `R CMD check`'s ephemeral install), also
  propagate `.libPaths()` to the cluster before registering it — see
  `?explain`'s `parallel` argument for the one-liner.

* `explain()` with `exact = TRUE` and `shap_only = TRUE` (the default) for
  `xgb.Booster` and `lgb.Booster` objects now correctly retains the
  `"baseline"` attribute and `"explain"` class; previously both were silently
  dropped when the bias column was subset out of the result (the baseline
  attribute was lost for both model types, and the class was also lost for
  `xgb.Booster`).

* Unknown values in `feature_names` now error informatively instead of
  failing obscurely downstream.

* A `pred_wrapper` that does not return one prediction per row (e.g., a
  multiclass probability matrix) now errors informatively instead of
  producing corrupted output.

* A `tbl_df` passed as `newdata` is now coerced to a data frame (matching the
  existing coercion of `X`), instead of triggering a misleading class-mismatch
  error.

## Documentation

* The vignette was rewritten to run in ~20 seconds (previously ~20 minutes,
  which required a `.Rmd.orig` precompile workaround that has been removed);
  it now also demonstrates the exact-vs-approximate comparison and
  `explain_effects()`'s interaction diagnostic on the Friedman 1 benchmark
  data.

* README and AGENTS.md were rewritten to match current package conventions
  (branch model, dependency philosophy, architecture map).

## Internal

* Removed ~350 lines of dead code: the vendored copy of `abind()` and the
  `copy_classes()` helper (superseded by the column-wise construction).

* Dropped **AmesHousing**, **ggplot2**, and **tibble** from Suggests; they
  were only used by the old vignette.


# fastshap 0.1.5

## Fixed

* `explain()` with `nsim = 1` now returns a correctly shaped matrix for all
  combinations of `nrow(newdata)` and number of features. Previously,
  `replicate(1L, ...)` returned a plain vector rather than a matrix, causing
  `rowMeans()` to dispatch as `mean.default()` (wrong results for multiple rows)
  and a `colnames<-` error for single-row inputs (#77).

* `explain()` with `adjust = TRUE` and a single feature name (e.g.,
  `feature_names = "wt"`) no longer throws `Error: object 'phis' not found`.
  When `foreach` iterates over a single element it skips `.combine`, returning a
  2-D matrix; the code now promotes it to a 3-D array before the adjustment loop
  (#79).

* `exact = TRUE` for multiclass `xgboost` or `lightgbm` models now stops
  immediately with an informative error instead of silently producing corrupted
  output (#51).

* Passing `exact` to `explain()` for a model type that does not support it (e.g.,
  `ranger`, `cv.glmnet`) no longer silently leaks the argument into `foreach()`.
  `exact` is now an explicit parameter of `explain.default()`; `exact = TRUE`
  issues a warning and falls back to the Monte Carlo approximation (#74).

## Documentation

* Updated the vignette parallel example to use `makeCluster()` +
  `registerDoParallel()` (cross-platform, including Windows) and to pass
  `.packages = "ranger"` via `...` so worker processes can find `predict.ranger`.
  The previous `registerDoParallel(cores = N)` pattern failed on Windows with
  "no applicable method for 'predict'" (#56).

* Clarified the `exact` parameter documentation to note that multiclass
  `xgboost`/`lightgbm` models are not yet supported and that passing `exact = TRUE`
  for other model types issues a warning and falls back to Monte Carlo.

## Infrastructure

* Package is now distributed via
  [r-universe](https://bgreenwell.r-universe.dev/fastshap) instead of CRAN.
  Installation instructions in the README have been updated accordingly.

# fastshap 0.1.4

## Changed

* `explain()` now returns `NA_real_` as the baseline (stored as the `"baseline"`
  attribute) when `adjust = FALSE` and no baseline is provided. This correctly
  signals that the baseline was not computed, rather than implying a default
  baseline of zero.

* Formalized the single-row performance optimization in `explain()` and removed
  the "experimental" label.

* Deprecated the `n_bins` argument in `gen_friedman()`; it will be removed in
  a future release.

## Fixed

* Fixed a typo in the error message in `gen_friedman()`.

## Cleanup

* Removed the `slowtests/` directory and cleaned up binary artifacts and large
  cache directories in `rjarticle/`. Updated `.gitignore` to prevent these from
  being re-tracked.

# fastshap 0.1.3

## Changed

* `explain()` with `raw = TRUE` now returns a 3-D array of dimensions
  `n x p x nsim` (observations × features × simulations) instead of a named
  list. This makes downstream computation straightforward: e.g.,
  `apply(result, 1:2, sd)` yields per-(observation, feature) standard errors,
  and `result[, , k]` gives the `k`-th simulation's SHAP matrix in the same
  `n x p` shape as the normal output. Using `raw = TRUE` together with
  `adjust = TRUE` now emits a warning and returns the adjusted means (same
  behaviour as `raw = FALSE`).

# fastshap 0.1.2

## Fixed

* Fixed compatibility with XGBoost 2.x: `explain.xgb.Booster()` now correctly handles the case where `predict(..., predcontrib = TRUE)` returns a named vector instead of a matrix for single-row inputs (#GH).

* Updated test suite to use the current LightGBM 4.x prediction API (`type = "raw"` and `type = "contrib"` instead of deprecated `rawscore` and `predcontrib` arguments; `newdata` instead of `data`).

* Updated test suite to use the current XGBoost 2.x training API (`x`/`y` instead of `data`/`label`; `learning_rate` instead of `eta`).

* Fixed a pre-existing typo in a test file where `ewdata` was used instead of `newdata`, causing the `newdata` argument to be silently ignored.

# fastshap 0.1.1

## Changed

* This NEWS file now follows the [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) format.

## Fixed 

* Removed an unnecessary `.Rd` file to satisfy CRAN policies.
* Fixed a couple of outdated URLs.
* Added [earth](https://CRAN.R-project.org/package=earth) to the list of suggested packages since it's referenced a couple of times in the package documentation.

# fastshap 0.1.0

## Breaking changes

* The `explain()` function now returns a matrix, as opposed to a [tibble](https://cran.r-project.org/package=tibble), which makes more sense since Shapley values values are ALWAYS numeric; data frames (and [tibbles](https://cran.r-project.org/package=tibble)'s) are really only necessary when the data are heterogeneous. In essence, the output from `explain()` will act like an R matrix but with class structure `c("explain", "matrix", "array")`; you could always convert the results to a tibble using `tibble::as_tibble(result)`.

* Two new data sets, `titanic` and `titanic_mice`, were added to the package; see the corresponding help pages for details.

* **The plotting functions have all been deprecated** in favor of the (far superior) [shapviz](https://cran.r-project.org/package=shapviz) package by @Mayer79 (`grid.arrange()` is also no longer imported from [gridExtra](https://cran.r-project.org/package=gridExtra)). Consequently, the output from `explain()` no longer needs to have its own `"explain"` class (only an ordinary `c("matrix", "array")` object is returned).

* The `explain()` function gained three new arguments:

  - `baseline`, which defaults to `NULL`, containing the baseline to use when adjusting Shapley values to meet the efficiency property. If `NULL` and `adjust = TRUE`, it will default to the average training prediction (i.e., the average prediction over `X`.)

  - `shap_only`, which defaults to `TRUE`, determines whether to return a matrix of Shapley values (`TRUE`) containing the baseline as an attribute or a list containing the Shapley values, corresponding feature values, and baseline (`FALSE`); setting to `FALSE` is a convenience when using the [shapviz](https://cran.r-project.org/package=shapviz) package.

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
[shap](https://github.com/shap/shap) [(#6)](https://github.com/bgreenwell/fastshap/issues/6).

* New (experimental) function for constructing [force plots](https://github.com/shap/shap) [(#7)](https://github.com/bgreenwell/fastshap/issues/7) to help visualize prediction explanations. The function is also a generic which means additional methods can be added.

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
