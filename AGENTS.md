# AGENTS.md — fastshap

R package for **fast, approximate Shapley values** explaining the predictions
of any supervised learning model. Exports: `explain()` (Monte Carlo Shapley
values, with exact methods for `lm`/`xgb.Booster`/`lgb.Booster` via
`exact = TRUE`), `explain_effects()` (deterministic main/total/interaction
effect decomposition), and `gen_friedman()` (benchmark data generator). Core
sampling/matrix-construction routines are in C++ (Rcpp/RcppArmadillo,
`src/fastshap.cpp`).

## Branches & releases

- **`devel`** (default): all development and PRs. Version carries a `.9000`
  suffix; NEWS.md starts with `# fastshap (development version)`.
- **`main`**: stable releases only, tagged `vX.Y.Z`. r-universe (pinned to
  main in bgreenwell/bgreenwell.r-universe.dev) and the pkgdown site both
  build from main — never push experimental work there.
- Release: merge devel → main (`--no-ff`), drop the `.9000` suffix and dev
  NEWS heading, tag, push, `gh release create`; then **merge main back into
  devel** (else the release merge commit leaves devel "behind" main) and bump
  devel to the next `.9000`.
- Shared fixes that main needs immediately (CI, README): commit to **main
  first, then merge main → devel**. Never cherry-pick devel → main — it
  duplicates commits and makes main appear "ahead" of devel.

## Dependency philosophy

Keep Imports minimal: `Rcpp, stats, utils` — nothing else without strong
justification. `foreach` lives in Suggests and is only touched when
`parallel = TRUE`; the sequential path is a base `for` loop. Use `pkg::fun()`
for Suggests packages. `earth` has no code/test call site but is declared in
Suggests because of an `\link[earth]{earth}` Rd cross-reference in
`?explain` (and is used in the vignette).

## Commands

```bash
Rscript -e 'devtools::document()'                                        # after roxygen edits
R CMD INSTALL --preclean . && Rscript -e 'library(fastshap); tinytest::run_test_dir("inst/tinytest")'  # full test suite
Rscript -e 'devtools::check()'                                           # before pushing
```

Reinstalling (rather than `devtools::load_all()`) matters here because the
core estimator lives in compiled C++ — `load_all()` alone won't pick up
`src/` changes without a recompile step.

Tests use **tinytest** (not testthat) in `inst/tinytest/`. Model-specific
tests (lightgbm/xgboost/ranger) are gated behind `at_home()`, which
`tests/tinytest.R` derives from either a `.9000` dev version *or*
`NOT_CRAN=true` — CI must set `NOT_CRAN=true` or the gated suite silently
skips. Always add a NEWS.md entry; never edit `man/` by hand.

## Architecture (R/)

- `explain.R` — `explain()` generic + `explain.default()` (Monte Carlo
  estimator) and the `explain.lm` / `explain.xgb.Booster` / `explain.lgb.Booster`
  methods (each dispatches to `explain.default()` unless `exact = TRUE`).
  Internal helpers: `gen_perm_matrix()` (permutation-order logical matrix per
  feature), `frankenstein_matrix()` / `frankenstein_df()` (build the two
  hybrid "Frankenstein" data sets whose prediction difference estimates a
  feature's marginal contribution), `explain_feature()` (stacks all `nsim`
  replications for one feature into a single pair of batched `pred_wrapper()`
  calls, chunked by `batch_size`), and `tree_shap_result()` (shared
  bias-column-stripping logic for the two exact Tree SHAP methods).
- `explain_effects.R` — `explain_effects()`: same Frankenstein-matrix
  machinery as `explain()`, but with fixed (empty/full) coalitions instead of
  random permutations, so `main`/`total`/`interaction`/`shapley_values` are
  deterministic (or `nsim`-sampled) rather than Monte Carlo estimates.
  `shapley_values = (main + total) / 2` is exact for models with at most
  pairwise interactions.
- `gen_friedman.R` — Friedman 1 benchmark data generator, used throughout the
  test suite and vignette (`x1:x2` is the only true interaction, which
  `explain_effects()`'s interaction diagnostic should isolate).
- `src/fastshap.cpp` — `genOMat()` (random permutation-order sampling) and
  `genFrankensteinMatrices()` (C++ backend for `frankenstein_matrix()`; builds
  the second hybrid matrix from the first via a single column swap, since
  they differ only in the feature of interest).

## Performance

`explain()`'s Monte Carlo loop batches predictions: all `nsim` replications
for a feature are stacked into one pair of `pred_wrapper()` calls (bounded by
the optional `batch_size` argument), not `2 * nsim` separate calls. All
randomness is drawn up front so results are invariant to `batch_size` for a
fixed seed. A deferred idea (permutation-walk / `method = "permutation"`
estimator, ~2x fewer evaluations per permutation with exact local accuracy)
and the full profiling/benchmarking behind these decisions are documented in
`.agents/explain-performance.md` — read that before touching the estimator
again.

## Gotchas

- **Seeded results changed in 0.2.0**: the Monte Carlo loop was vectorized,
  which changes how the RNG stream is consumed. This is expected and
  documented in NEWS.md; don't try to match pre-0.2.0 seeded output.
- `explain.lm(exact = TRUE)` still requires `X` (no default) even though the
  underlying `predict(type = "terms")` call doesn't use it for computation —
  it's only used for `feature_values` metadata when `shap_only = FALSE`.
- A `pred_wrapper` that doesn't return one prediction per row of `newdata`
  (e.g., a multiclass wrapper accidentally returning a probability matrix)
  errors informatively inside `explain_feature()`/`explain_effects_feature()`
  — don't remove that length check.
- `parallel = TRUE` requires `foreach` to be installed (Suggests, not
  Imports) and an already-registered backend; it errors informatively if
  `foreach` is missing rather than silently running sequentially.

## CI / site

GitHub Actions (r-lib/actions v2): R-CMD-check + test-coverage on pushes/PRs
to main and devel; pkgdown builds from **main only** and deploys to
`gh-pages` (served at https://bgreenwell.github.io/fastshap/). The vignette
(`vignettes/fastshap.Rmd`) is plain Rmd and runs at build time (~20s) — keep
it that way; it previously used a `.orig`-file precompile workaround because
an early draft took ~20 minutes to knit (huge `nsim`/background sizes), which
is no longer necessary now that predictions are batched. Keep chunks small
(a few hundred `nsim`, a few hundred background rows) so it stays fast.
