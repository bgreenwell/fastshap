# Performance roadmap for `explain()`

Analysis of where `explain()` spends its time and a prioritized list of
optimizations. Line references are anchored to commit `a1ad363`; verify them
against the current source before acting on this document.

## Where the time goes

The Monte Carlo estimator calls `explain_column()` once per (feature, sim)
pair, and each call invokes `pred_wrapper` twice — once per Frankenstein
matrix (`R/explain.R:76-77`). A full run therefore makes `2 * p * nsim`
prediction calls of `nrow(newdata)` rows each.

Profiling (`Rprof`, ranger with 100 trees, n = 500, p = 10, nsim = 10):

| Path       | Time in `predict()` | Time in R machinery |
|------------|---------------------|---------------------|
| data.frame | ~85%                | ~10% (`[<-.data.frame`, `unlist`, `copy_classes`) |
| matrix     | ~93%                | ~5%                 |

**Conclusion:** the dominant cost is the number and granularity of
`pred_wrapper` calls, not the C++ or the sampling machinery. The biggest wins
come from (a) fewer, larger prediction calls and (b) needing fewer simulations
for the same accuracy. Micro-optimizing `genOMat()`/`genFrankensteinMatrices()`
helps only the cheap-model case.

## Recommendations (by impact)

### 1. Batch prediction calls (big win, backward compatible)

Generate all Frankenstein matrices for a feature (or for all features) up
front, `rbind` them, and call `pred_wrapper` once — or in chunks capped by a
new `batch_size` argument to control peak memory.

- Measured 1.6x with ranger even at already-decent 500-row calls
  (200 calls x 500 rows = 0.53 s vs. 1 call x 100k rows = 0.32 s).
- Gains are far larger for predict functions with high per-call overhead:
  xgboost on small batches, keras/torch, tidymodels workflows that re-run
  preprocessing on every call.
- The single-row fast path already does exactly this by stacking the
  observation `nsim` times and recursing with `nsim = 1`
  (`R/explain.R:310-324`). Batching is the generalization of that trick to
  multi-row `newdata`.
- Bonus: one big call lets the model's own threaded predict (ranger, xgboost,
  lightgbm) supply the parallelism. This beats `foreach`-over-features, which
  serializes the model and data to every worker and offers at most p-way
  parallelism.

### 2. Fix the `newdata` path's `genOMat()` waste (bug-level, one-line fix)

At `R/explain.R:33`, when `newdata` is supplied, `O` is generated at the
*background* size and then row-subsampled:

```r
O <- genOMat(n, p)[sample(n, size = nrow(newdata), replace = TRUE), , drop = FALSE]
```

Rows of `O` are iid, so `genOMat(nrow(newdata), p)` is distributionally
identical. Measured cost at n = 100k: ~58 ms per call vs. ~0 — incurred once
per (feature, sim). Explaining 10 rows against a 100k-row background with
p = 10, nsim = 50 burns ~30 s generating random rows that are thrown away.

Note: changing this alters the RNG stream, so seeded results will differ from
previous versions. Flag in NEWS.

### 3. Build B2 from B1 with a single column swap

`B[[1]]` and `B[[2]]` differ *only* in the feature-of-interest column (B1
takes it from `X`, B2 from `W`). Both code paths currently rebuild the entire
second matrix.

- C++ (`src/fastshap.cpp:52-77`): replace the second `find()` pass and two
  `elem()` assignments with `B2 = B1; B2.col(feature - 1) = W.col(feature - 1)`.
  Also drop `B1.elem(u) = X.elem(u)` — it is a no-op because `B1` is
  initialized as a copy of `X`.
- While there, pass `const arma::mat&` / `const arma::umat&` instead of by
  value: `X`, `W`, and `O` are all copied on every call today.
- R data-frame branch (`R/explain.R:63-68`): same trick —
  `B[[2L]] <- B[[1L]]; B[[2L]][[column]] <- W[[column]]` kills two of the four
  slow matrix-indexed assignments.

### 4. Column-wise construction on the data-frame path

`B[[1L]][O] <- X[O]` (`R/explain.R:64`) uses matrix-style logical indexing of
a data frame — one of the slowest operations in base R (per-cell assignment
with copies). Build each column instead:

```r
b <- X[[j]]
keep <- O[, j]
b[!keep] <- W[[j]][!keep]
```

This is much faster *and* preserves column classes automatically, which makes
the two `copy_classes()` calls in the hot loop (`R/explain.R:72-73`, each with
a `sapply` + `all.equal` sanity check per invocation) unnecessary. Similarly,
`W <- X[sample(n, replace = TRUE), ]` does a full data-frame row-subset per
(feature, sim); sample indices once and gather column-wise.

### 5. Permutation-walk SHAP (the algorithmic upgrade)

The Strumbelj–Kononenko scheme spends `2p` evaluations per permutation to get
*one* feature's contribution. The permutation-SHAP formulation (Python shap's
`PermutationExplainer`; R packages {kernelshap} and {permshap}) walks one
permutation through **all** features, flipping one feature at a time from
background to foreground: `p + 1` evaluations yield contributions for all `p`
features via consecutive differences.

- ~2x fewer predictions per permutation.
- The differences telescope, so contributions sum exactly to
  `f(x) - f(background)`: local accuracy holds by construction, making the
  `adjust = TRUE` machinery (and its `nsim > 1` requirement) obsolete on this
  path.
- Forward + reverse walks (antithetic pairs) roughly halve variance again for
  near-additive models.

Bigger lift; expose as `method = "permutation"` rather than replacing the
current estimator. This is the change that makes fastshap competitive with
modern explainers.

### 6. Variance reduction: accuracy per unit compute

Even within the current estimator, these cut the `nsim` needed for a given
standard error — often worth more than a constant-factor speedup:

- Antithetic sampling: use each permutation and its reversal.
- Stratify the coalition-size draw in `genOMat()` (currently uniform via
  `sample(num_cols, num_rows, true) - 1`, `src/fastshap.cpp:27`) — balanced
  allocation across sizes, or pair sizes `k` with `p - 1 - k`.
- Per-observation convergence-based early stopping: stop simulating rows whose
  CI is already tight. `inst/tinytest/test-convergence.R` provides scaffolding
  for validating this.

#### Issue #86: pin the extreme coalitions (reviewed 2026-07-02)

[GH #86](https://github.com/bgreenwell/fastshap/issues/86) proposes always
including the two highest-weight coalitions — `S = {}` (pure main effect of
feature j) and `S = all-but-j` (total effect) — deterministically, sampling
only interior coalitions, and reweighting (slides 9–11 of the attached deck;
same idea as {kernelshap}'s "hybrid" strategy).

**The math checks out.** Under the permutation distribution the size of the
before-j coalition is uniform on `{0, ..., p-1}` (exactly what `genOMat()`
samples via `sample(num_cols, num_rows, true) - 1`). There is only one
coalition at each extreme size, so those two specific coalitions carry
probability `1/p` each — `2/p` combined (0.4 at p = 5, matching slide 10).
The stratified estimator is unbiased:

```
phi_j = (1/p) * mean(D_empty)                 # D_empty = f(w <- x_j) - f(w)
      + (1/p) * mean(D_full)                  # D_full  = f(x) - f(x <- w_j)
      + ((p-2)/p) * mean(D_interior)          # sizes uniform on {1..p-2}
```

**Measured gains are real but modest** (equal prediction budget per feature,
1000 replications, one-background-row-per-draw structure mirroring
`explain_column()`):

| Scenario                    | Variance reduction |
|-----------------------------|--------------------|
| pure interactions, p = 5    | 1.30x              |
| friedman, p = 5             | 1.24x              |
| friedman + noise, p = 10    | 1.08x              |
| friedman + noise, p = 20    | 1.00x (none)       |

**Why smaller than the "40% of the weight" framing suggests:** each fastshap
draw couples the coalition with a *single* background row `w`, and the
dominant noise source is the `w` draw — which pinning the coalition cannot
remove. Stratification eliminates only the between-coalition-stratum variance
component, and the fixed weight shrinks as `2/p`. The trick pays off much more
in estimators that average each coalition's value over many background rows
per draw (e.g., kernelshap's), where coalition-choice noise dominates.

Implementation notes, if pursued:

- The two extreme Frankenstein matrices are single-column swaps of `W` and
  `newdata` — no `genOMat()`/`O` machinery needed for them. Composes with
  item #3.
- `f(x)` is deterministic and already computed when `adjust = TRUE`
  (`R/explain.R:291`); `f(w)` can be shared across all p features by sharing
  `W` within a sim. Extremes therefore cost roughly half an interior draw.
- Allocation: a couple of draws on each extreme, the rest interior — weights
  roughly `1 : 1 : (p-2)` since within-stratum variances are comparable
  (all dominated by `w` noise). Empirically (2, 2, 8) beat (5, 5, 5).
- Interior sampling is a one-line change in `genOMat()`: draw `num_feat`
  uniform on `{1, ..., p-2}` (`src/fastshap.cpp:27`), plus the reweighting in
  R. Needs a `p >= 3` guard.
- `p <= 2` becomes *exact* with `2p + 2` evaluations — currently Monte-Carloed.
- Byproduct worth exposing regardless of whether the hybrid estimator lands:
  `D_empty` (pure main effect) and `D_full` (total effect) are interpretable
  per-feature diagnostics — see the next subsection.
- Changes seeded results; the `adjust = TRUE` variance bookkeeping must treat
  only the interior portion as sampled.

**Verdict:** sound and cheap to add, but modest in fastshap's sampling
structure — worth ~1.1–1.3x for p <= 10, negligible for p >= 20. Priority
below batching (#1) and the quick wins (#2–#4). If the permutation-walk
estimator (#5) lands, revisit: kernelshap-style hybrid with batched exact
extremes is the fuller expression of this idea.

#### Exposing the pinned terms: cheap main/total effect decomposition

The two coalitions issue #86 pins are interpretable quantities in their own
right, computable in `2p + 2` predictions per background draw with **no
coalition sampling at all** — and deterministic if averaged over the full
background:

- `main_j = E_w[f(w <- x_j)] - E_w[f(w)]` — the effect of feature j *alone*
  (no other features of x present). This is exactly the **centered partial
  dependence** of feature j evaluated at `x_j` — i.e., per-observation PD
  values; direct conceptual bridge to {pdp}.
- `total_j = f(x) - E_w[f(x <- w_j)]` — the effect of feature j *in the
  context of the full observation* (main effect plus all interactions
  involving j). The local analog of permutation importance / LOCO.
- `gap_j = total_j - main_j` — interaction content involving j at x. Zero iff
  j enters (this region of) the model additively.

Verified properties (friedman function, full-background averaging, exact
Shapley by brute force):

| Property | Result |
|----------|--------|
| Additive features | `main = total = exact SHAP` exactly |
| Pairwise interaction (`sin(pi*x1*x2)`) | `gap` splits equally across both partners |
| `(main + total)/2` vs exact SHAP | **Exact for models with at most pairwise interactions** (E[delta \| size] is linear in coalition size, so the two-endpoint average equals the uniform-over-sizes average); biased for 3-way+ (measured ~3% relative error on `50*x1*x2*x3`) |
| `mean(abs(gap))` over observations | Clean global interaction-strength measure: 1.09/1.09/0/0/0 on friedman — a cheap alternative to `vip::vint()`'s Friedman-H approach |

API options (not yet decided):

1. **Standalone `explain_effects()`** (preferred starting point): returns
   `main`, `total`, `interaction` (gap), `shap1` (= `(main+total)/2`), and
   `baseline` as n x p matrices. Default to full-background averaging
   (deterministic) when `nrow(X) * nrow(newdata)` is manageable, else sample
   `nsim` background draws. All matrices are single-column swaps (item #3);
   trivially batchable (item #1). Use cases: fast screening before choosing
   `nsim`, exact explanations for additive/pairwise models (GAMs, EBMs,
   depth-2 GBMs), interaction diagnostics.
2. **Rider on `explain()`**: `effects = TRUE` attaches `main`/`total` as
   attributes (or extra list components when `shap_only = FALSE`). If the
   hybrid estimator is implemented these are free (they *are* the pinned
   strata). Check shapviz tolerance for extra list components first
   (`inst/tinytest/test-shapviz.R`).
3. **Feed {vip}**: `mean(abs(gap))` as interaction importance;
   `mean(abs(main))` is the Greenwell-Boehmke-McCarthy (2018) PD-based
   importance evaluated at data points.
4. **Internal use in `explain()`**: for `p <= 2`, return the exact answer via
   these terms instead of Monte Carlo.

Caveats to document: marginal/interventional sampling (same correlated-feature
caveat as the rest of fastshap); `gap` is a diagnostic, not the formal SHAP
interaction-value decomposition; `shap1` exactness holds only up to pairwise
interactions.

### 7. Smaller items (cheap, do opportunistically)

- Move the `cbind` insertion of the TRUE column (`R/explain.R:42-52`) into
  `genOMat()` so C++ returns the final matrix. Saves two full-matrix copies
  plus the LogicalMatrix-to-`umat` coercion per call.
- Preallocate the `n x p` result matrix and fill it, rather than the
  `foreach`-list + `sapply` combine (`R/explain.R:417-448`).
- `raw = TRUE` path: fill a preallocated 3-D array instead of
  `unlist` + `aperm` (`R/explain.R:434`).
- `genOMat()` allocates inside its row loop (`Orow`, per-row `sample()`);
  a preallocated partial Fisher–Yates would avoid this. Minor — only matters
  for cheap models.

## Suggested sequencing

| Step | Items | Effort | Risk |
|------|-------|--------|------|
| 1    | #2, #3, #4 (quick wins) | an afternoon | low — pure refactors, existing tests catch regressions |
| 2    | #1 (`batch_size`)       | a few days   | low — backward compatible, biggest user-visible speedup |
| 3    | #6 (variance reduction, incl. GH #86) | incremental | medium — statistical validation needed |
| 4    | #5 (`method = "permutation"`) | v0.2-worthy feature | medium — new estimator, new tests |

## Testing notes

- `inst/tinytest/test-fastshap-vs-other-pkgs.R` and
  `inst/tinytest/test-convergence.R` catch statistical regressions (compare
  approximate vs. exact values for lm/xgboost).
- Items #2 and any RNG restructuring change seeded results; update any
  snapshot-style expectations and mention in NEWS.
- Per `AGENTS.md`: TDD with tinytest, then `devtools::document()` and a clean
  `devtools::check()` before committing.

## Benchmark reproduction

```r
devtools::load_all()
set.seed(101)
df <- gen_friedman(500)
X <- subset(df, select = -y)
fit <- ranger::ranger(y ~ ., data = df, num.trees = 100)
pfun <- function(object, newdata) predict(object, data = newdata)$predictions

# Profile
Rprof(interval = 0.005)
shap <- explain(fit, X = X, nsim = 10, pred_wrapper = pfun)
Rprof(NULL)
head(summaryRprof()$by.self, 15)

# Batching comparison (matrix interface)
Xm <- as.matrix(X)
fit2 <- ranger::ranger(x = Xm, y = df$y, num.trees = 100)
system.time(for (i in 1:200) predict(fit2, data = Xm)$predictions)          # many small calls
system.time(predict(fit2, data = Xm[rep(1:500, times = 200), ])$predictions) # one big call

# genOMat sizing waste
system.time(for (i in 1:100) fastshap:::genOMat(100000, 10))  # newdata path today
system.time(for (i in 1:100) fastshap:::genOMat(10, 10))      # what it should cost
```

Issue #86 hybrid-estimator benchmark (equal budget, current vs. stratified):

```r
run_bench <- function(f, p, label, R = 1000) {
  Xb <- matrix(runif(500 * p), ncol = p); x0 <- runif(p)
  draw_delta <- function(j, s) {  # one MC draw at coalition size s
    w <- Xb[sample(500, 1), ]
    S <- if (s > 0) sample(setdiff(1:p, j), s) else integer(0)
    b1 <- w; b1[c(S, j)] <- x0[c(S, j)]
    b2 <- w; b2[S] <- x0[S]
    f(rbind(b1)) - f(rbind(b2))
  }
  cur <- replicate(R, sapply(1:p, function(j)  # 10 draws, size ~ U{0..p-1}
    mean(replicate(10, draw_delta(j, sample(0:(p - 1), 1))))))
  hyb <- replicate(R, sapply(1:p, function(j) {  # (2, 2, 8) allocation
    (1/p) * mean(replicate(2, draw_delta(j, 0))) +
    (1/p) * mean(replicate(2, draw_delta(j, p - 1))) +
    ((p - 2)/p) * mean(replicate(8, draw_delta(j, sample(1:(p - 2), 1))))
  }))
  sc <- apply(cur, 1, sd); sh <- apply(hyb, 1, sd)
  vr <- (sc[sc > 1e-12] / sh[sc > 1e-12])^2  # drop zero-variance noise features
  cat(sprintf("%-30s p=%2d  var ratio: %.2f [%.2f, %.2f]\n",
              label, p, exp(mean(log(vr))), min(vr), max(vr)))
}
set.seed(2)
f_fri <- function(M) 10*sin(pi*M[,1]*M[,2]) + 20*(M[,3] - .5)^2 + 10*M[,4] + 5*M[,5]
run_bench(function(M) 50*M[,1]*M[,2]*M[,3] + 10*M[,4]*M[,5], 5, "pure interactions")
run_bench(f_fri,  5, "friedman")
run_bench(f_fri, 10, "friedman + 5 noise features")
run_bench(f_fri, 20, "friedman + 15 noise features")
```
