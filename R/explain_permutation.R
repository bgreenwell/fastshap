#' Permutation-walk Monte Carlo Shapley estimator
#'
#' Estimate Shapley values for *all* features at once by walking random
#' permutations from a fully-background state to the fully-foreground
#' (instance) state, flipping one feature at a time, and reading off each
#' feature's contribution as the difference between consecutive predictions
#' along the walk. Unlike [explain_feature()], which draws independent
#' coalitions per feature, this computes every feature's contribution from
#' the same walk: `p` prediction calls (plus one, cached, up-front call for
#' the all-instance endpoint) per replication yield contributions for all `p`
#' features, roughly half the cost of the default estimator for the same
#' `nsim`. The per-walk contributions telescope exactly to
#' `f(x) - f(background)`; the *averaged* output over `nsim` replications
#' still carries ordinary Monte Carlo noise, the same as the unadjusted
#' default estimator.
#'
#' @return If `raw = FALSE` (the default), an `nrow(newdata)` x `ncol(X)`
#' matrix of estimated Shapley values (averaged over `nsim` replications). If
#' `raw = TRUE`, an `nrow(newdata)` x `ncol(X)` x `nsim` array of the
#' per-replication contributions.
#'
#' @keywords internal
#'
#' @noRd
explain_permutation <- function(object, X, nsim, pred_wrapper, newdata,
                                raw = FALSE, batch_size = NULL) {
  n <- nrow(newdata)
  p <- ncol(X)
  reps_per_chunk <- if (is.null(batch_size)) {
    nsim
  } else {
    max(1L, min(nsim, batch_size %/% n))
  }
  chunks <- split(seq_len(nsim), ceiling(seq_len(nsim) / reps_per_chunk))

  # All randomness up front so results depend only on the random seed, not on
  # the chunking induced by `batch_size`
  w_idx_all <- sample(nrow(X), size = n * nsim, replace = TRUE)
  perm_all <- genPermMat(n * nsim, p)

  # Cache the all-instance endpoint prediction once; reused for every walk's
  # final step instead of re-predicting an already-known value
  fx <- as.numeric(pred_wrapper(object, newdata = newdata))

  phi_sum <- matrix(0, nrow = n, ncol = p)
  if (isTRUE(raw)) {
    raw_arr <- array(NA_real_, dim = c(n, p, nsim))
  }

  for (chunk in chunks) {
    k <- length(chunk)
    N <- n * k
    idx <- ((chunk[1L] - 1L) * n + 1L):(chunk[length(chunk)] * n)
    w_idx <- w_idx_all[idx]
    perm <- perm_all[idx, , drop = FALSE]
    Xrep <- newdata[rep(seq_len(n), times = k), , drop = FALSE]
    obs_idx <- rep(seq_len(n), times = k)
    rep_idx <- rep(seq_len(k), each = n)  # chunk-local replication index

    mask <- matrix(FALSE, nrow = N, ncol = p)
    # Chunk-local accumulator only (bounded by batch_size, not by nsim)
    delta_arr <- array(0, dim = c(n, p, k))

    # Step 0 endpoint: mask all-FALSE, i.e., pure background (no need to
    # invoke the Frankenstein machinery for an all-FALSE mask)
    pred_prev <- as.numeric(pred_wrapper(object, newdata = X[w_idx, , drop = FALSE]))

    for (step in seq_len(p)) {
      mask[cbind(seq_len(N), perm[, step])] <- TRUE
      if (step == p) {
        # Step p endpoint: mask all-TRUE, i.e., exactly `newdata`; reuse the
        # cached prediction instead of calling pred_wrapper() again
        pred_cur <- rep(fx, times = k)
      } else {
        B <- if (is.matrix(X)) {
          frankenstein_matrix(Xrep, X = X, w_idx = w_idx, O = mask, col = 1L)[[1L]]
        } else {
          frankenstein_df(Xrep, X = X, w_idx = w_idx, O = mask, col = 1L)[[1L]]
        }
        pred_cur <- as.numeric(pred_wrapper(object, newdata = B))
        if (length(pred_cur) != N) {
          stop("`pred_wrapper` must return a numeric vector with one prediction ",
               "per row of its `newdata` argument (expected length ", N,
               ", got ", length(pred_cur), ").", call. = FALSE)
        }
      }
      delta <- pred_cur - pred_prev
      feat_idx <- perm[, step]
      # Safe: (obs_idx, rep_idx) alone already uniquely identifies each of the
      # N rows (by construction of the "times"-tiling), so appending feat_idx
      # as a third index component can never collide -- no accumulation or
      # grouping logic needed.
      delta_arr[cbind(obs_idx, feat_idx, rep_idx)] <- delta
      pred_prev <- pred_cur
    }

    phi_sum <- phi_sum + matrix(rowSums(matrix(delta_arr, nrow = n * p, ncol = k)), nrow = n, ncol = p)
    if (isTRUE(raw)) {
      raw_arr[, , chunk] <- delta_arr
    }
  }

  if (isTRUE(raw)) {
    return(raw_arr)
  }
  phi_sum / nsim
}
