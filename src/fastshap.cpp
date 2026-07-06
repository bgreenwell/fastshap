#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
LogicalMatrix genOMat(int num_rows, int num_cols) {

  // Parameters
  // ----------
  //
  //   num_rows: Integer specifying the number of instances to explain
  //   num_cols: Integer specifying the total number of features available in
  //             the training data
  //
  // Results
  // -------
  //
  //   A LogicalMatrix matrix with dimension num_rows X (num_cols - 1)

  // Inititalize variables to store results
  LogicalMatrix O(num_rows, num_cols - 1);
  LogicalVector Orow(num_cols - 1);

  // Simulate the number of features that appear before the feature of interest
  // in each random permutation; note that each element can range from 0 to
  // (num_cols - 1) with equal probability
  IntegerVector num_feat = sample(num_cols, num_rows, true) - 1;

  // Build up logical matrix row by row (essentially, a true indicates that the
  // corresponding feature appears before the feature of interest in that
  // particular random permutation)
  for (int i = 0; i < num_rows; i++) {

    // Construct row i of O
    if (num_feat[i] == num_cols - 1) {  // feature appears at end of random permutation
      Orow.fill(true);
    } else if (num_feat[i] > 0) {  // feature appears somewhere in middle of random permutation
      Orow[sample(num_cols - 1, num_feat[i], false) - 1] = true;
    }
    O(i, _) = Orow;
    Orow.fill(false);  // reset to all false

  }

  // Return results
  return O;

}


// [[Rcpp::export]]
IntegerMatrix genPermMat(int N, int p) {

  // Parameters
  // ----------
  //
  //   N: Integer specifying the number of independent random permutations to
  //      draw (one per Monte Carlo "walk")
  //   p: Integer specifying the number of features (i.e., columns of X)
  //
  // Results
  // -------
  //
  //   An N x p integer matrix; each row is an independent random permutation
  //   of 1:p, giving the order in which features are flipped from background
  //   to foreground along that walk.

  IntegerMatrix perm(N, p);
  for (int i = 0; i < N; i++) {
    perm(i, _) = sample(p, p, false);  // Rcpp sugar; consumes R's RNG stream
  }
  return perm;

}


// [[Rcpp::export]]
List genFrankensteinMatrices(const arma::mat& X, const arma::mat& W,
                             const arma::umat& O, int feature) {

  // Parameters
  // ----------
  //
  //   X: Matrix of feature values for the instances being explained
  //   W: Matrix of sampled background feature values (same dimension as X)
  //   O: Logical matrix; a true at (i, j) indicates that feature j appears
  //      before the feature of interest in the i-th random permutation.
  //      Column `feature` must be all true.
  //   feature: 1-based column index of the feature being explained
  //
  // Results
  // -------
  //
  //   A list of two matrices. B1 takes the feature of interest (and every
  //   feature appearing before it) from X and the rest from W; B2 is
  //   identical except the feature of interest comes from W.

  // B1 starts as a copy of X, so only the positions taken from W (i.e.,
  // where O is false) need to be assigned
  arma::mat B1 = X;
  arma::uvec notu = arma::find(O == 0);
  B1.elem( notu ) = W.elem( notu );

  // B2 differs from B1 in the feature-of-interest column only
  arma::mat B2 = B1;
  B2.col(feature - 1) = W.col(feature - 1);

  // Return list of results
  List out(2);
  out(0) = B1;
  out(1) = B2;
  return out;

}
