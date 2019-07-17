#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalMatrix genOMat(DataFrame x) {
  int num_rows = x.nrows();
  int num_cols = x.length();
  LogicalMatrix O(num_rows, num_cols - 1);
  LogicalVector bools = LogicalVector::create(1, 0);
  IntegerVector pos = sample(num_cols - 1, num_rows, true);
  NumericVector probs;
  for (int i = 0; i < num_rows; i++) {
    probs = NumericVector::create(pos[i] / (num_cols - 1), 1 - pos[i] / (num_cols - 1));
    O(i, _) = sample(bools, num_cols - 1, true, probs);
  }
  return O;
}
