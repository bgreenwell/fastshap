#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalMatrix genOMat2(int num_rows, int num_cols) {
  LogicalMatrix O(num_rows, num_cols - 1);
  LogicalVector bools = LogicalVector::create(1, 0);
  IntegerVector pos = sample(num_cols - 1, num_rows, true);  // approximate position
  NumericVector probs;
  for (int i = 0; i < num_rows; i++) {
    probs = NumericVector::create(pos[i] / (num_cols - 1.0), 
                                  1.0 - pos[i] / (num_cols - 1.0));
    O(i, _) = sample(bools, num_cols - 1, true, probs);
  }
  return O;
}
