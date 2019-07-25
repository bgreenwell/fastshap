#include <Rcpp.h>
using namespace Rcpp;


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
