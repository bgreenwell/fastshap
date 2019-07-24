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
  NumericVector probs;
  
  // Inititalize logical vector to sample from in each row of output
  LogicalVector bools = LogicalVector::create(1, 0);

  // Simulate the number of features that appear before the feature of interest
  // in each random permutation; note that each element can range from 0 to 
  // (num_cols - 1) with equal probability
  IntegerVector num_feat = sample(num_cols, num_rows, true) - 1;

  // Build up logical matrix row by row (essentially, a true indicates that the
  // corresponding feature appears before the feature of interest in that 
  // particular random permutation)
  for (int i = 0; i < num_rows; i++) {
    
    // Generate sampling probabilities to use for row i
    probs = NumericVector::create(num_feat[i] / (num_cols - 1.0), 
                                  1.0 - num_feat[i] / (num_cols - 1.0));

    // Construct row i of O by sampling from the vector {true, false} with the
    // probability weights stored in probs
    O(i, _) = sample(bools, num_cols - 1, true, probs);

  }
  
  // FIXME: For some reason we tend to get more extremes in the matrix below 
  // (i.e., more occurences of 0 true's and (num_cols - 1) true's) when the 
  // number of trues in any row should be uniform between 0 and (num_cols - 1).
  //
  // Run the following in R to check for discrete uniform:
  //
  // > O <- fastshap:::genOMat(100000, 100)
  // > num_TRUE <- apply(O, MARGIN = 1, FUN = sum)
  // > (tab <- table(num_TRUE))
  // > barplot(tab, xlab = "Frequency of TRUE's accross columns")
  
  // Return results
  return O;

}


//
// Both of the approaches below are equivalent, but to my surprise, slower than
// the approach above
//

// // [[Rcpp::export]]
// LogicalMatrix genOMat2(int num_rows, int num_cols, int feature_col) {
//   
//   // Parameters
//   // ----------
//   //
//   //   num_rows: Integer specifying the number of instances to explain
//   //   num_cols: Integer specifying the total number of features available in 
//   //             the training data
//   //   feature_col: Integer specifying the feature column of interest
//   //
//   // Results
//   // -------
//   //
//   //   A LogicalMatrix matrix with dimension num_rows X num_cols
//   
//   // Inititalize variables to store results
//   LogicalMatrix O(num_rows, num_cols);
//   LogicalVector Orow(num_cols - 1);
//   NumericVector probs;
//   
//   // Inititalize logical vector to sample from in each row of output
//   LogicalVector bools = LogicalVector::create(1, 0);
//   
//   // Simulate the number of features that appear before the feature of interest
//   // in each random permutation; note that each element can range from 0 to 
//   // (num_cols - 1) with equal probability
//   IntegerVector num_feat = sample(num_cols, num_rows, true) - 1;
//   
//   // Build up logical matrix row by row (essentially, a true indicates that the
//   // corresponding feature appears before the feature of interest in that 
//   // particular random permutation)
//   for (int i = 0; i < num_rows; i++) {
//     
//     // Generate sampling probabilities to use for row i
//     probs = NumericVector::create(num_feat[i] / (num_cols - 1.0), 
//                                   1.0 - num_feat[i] / (num_cols - 1.0));
//     
//     // Construct row i
//     Orow = sample(bools, num_cols - 1, true, probs);
//     Orow.insert(feature_col, true);
//     O(i, _) = Orow;
//     
//   }
//   
//   // Return results
//   return O;
//   
// }


// // [[Rcpp::export]]
// LogicalMatrix genOMat3(int num_rows, int num_cols) {
//   
//   // Parameters
//   // ----------
//   //
//   //   num_rows: Integer specifying the number of instances to explain
//   //   num_cols: Integer specifying the total number of features available in 
//   //             the training data
//   //
//   // Results
//   // -------
//   //
//   //   A LogicalMatrix matrix with dimension num_rows X (num_cols - 1)
//   
//   // Inititalize variables to store results
//   LogicalMatrix O(num_rows, num_cols - 1);
// 
//   // Inititalize logical vector to sample from in each row of output
//   LogicalVector bools = LogicalVector::create(1, 0);
//   
//   // Simulate the number of features that appear before the feature of interest
//   // in each random permutation; note that each element can range from 0 to 
//   // (num_cols - 1) with equal probability
//   IntegerVector num_feat = sample(num_cols, num_rows, true) - 1;
//   
//   // Build up logical matrix row by row (essentially, a true indicates that the
//   // corresponding feature appears before the feature of interest in that 
//   // particular random permutation)
//   for (int i = 0; i < num_rows; i++) {
// 
//     // Construct row i
//     O(i, _) = rbinom(num_cols - 1, 1, num_feat[i] / (num_cols - 1.0));
//     
//   }
//   
//   // FIXME: For some reason we tend to get more extremes in the matrix below 
//   // (i.e., more occurences of 0 true's and (num_cols - 1) true's) when the 
//   // number of trues in any row should be uniform between 0 and (num_cols - 1).
//   
//   // Return results
//   return O;
//   
// }
