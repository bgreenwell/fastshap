# Benchmarks for genOMat function(s)

# #include <Rcpp.h>
# using namespace Rcpp;
# 
# // [[Rcpp::export]]
# LogicalMatrix genOMat2(int num_rows, int num_cols, int col) {
#   LogicalMatrix O(num_rows, num_cols);
#   LogicalVector Orow(num_cols - 1);
#   LogicalVector bools = LogicalVector::create(1, 0);
#   IntegerVector pos = sample(num_cols - 1, num_rows, true);  // approximate position
#   NumericVector probs;
#   for (int i = 0; i < num_rows; i++) {
#     probs = NumericVector::create(pos[i] / (num_cols - 1.0), 
#                                   1.0 - pos[i] / (num_cols - 1.0));
#     Orow = sample(bools, num_cols - 1, true, probs);
#     Orow.insert(col - 1, true);
#     O(i, _) = Orow;
#   }
#   return O;
# }
fun2 <- function(m, p, column = 3) {
  fastshap:::genOMat2(m, p, column)
}

fun1 <- function(m, p, column = 3) {
  O <- fastshap:::genOMat(m, p)
  O <- if (column == 1) {  # case 1
    cbind(TRUE, O)
  } else if (column == p) {  # case 2
    cbind(O, TRUE)
  } else {  # case 3
    cbind(  
      O[, 1:(column - 1), drop = FALSE], 
      TRUE, 
      O[, column:(p - 1), drop = FALSE]
    )
  }
  O
}

fun3 <- function(m, p, column = 3) {
  pos <- sample(p - 1, size = m, replace = TRUE)
  O <- t(sapply(pos, FUN = function(x) {
    sample(c(TRUE, FALSE), size = p - 1, replace = TRUE, 
           prob = c(x / (p - 1), 1 - x / (p - 1)))
  }))
  O <- if (column == 1) {  # case 1
    cbind(TRUE, O)
  } else if (column == p) {  # case 2
    cbind(O, TRUE)
  } else {  # case 3
    cbind(  
      O[, 1:(column - 1), drop = FALSE], 
      TRUE, 
      O[, column:(p - 1), drop = FALSE]
    )
  }
  O
}

mb <- microbenchmark(
  fun1(10000, 25),
  fun2(10000, 25),
  fun3(10000, 25),
  times = 1000
)
autoplot(mb)
