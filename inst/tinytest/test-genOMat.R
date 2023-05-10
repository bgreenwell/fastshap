# Check C++ function
num_rows <- 100000
num_cols <- 10
set.seed(888)
O <- fastshap:::genOMat(num_rows, num_cols)
tab <- table(apply(O, MARGIN = 1, FUN = sum))

# Expectations
expect_true(
  all(round(tab / num_rows, digits = 2) == 1 / num_cols)
)
