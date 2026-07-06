# Check C++ permutation sampler used by the method = "permutation" estimator
num_rows <- 20000
p <- 5
set.seed(888)
perm <- fastshap:::genPermMat(num_rows, p)

# Every row is a valid permutation of 1:p
expect_true(all(apply(perm, 1, function(r) setequal(r, 1:p))))

# Marginal distribution of each position should be roughly uniform over 1:p
tab <- table(perm[, 1L])
expect_true(all(round(tab / num_rows, digits = 2) == round(1 / p, digits = 2)))

# Seed-reproducible (uses R's RNG stream, not a C++-native RNG)
set.seed(55)
a <- fastshap:::genPermMat(10, 4)
set.seed(55)
b <- fastshap:::genPermMat(10, 4)
expect_identical(a, b)

# p = 1 edge case
expect_true(all(fastshap:::genPermMat(5, 1) == 1))
