# Create some data frames
x <- y <- data.frame(
  x1 = 1L:5L,                                                # integer
  x2 = c(1.1, 2.1, 3.1, 4.1, 5.1),                           # numeric
  x3 = c(TRUE, TRUE, FALSE, TRUE, FALSE),                    # logical
  x4 = factor(c("A", "B", "A", "B", "B")),                   # nominal factor
  x5 = ordered(c("low", "medium", "medium", "low", "high"),  # ordered factor
               levels = c("low", "medium", "high"))          
)
z1 <- data.frame(
  x1 = c("1", "2"),
  x2 = c("1.1", "4.1"),
  x3 = c("TRUE", "FALSE"),
  x4 = c("A", "A"),
  x5 = c("high", "low")
)
z2 <- fastshap:::copy_classes(z1, y)

# Generate logical matrix for subsetting
o <- matrix(sample(c(TRUE, FALSE), size = 25, replace = TRUE), nrow = 5)

# Swap values
x[o] <- y[o]

# Expectations
expect_false(identical(x, y))
expect_identical(fastshap:::copy_classes(x, y), y)
expect_identical(sapply(z2, FUN = class), sapply(y, FUN = class))
