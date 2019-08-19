# replicate2 <- function(n, expr, FUN.VALUE) {  # slightly faster version
#   vapply(integer(n), FUN = eval.parent(substitute(function(...) expr)), 
#          FUN.VALUE = FUN.VALUE) 
# }


#' Parallel version of \code{replicate}
#' 
#' Parallel version of base R's \code{\link[base]{replicate}} function.
#' 
#' @param Integer specifying the number of replications.
#' 
#' @param expr The expression to evaluate repeatedly.
#' 
#' @param simplify Logical or character string specifying whether or not the 
#' result be simplified to a vector, matrix, or higher dimensional array if 
#' possible.
#' 
#' @param ... Additional optional arguments to be passed on to 
#' \code{\link[plyr]{llply}}.
#' 
#' @export
par_replicate <- function(n, expr, simplify = "array", ...) {
  answer <- plyr::llply(
    .data = integer(n),
    .fun = eval.parent(substitute(function(...) expr)),
    ...
  )
  if (!isFALSE(simplify) && length(answer)) {
    simplify2array(answer, higher = (simplify == "array"))
  } else {
    answer
  }
}


# Becnhmark
library(microbenchmark)
library(doParallel)
registerDoParallel(cores = 8)
mb <- microbenchmark(
  par_replicate(1000, mean(rnorm(10000))),
  par_replicate(1000, mean(rnorm(10000)), .parallel = TRUE),
  times = 100L
)
