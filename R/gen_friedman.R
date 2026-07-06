#' Friedman benchmark data
#'
#' Simulate data from the Friedman 1 benchmark problem. These data were
#' originally described in Friedman (1991) and Breiman (1996). For details, see
#' [sklearn.datasets.make_friedman1](https://scikit-learn.org/stable/modules/generated/sklearn.datasets.make_friedman1.html).
#'
#' @param n_samples Integer specifying the number of samples (i.e., rows) to
#' generate. Default is 100.
#'
#' @param n_features Integer specifying the number of features to generate.
#' Default is 10.
#'
#' @param sigma Numeric specifying the standard deviation of the noise.
#'
#' @param seed Integer specifying the random seed. If `NULL` (the default)
#' the results will be different each time the function is run.
#'
#' @note This function is mostly used for internal testing.
#'
#' @references
#' Breiman, Leo (1996) Bagging predictors. Machine Learning 24, pages 123-140.
#'
#' Friedman, Jerome H. (1991) Multivariate adaptive regression splines. The
#' Annals of Statistics 19 (1), pages 1-67.
#'
#' @export
#'
#' @examples
#' gen_friedman()
gen_friedman <- function(n_samples = 100, n_features = 10, sigma = 0.1,
                         seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  x <- matrix(stats::runif(n_samples * n_features), ncol = n_features)
  colnames(x) <- paste0("x", seq_len(n_features))
  y = 10 * sin(pi * x[, 1L] * x[, 2L]) + 20 * (x[, 3L] - 0.5) ^ 2 +
    10 * x[, 4L] + 5 * x[, 5L] + stats::rnorm(n_samples, sd = sigma)
  as.data.frame(cbind(y = y, x))
}
