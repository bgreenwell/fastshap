#' #' @keywords internal
#' #' 
#' #' @export
#' print.explain <- function(x, ...) {
#'   if (is.matrix(x)) {
#'     x <- data.matrix(as.data.frame(x))
#'   } 
#'   print(x)
#'   invisible(x)
#' }
