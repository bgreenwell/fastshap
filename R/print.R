#' @keywords internal
#' 
#' @export
print.explain <- function(x, ...) {
  print(data.matrix(as.data.frame(x)))
  invisible(x)
}
