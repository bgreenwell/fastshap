#' Arrange multiple grobs on a page
#'
#' See \code{\link[gridExtra]{grid.arrange}} for more details.
#' 
#' @return A \code{"gtable"} object; see 
#' \code{\link[gridExtra]{gridExtra-package}} for details.
#'
#' @name grid.arrange
#' 
#' @rdname grid.arrange
#' 
#' @keywords internal
#' 
#' @export
#' 
#' @importFrom gridExtra grid.arrange
#' 
#' @usage grid.arrange(..., newpage = TRUE)
NULL


#' Copy column classes
#' 
#' Copy column classes of \code{y} on to \code{x}.
#' 
#' @param x A data frame.
#' 
#' @param y A data frame.
#' 
#' @keywords internal
#' 
#' @noRd
copy_classes <- function(x, y) {
  x.names <- names(x)
  y.names <- names(y)
  if (length(setdiff(x.names, y.names)) > 0) {
    stop("Data frame x contains columns not present in data frame y.")
  }
  column.names <- intersect(x.names, y.names)
  for (name in column.names) {
    # Do the classes match? If factors, do they have the same levels?
    if (!identical(class(x[[name]]), class(y[[name]])) ||
        !identical(levels(x[[name]]), levels(y[[name]]))) {
      # Convert to numeric or integer class
      if (is.numeric(y[[name]])) {
        if (is.integer(y[[name]])) {
          x[[name]] <- as.integer(x[[name]])
        } else {
          x[[name]] <- as.numeric(x[[name]])
        }
      }
      # Convert to factor or ordered class
      if (is.factor(y[[name]])) {
        if (is.ordered(y[[name]])) {
          x[[name]] <- ordered(x[[name]], levels = levels(y[[name]]))
        } else {
          x[[name]] <- factor(x[[name]], levels = levels(y[[name]]))
        }
      }
      # Convert to character
      if (is.character(y[[name]])) {
        x[[name]] <- as.character(x[[name]])
      }
      # Convert to logical
      if (is.logical(y[[name]])) {
        x[[name]] <- if(getRversion() <= "3.6.0") {
          as.logical(trimws(x[[name]]))  # " TRUE" -> "TRUE"
        } else {
          as.logical(x[[name]])
        }
      }
    }
  }
  # Sanity check
  stopifnot(all.equal(
    target = sapply(x[column.names], class),
    current = sapply(y[column.names], class))
  )
  x  # return x with copied classes
}
