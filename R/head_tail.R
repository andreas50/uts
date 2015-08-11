#' Return the First or Last Part
#' 
#' Return a subperiod time series with the first or last part.
#' 
#' @param x a \code{"uts"} object.
#' @param n a single integer. If positive, length of the the resulting time series. If negative, all but the \code{n} last/first number of observations of \code{x}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{head}} and \code{\link{tail}} in base \R.
#' @keywords internal
#' @examples
#' head(ex_uts(), 2)
#' head(ex_uts(), -2)
#' head(ex_uts(), -6)
head.uts <- function(x, n=6L, ...)
{
  # Determine affected indices
  pos <- seq(length=min(abs(n), length(x)))
  
  # Return first part
  if (n > 0) {
    x$values <- x$values[pos]
    x$times <- x$times[pos]
  }
  
  # Return all but the first part
  if (n < 0) {
    x$values <- x$values[-pos]
    x$times <- x$times[-pos]
  }
  x
}


#' @rdname head.uts
#' 
#' @keywords internal
#' @examples
#' tail(ex_uts(), 2)
#' tail(ex_uts(), -2)
#' tail(ex_uts(), -6)
tail.uts <- function(x, n=6L, ...)
{
  # Determine affected indices
  len <- length(x)
  n_capped <- min(abs(n), len)
  pos <- rev(len + 1 - seq(length=n_capped))
  
  # Return last part
  if (n > 0) {
    x$values <- x$values[pos]
    x$times <- x$times[pos]
  }
  
  # Return all but the last part
  if (n < 0) {
    x$values <- x$values[-pos]
    x$times <- x$times[-pos]
  }
  x
}