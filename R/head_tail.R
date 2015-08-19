######################################################
# Head and Tail (in terms of number of observations) #
######################################################

#' Return the First or Last Part
#' 
#' Return a subperiod time series with the first or last part. The length of the output is specified in terms of the \emph{number of observations}.
#' 
#' @note For an evenly spaced time series, the methods \code{head()} and \code{head_t()} (or \code{tail()} and \code{tail_t()}) essentially work the same. For unevenly spaced time series, however, they are quite different. The subperiod window of the former is determined by the observation \emph{values}, while for the latter it is determined by the observation \emph{times}.
#' 
#' @param x a \code{"uts"} object.
#' @param n a single integer. If positive, length of the the resulting time series. If negative, all but the \code{n} last/first number of observations of \code{x}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{head_t}}, \code{\link{tail_t}}, \code{\link{window}} for other methods that extract a subperiod time series.
#' @examples
#' head(ex_uts(), 2)
#' head(ex_uts(), -2)
#' head(ex_uts(), -6)
head.uts <- function(x, n=6L, ...)
{
  # Argument checking
  if (is.duration(n))
    stop("'n' is a duration object instead of an integer")
  
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
#' @examples
#' tail(ex_uts(), 1)
#' tail(ex_uts(), -1)
#' tail(ex_uts(), -6)
tail.uts <- function(x, n=6L, ...)
{
  # Argument checking
  if (is.duration(n))
    stop("'n' is a duration object instead of an integer")
  
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
