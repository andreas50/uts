#' Number of Observations
#' 
#' Return the number of time series observations.
#' 
#' @note For an evenly spaced time series there is a one-to-one correspondence between (1) the \emph{number} of observations, and (2) \emph{temporal} length. For unevenly spaced time series, however, these two concepts are quite different.s.
#' 
#' @param x a \code{"uts"} object.
#' 
#' @seealso \code{\link{length_t}} allows to get the temporal length of a time series.
#' @examples 
#' length(ex_uts())
#' length(uts())
length.uts <- function(x)
{
  length(x$times)
}


#' Temporal Length
#'
#' Return the temporal length of a time series, i.e. the time difference between the first and last observation time.
#'
#' @note For an evenly spaced time series there is a one-to-one correspondence between (1) the \emph{number} of observations, and (2) \emph{temporal} length. For unevenly spaced time series, however, these two concepts are quite different.
#'
#' @return A \code{\link{difftime}} object.
#' @param x a time series object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{length.uts}} allows to get the \emph{number} observations.
length_t <- function(x, ...) UseMethod("length_t")


#' @describeIn length_t temporal length of \code{"uts"} object.
#' 
#' @examples
#' length_t(ex_uts())
#' length_t(ex_uts2())
#' length_t(uts())
length_t.uts <- function(x,  ...)
{
  end(x) - start(x)
}
