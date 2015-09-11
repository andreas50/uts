#' Lag Observation Values
#' 
#' Compute a lagged version of a time series by shifting observation values.
#' 
#' Each observation time-value tuple \eqn{(t[n], x[n])} in the original time series is replaced by \eqn{(t[n], x[n-k])} in the lagged time series. Observations without corresponding un-lagged value (for example, the second observation for lag \code{k=3}) are dropped from the output.
#' 
#' @return A \code{"uts"} object with the same observation times (apart from dropped observations) as \code{x}.
#' @param x a \code{"uts"} object.
#' @param k the number of lags (in units of observations).
#' @param \dots further arguments passed to or from methods.
#' 
#' @note For an evenly spaced time series (1) shifting observation \emph{times}, and (2) shifting observation \emph{values} essentinally gives the same result. For unevenly spaced time series, however, these two operations are quite different. The former affects only the observation \emph{times}, while the latter affects only the observation \emph{values} (apart from observations that are dropped).
#' 
#' @seealso \code{\link[stats:lag]{lag}} in base \R.
#' @seealso \code{\link{lag_t}} allows to shift observation \emph{times}, as opposed to observation \emph{values}.
#' @examples
#' # Shift observations values forward by one observation
#' lag(ex_uts(), k=1)
#' 
#' # Shift observations values forward by two observations
#' lag(ex_uts(), k=-2)
#' 
#' # If the lag 'k' is >= the length of the time series, all observations are dropped
#' lag(ex_uts(), k=6)
lag.uts <- function(x, k=1, ...)
{
  # Argument checking
  if (is.duration(k))
    stop("The lag 'k' is a duration object instead of an integer")
  
  # Nothing to do
  if (k == 0L)
    return(x)
  
  # Trivial case
  len <- length(x)
  if (len <= abs(k))
    return(uts())
  
  # Shift observation values
  if (k > 0)
    x$values <- c(rep(NA, k), x$values[1L:(len-k)])
  else
    x$values <- c(x$values[(1L-k):len], rep(NA, abs(k)))
  
  # Drop observation times without matching lagged value
  if (k > 0) {
    x$values <- x$values[-(1L:k)]
    x$times <- x$times[-(1L:k)]
  }
  if (k < 0) {
    drop <- (len - abs(k) + 1L):len
    x$values <- x$values[-drop]
    x$times <- x$times[-drop]
  }
  x
}


#' Lag Observation Times
#'
#' Compute a lagged version of a time series by shifting observation \emph{times}. Each observation time is shifted by the same temporal amount, thereby causing a shift in the time series sample path.
#'
#' @note For an evenly spaced time series (1) shifting observation \emph{times}, and (2) shifting observation \emph{values} essentinally gives the same result. For unevenly spaced time series, however, these two operations are quite different. The former affects only the observation \emph{times}, while the latter affects only the observation \emph{values} (apart from observations that are dropped).
#'
#' @param x a time series object.
#' @param lag_t a \code{\link[lubridate]{duration}} object, specifying how much to shift the observation times of \code{x} forward.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{lag.uts}} allows to shift observation \emph{values}, as opposed to observation \emph{times}.
lag_t <- function(x, ...) UseMethod("lag_t")


#' @describeIn lag_t shift observation times for a \code{"uts"} object.
#' 
#' @examples
#' lag_t(ex_uts(), ddays(3))
#' lag_t(ex_uts(), dhours(7.5))
#  \dontrun{lag_t(ex_uts(), 3)}   # error, because 'lag_t' is not a duration object
lag_t.uts <- function(x, lag_t, ...)
{
  # Argument checking
  if (!is.duration(lag_t))
    stop("The lag_t is not a 'duration' object")
  
  # Shift observation times
  x$times <- x$times + lag_t
  x
}

