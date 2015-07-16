#' Lag Obseration Values
#' 
#' Compute a lagged version of a time series by shifting individual observations values, while keeping the observation times unchanged.
#' 
#' The n-th observation of each original time series becomes the (n+k)-th observation of the lagged time series for 1 <= (n+k) <= length(x). Observations without corresponding un-lagged value (for example, the second observation for lag k=3) are set to \code{NA}.
#' 
#' @return A \code{"uts"} object with the observation times (and in particular, of the same length) as \code{x}.
#' @param x a \code{"uts"} object.
#' @param k the number of lags (in units of observations).
#' @param \dots further arguments passed to or from methods.
#' 
#' @note For an evenly-spaced time series (1) shifting observation \emph{times}, and (2) shifting observation \emph{values} essentinally gives the same result (apart from the \code{NA}s that are introduced in the latter case). For unevenly-spaced time series, however, these two operations are quite different. The former only affects the observation times (but not the observation values), while the latter only affects the observation values (but not the observation times).
#' 
#' @seealso \code{\link[stats:lag]{lag}} in base \R.
#' @seealso #' @seealso \code{\link{lag_t}} allows to shift observation \emph{times}, as opposed to observation \emph{values}.
#' @examples
#' # Shift observations values forward by one observation
#' lag(ex_uts(), k=1)
#' 
#' # Shift observations values forward by two observations
#' lag(ex_uts(), k=-2)
#' 
#' # If the lag >= the length of the time series, all observation values are N
#' lag(ex_uts(), k=6)
#' lag(ex_uts(), k=-6)
lag.uts <- function(x, k=1, ...)
{
  # Nothing to do
  if (k == 0)
    return(x)
  
  # Special case of |k| >= length(x)
  k <- as.integer(k)
  len <- length(x)
  if (abs(k) >= len) { 
    x$values <- rep(NA, len)
    return(x)
  }
  
  # Shift observation values
  if (k > 0)
    x$values <- c(rep(NA, k), x$values[1:(len-k)])
  else
    x$values <- c(x$values[(1-k):len], rep(NA, abs(k)))
  x
}


#' Lag Observation Times
#'
#' Lag observation times of a time series by a given amount. In other words, add a certain amount of time to each observation time.
#'
#' @note For an evenly-spaced time series (1) shifting observation \emph{times}, and (2) shifting observation \emph{values} essentinally gives the same result (apart from the \code{NA}s that are introduced in the latter case). For unevenly-spaced time series, however, these two operations are quite different. The former only affects the observation times (but not the observation values), while the latter only affects the observation values (but not the observation times).
#'
#' @param x a time series object of appropriate type.
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


