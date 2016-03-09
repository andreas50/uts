#' Lagged Differences
#'
#' Return a time series with suitably lagged differences of the observation values. Observation times without corresponding lagged observation time (for example, the second observation for \code{lag=3}) are dropped from the output.
#' 
#' @param x a \code{"uts"} object.
#' @param lag an integer indicating which lag to use.
#' @param scale on which scale to calculate differences. Either \code{"abs"} for absolute differences \code{x[n] - x[n - lag]}, \code{"rel"} for relative differences \code{x[n] / x[n - lag] - 1}, or \code{"log"} for logarithmic differences \code{log(x[n] / x[n - lag])}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @note For an evenly spaced time series, calculating differences over (1) a certain number of observations (e.g. over four observations for quarterly data), and (2) over a fixed time horizon (e.g. over one year) gives the same result. For unevenly spaced time series, however, these two operations are quite different.
#' 
#' @seealso \code{\link{diff_t}} allows to calculate differences over a fixed \emph{time horizon}, as opposed to \emph{number of observations}.
#' @seealso \code{\link{diff}} in base \R.
#' 
#' @examples
#' diff(ex_uts())
#' diff(ex_uts(), lag=-3)
#' diff(ex_uts(), scale="log")
#' diff(ex_uts(), lag=10)     # an empty time series, because the lag is too large
diff.uts <- function(x, lag=1, scale="abs", ...)
{
  # Argument checking
  if (is.duration(lag))
    stop("'lag' is a duration object instead of an integer")
  
  # Trivial case
  len <- length(x)
  if (len <= abs(lag))
    return(uts())
  
  # Calculate lagged difference on desired scale
  if (scale == "abs")
    out <- x - lag(x, k=lag)
  else if (scale == "rel")
    out <- x / lag(x, k=lag) - 1
  else if (scale == "log")
    out <- log(x / lag(x, k=lag))
  else
    stop("Unknown scale")
  
  # Drop observation times without matching lagged value
  if (lag < 0) {
    len <- length(out)
    drop <- (len - abs(lag) + 1L):len
    out$values <- out$values[-drop]
    out$times <- out$times[-drop]
  }
  out
}


#' Rolling Differences
#' 
#' Return a time series with differences over a fixed time horizon.
#' 
#' The time series difference at observation time \code{t} for time horizon \code{by} is calculated between the values \code{x_t} and \code{x[t - by]} on a suitable scale. Here, \code{x[t - by]} is the sampled value of \code{x} at time \code{t - by}, see \code{\link{sample_values}}.
#' 
#' Observations times for which no difference can be calculated (i.e. times \code{t_i} with \code{t_i - by < t_1}) are dropped from the output.
#' 
#' @param x a time series object.
#' @param by a finite \code{\link[lubridate]{duration}} object, specifying over which time horizon to calculate differences in observation values.
#' @param scale on which scale to calculate differences. Either \code{"abs"} for absolute differences \code{x_t - x[t - by]}, \code{"rel"} for relative differences \code{x_t / x[t - by] - 1}, or \code{"log"} for logarithmic differences \code{log(x_t / x[t - by])}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @note For an evenly spaced time series, calculating differences over (1) a certain number of observations (e.g. over four observations for quarterly data), and (2) over a fixed time horizon (e.g. over one year) gives the same result. For unevenly spaced time series, however, these two operations are quite different.
diff_t <- function(x, ...) UseMethod("diff_t")
  

#' @describeIn diff_t rolling difference for a \code{"uts"} object.  
#' 
#' @seealso \code{\link{diff}} allows to calculate differences over a fixed \emph{number of observations}, as opposed to \emph{time horizon}.
#' @examples
#' diff_t(ex_uts(), by=ddays(1))
#' diff_t(ex_uts(), by=ddays(5)) # an empty time series, because the time difference is too large
diff_t.uts <- function(x, by=NULL, scale="abs", ...)
{
  # Argument checking
  if (!is.duration(by))
    stop("'by' is not a 'duration' object")
  if (is.na(by))
    stop("The return horizon 'by' is NA")
  if (!is.finite(by))
    stop("'by' is not finite")

  # Calculate lagged time series
  x_lag <- lag_t(x[x$times - by], by)
  
  # Calculate difference on desired scale
  if (scale == "abs")
    out <- x - x_lag
  else if (scale == "rel")
    out <- x / x_lag - 1
  else if (scale == "log")
    out <- log(x / x_lag)
  else
    stop("Unknown scale")
  
  # Drop observation times without matching lagged value
  window(out, start(x) + by)
}
