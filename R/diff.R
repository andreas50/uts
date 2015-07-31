#' Lagged Differences
#'
#' Return a time series with suitably lagged differences between the observation values.
#' 
#' @note Observations times without corresponding lagged value (for example, the second observation for \code{lag=3}) are dropped from the output.
#' 
#' @param x a \code{"uts"} object.
#' @param lag an integer indicating which lag to use.
#' @param scale on which scale to calculate lagged differences. Either \code{"abs"} for absolute differences \code{X[n] - X[n - lag]}, \code{"rel"} for relative differences \code{X[n] / X[n - lag] - 1}, or \code{"log"} for logarithmic differences \code{log(X[n] / X[n - lag])}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{lag}}, \code{\link{lag_t}}, and \code{\link{diff_t}}
#' @examples
#' diff(ex_uts())
#' diff(ex_uts(), lag=-3)
#' diff(ex_uts(), scale="log")
#' diff(ex_uts(), lag=10)     # an empty time series, because the lag is too large
diff.uts <- function(x, lag=1, scale="log", ...)
{
  # Trivial case
  if (length(x) <= abs(lag))
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
  if (lag > 0) {
    out$values <- out$values[-(1:lag)]
    out$times <- out$times[-(1:lag)]
  }
  if (lag < 0) {
    len <- length(out)
    drop <- (len - abs(lag) + 1):len
    out$values <- out$values[-drop]
    out$times <- out$times[-drop]
  }
  out
}


#' Rolling Differences
#' 
#' @param x a time series object.
#' @param \dots further arguments passed to or from methods.
diff_t <- function(x, ...) UseMethod("diff_t")
  
  
#' Rolling Differences
#' 
#' @param x a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{lag}}, \code{\link{lag_t}}, and \code{\link{diff}}
#' @examples
#' diff_t(ex_uts())
diff_t.uts <- function(x, ...)
{
}
