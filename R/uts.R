##############################################
# UTS (Unevenly-spaced Time Series) S3 class #
##############################################

# -----------------
# Generic functions
# -----------------

# ----------------------
# Method implementations
# ----------------------

#' Unevenly-spaced Time Series
#' 
#' Create an unevenly-spaced time series (\code{"uts"}) object from a vector of observation values and a vector of observation times of matching length.
#'
#' @return An object of class \code{"uts"}.
#' @param values a vector of observation values.
#' @param times a vector of strictly increasing observation times. Must be a \code{\link{POSIXct}} object or be coercible using \code{\link{as.POSIXct}}. Observation times cannot be \code{NA}.
#' 
#' @keywords ts classes
#' @examples
#' # Create a numeric "uts"
#' dts <- c("2007-11-08", "2007-11-08", "2007-11-08", "2007-11-09", "2007-11-09", "2007-11-09")
#' tms <- c("7:00:00", "8:01:00", "13:15:00", "7:30:00", "8:51:00", "15:15:00")
#' uts(values=c(48.375, 48.5, 48.375, 47, 47.5, 47.35), times=paste(dts, tms))
#' 
#' # Create an "uts" with non-numeric observations
#' uts(list(1:5, c("a", "B")), c("2007-11-08 1:01:00", "2007-11-09 15:16:00"))
#'
#' # Create an empty "uts"
#' uts()
uts <- function(values=c(), times=as.POSIXct(character(0)))
{
  # Argument checking
  if (length(values) != length(times))
    stop("The number of observation values and observation times does not match")
  if (!is.POSIXct("POSIXct"))
    times <- as.POSIXct(times)
  if (anyNA(times))
    stop("Observation times cannot be NA")
  if (any(diff(times) <= 0))
    stop("The observation times need to be a strictly increasing")
  
  # Creat "uts" object
  x <- list(values=values, times=times)
  class(x) <- c("uts", "list")
  x   
}


#' Remove NAs
#' 
#' Returns the object with incomplete cases removed.
#' 
#' @param object a time series object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link[stats:na.fail]{na.fail}}, \code{\link[stats:na.fail]{na.omit}}
#' @examples
#' # Remove NAs from a "uts"
#' tmp <- ex_uts()
#' tmp$values[c(2, 4)] <- NA
#' na.omit(tmp)
na.omit.uts <- function(object, ...)
{
  keep <- !is.na(object$values)
  object$values <- object$values[keep]
  object$times <- object$times[keep]
  object
}


#' Lag a Time Series
#' 
#' Compute a lagged version of a time series by shifting individual observations values, while keeping the observation times unchanged.
#' 
#' The n-th observation of each original time series becomes the (n+k)-th observation of the lagged time series for 1 <= (n+k) <= length(x). Observations without corresponding un-lagged value (for example, the second observation for lag k=3) are set to \code{NA}.
#' 
#' @return A time series object with the same class, length, and observation times as \code{x}.
#' @param x a time series object.
#' @param k the number of lags (in units of observations).
#' @param \dots further arguments passed to or from methods.
#' 
#' @note For an evenly-spaced time series (1) shifting observation \emph{times}, and (2) shifting observation \emph{values} essentinally gives the same result (apart from the \code{NA}s that are introduced in the latter case). For unevenly-spaced time series, however, these two operations are quite different. The former only affects the vector observation times (but not the vector of observation values), while the latter only affects the vector observation values (but not the vector of observation times).
#' 
#' @seealso \code{\link[stats:lag]{lag}}
#' @examples
#' # Shift observations values forward by one observation
#' lag(ex_uts(), k=1)
#' 
#' # Shift observations values forward by two observations
#' lag(ex_uts(), k=-2)
#' 
#' # If the lag >= the length of the time series, all observation values are N
#' lag(ex_uts(), k=6)
lag.uts <- function(x, k=1, ...)
{
  # Nothing to do
  if (k == 0)
    return(x)
  
  # Special case of |k| >= length(x)
  k <- as.integer(k)
  len <- length(x$values)
  if (abs(k) >= len) { 
    x$values <- rep(NA, len)
    return(x)
  }
  
  # Shift observation values
  values <- x$values
  if (k < 0)
    values <- rev(values)
  values_new <- lag(values, k)
  values_new[1:abs(k)] <- NA
  if (k < 0)
    values_new <- rev(values_new)
  
  # Return lagged time series
  x$values <- values_new
  x
}

