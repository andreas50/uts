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


#' Is Object a uts?
#' 
#' Return \code{TRUE} if and only if the argument is a \code{"uts"} object.
#'  
#' @param x an \R object.
#' 
#' @keywords internal
#' @examples
#' is.uts(ex_uts())
#' is.uts(5)
is.uts <- function(x)
{
  inherits(x, "uts")
}
if (0) {
  is.uts(VIX)
} 


#' Number of Observations
#' 
#' Return the number of time series observations.
#' 
#' @note
#' This methods only exists because the default implementation of \code{\link{length}} from base \R returns the number of fields in the internal representation of a \code{"uts"} object, instead of the number of observations.
#' 
#' @param x a \code{"uts"} object.
#' 
#' @keywords internal
#' @examples 
#' length(ex_uts())
length.uts <- function(x)
{
  length(x$times)
}


#' Summary of Time Series Values
#' 
#' This method calls \code{\link{summary}} from base \R with observation values of a time series.
#' 
#' @note
#' This method only exists because \code{\link{summary.default}} produces an error message.
#'  
#' @param x a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords internal
#' @examples
#' summary(ex_uts())
summary.uts <- function(object, ...)
{
  summary(object$values)
}


#' Internal Structure of uts
#' 
#' Compactly display the internal structure of a \code{"uts"} object.
#' 
#' @note
#' This methods only exists because the default implementation of \code{\link{str}} from base \R produces an error message.
#'  
#' @param object a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords internal
#' @examples
#' str(ex_uts())
#' str(ex_uts2())
str.uts <- function(object, ...)
{
  str(unclass(object), ...)
}


#' Print Values
#' 
#' @param x a time series object.
#' @param style the printing style. Either \code{"horizontal"} (the default), "vertical" or "plain" (which first prints the data and then the index).
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link[base:print]{print}}
#' @examples
#' print(ex_uts())
#' print(ex_uts(), style="vertical")
#' print(ex_uts(), style="plain")
#' uts()
print.uts <- function (x, style="horizontal", ...) 
{
  # Trivial case of no observations
  if (length(x) == 0) {
    cat("No observations available at this time.\n")
    return(invisible(x))
  }
    
  style <- match.arg(style, c("horizontal", "vertical", "plain"))
  if (style == "vertical") {
    out <- as.matrix(x$values)
    rownames(out) <- as.character(x$times)
    colnames(out) <- "values"
    print(out, ...)
  } else if (style == "horizontal") {
    out <- x$values
    names(out) <- x$times
    print(out, ...)
  } else
    print.default(x, ...)
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


#' Generic which function
#'
#' The function is needed, because \code{\link[base:which]{which}} of base \R is not generic.
#' 
#' @note
#' As recommended in Section 7.1 ("Adding new generics") of "Writing R Extensions", the implementation of \code{\link{which.default}} has been made a wrapper around \code{\link[base:which]{base::which}}.
#' 
#' @param x an \R object.
#' @param \dots further arguments passed to or from methods.
which <- function(x, ...) UseMethod("which")


#' @describeIn which simply calls the default implementation of base \R
which.default <- function(x, ...) base::which(x, ...)


#' Which observation values are TRUE?
#' 
#' For a logical \code{"uts"} (i.e. a \code{"uts"} with logical observation values), get the observation times with \code{TRUE} observation value.
#' 
#' @param x a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{which}}
#' @examples
#' which(ex_uts() > 48)
which.uts <- function(x, ...)
{
  # Argument checking
  if (!is.logical(x$values))
    stop("The observation values or not logical")

  if (length(x) > 0)
    x$times[which(x$values)]
  else
    as.POSIXct(character(0))
}

