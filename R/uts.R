##############################################
# UTS (Unevenly-spaced Time Series) S3 class #
##############################################

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


#' Merge two or more uts
#' 
#' Merge two or more \code{"uts"} into a single time series. For observation times that show up in more than one time series, the observation value of the first \code{"uts"} in the argument list with such observation time is used.
#' 
#' @param x,y \code{"uts"} objects.
#' @param tolerance tolerance for numerical noise in observation times.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' merge(ex_uts(), ex_uts())
#' merge(ex_uts(), ex_uts2())
#' merge(uts(), ex_uts())
merge.uts <- function(x, y, tolerance=.Machine$double.eps ^ 0.5, ...)
{
  # Determine the union of observation times 
  utsv <- c(list(x, y), list(...))
  all_times <- x$times
  for (j in 2:length(utsv))
    all_times <- sorted_union(all_times, utsv[[j]]$times, tolerance=tolerance)
  attributes(all_times) <- attributes(x$times)
  
  # Merge observation values, with priority determined by the order of arguments
  values <- numeric(length(all_times))
  for (j in length(utsv):1) {
    uts <- utsv[[j]]
    pos <- num_leq_sorted(uts$times, all_times, tolerance=tolerance)
    values[pos] <- uts$values
  }
  
  # Return single merged "uts"
  uts(values, all_times)
}


#' First and Last Observation Time
#' 
#' Get the first and last observation time, respectively.
#' 
#' @return \code{start()} returns the first observation time.
#' @param x a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' start(ex_uts())
start.uts <- function(x, ...)
{
  if (length(x) > 0)
    x$times[1]
  else
    NA
}


#' @rdname start.uts
#' 
#' @return \code{end()} returns the last observation time.
#' 
#' @examples
#' end(ex_uts())
end.uts <- function(x, ...)
{
  if (length(x) > 0)
    x$times[length(x$times)]
  else
    NA
}


#' Time Window
#' 
#' Extract a subperiod time series between times \code{start} and \code{end}.
#' 
#' @param x a \code{"uts"} object.
#' @param start the start time of the period of interest.
#' @param end the end time of the period of interest.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' window(ex_uts(), start=as.POSIXct("2007-11-09"))
#' window(ex_uts(), start=as.POSIXct("2007-11-08"), end=as.POSIXct("2007-11-09"))
window.uts <- function(x, start=NULL, end=NULL, ...)
{
  if (is.null(start))
    start <- start(x, 3)
  if (is.null(end))
    end <- end(x, 2)
  
  # Argument checking
  if (length(x) == 0)
    return(x)
  if (!is.POSIXct(start))
    start <- as.POSIXct(start)
  if (!is.POSIXct(end))
    end <- as.POSIXct(end)
  
  # Determine observations in [start, end] window
  start_pos <- sum(x$times < start) + 1
  end_pos <- sum(x$times <= end)
  if (start_pos <= end_pos)
    used_pos <- start_pos:end_pos
  else
    used_pos <- c()
  
  # Drop observations outside window
  x$values <- x$values[used_pos]
  x$times <- x$times[used_pos]
  x
}


#' Coerce to a Data Frame
#'
#' Flatten a \code{"uts"} to a two-column \code{data.frame}, with the observation times in first column and observation values in the second column. The column names are \code{"time"} and \code{"value"}.
#' 
#' @note Only time series with atomic observation values can be coerced to a \code{data.frame}.
#' 
#' @param x a \code{"uts"} object.
#' @param \dots arguments passed to \code{\link{format.POSIXct}}.
#' 
#' @examples
#' as.data.frame(ex_uts())
#' as.data.frame(uts(c("cat", "dog"), Sys.time() + days(1:2)), format="%Y-%m-%d")
as.data.frame.uts <- function(x,  ...)
{
  # Argument checking
  if (!is.atomic(x$values))
    stop("Only time series with atomic observation values can be coerced to a data.frame")
  
  # Flatten the data
  if (length(x) == 0)
    values <- numeric(0)
  else
    values <- x$values
  data.frame(time=format(x$times, ...), value=values, stringsAsFactors=FALSE)
}


#' Observation Times
#' 
#' Get the observation times.
#' 
#' @note The observation times of a \code{"uts"} object \code{x} can also be accessed using \code{x$times}. However, using \code{time(x)} relies on the internal object representation.
#' 
#' @return A \code{\link{POSIXct}} object with the observation times of \code{x}.
#' @param x a \code{"uts"} object with \code{\link{logical}} observation values.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' time(ex_uts())
#' identical(time(ex_uts()), ex_uts()$times)     # TRUE
time.uts <- function(x, ...)
{
  x$times
}


#' Lagged Differences
#'
#' Return a time series with suitably lagged differences between the observation values.
#' 
#' @note Observations times without corresponding lagged value (for example, the second observation for lag=3) are dropped from the output.
#' 
#' @param x a \code{"uts"} object.
#' @param lag an integer indicating which lag to use.
#' @param scale on which scale to calculate lagged differences. Either \code{"abs"} for absolute differences \code{X[n] - X[n - lag]}, \code{"rel"} for relative differences \code{X[n] / X[n - lag] - 1}, or \code{"log"} for logarithmic differences \code{log(X[n] / X[n - lag])}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link[base:diff]{diff}} in base \R.
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

