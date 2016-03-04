###################################
# Constructor and basic functions #
###################################

#' Unevenly-spaced Time Series
#' 
#' Create an unevenly spaced time series (\code{"uts"}) object from a vector of observation values and a vector of observation times of matching length.
#' 
#' As shown in the example below, it is possible to store arbitrary \R objects in a \code{"uts"} object. However, some time series operations (e.g. arithmetic operations) require the observation values to be either \code{\link{numeric}}, \code{\link{logical}}, or \code{\link{complex}}.
#' 
#' @note An abstract class \code{"uts_virtual"} exists from which \code{"uts"}, \code{"uts_vector"}, and \code{"uts_matrix"}, inherit (see package \code{utsMultivariate} for the latter three classes): it is used to allow operations such as subtraction to mix the classes.
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
#' # Create a "uts" with non-numeric observations
#' uts(list(1:5, c("a", "B")), c("2007-11-08 1:01:00", "2007-11-09 15:16:00"))
#'
#' # Create an empty "uts"
#' uts()
#' 
#' # All of the following are TRUE
#' is.uts(ex_uts())
#' is.uts_virtual(ex_uts())
uts <- function(values=numeric(), times=as.POSIXct(character()))
{
  # Argument checking
  if (length(values) != length(times))
    stop("The number of observation values and observation times does not match")
  if (!is.POSIXct("POSIXct"))
    times <- as.POSIXct(times)
  if (anyNA(times))
    stop("Observation times cannot be NA")
  if (is.unsorted(times, strictly=TRUE))
    stop("The observation times need to be a strictly increasing")
  
  # Creat "uts" object
  x <- list(values=values, times=times)
  class(x) <- c("uts", "uts_virtual")
  x   
}


#' @rdname uts
#' 
#' @description \code{is.uts} returns \code{TRUE} if its argument is a \code{"uts"} object.
#' 
#' @keywords internal
#' @param x an \R object.
is.uts <- function(x)
{
  inherits(x, "uts")
}


#' @rdname uts
#' 
#' @keywords internal
#' @description \code{is.uts_virtual} returns \code{TRUE} if its argument is a \code{"uts_virtual"} object.
is.uts_virtual <- function(x)
{
  inherits(x, "uts_virtual")
}


#' Internal Structure of uts
#' 
#' Compactly display the internal structure of a \code{"uts"} object.
#' 
#' @note
#' This methods only exists because the default implementation of \code{\link{str}} in base \R produces an error message.
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


#' Print Observation Times and Values
#' 
#' Print the observation time and values in one of two different formats.
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
  # Special case of zero length
  if (length(x) == 0L) {
    cat("uts(0)\n")
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
  invisible(x)
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
#' merge(ex_uts2(), ex_uts())
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


#' Time Window
#' 
#' Extract a subperiod time series between times \code{start} and \code{end}.
#' 
#' @param x a \code{"uts"} object.
#' @param start a \code{\link{POSIXct}} object or coercible using \code{\link{as.POSIXct}}. The start time of the period of interest.
#' @param end a \code{\link{POSIXct}} object or coercible using \code{\link{as.POSIXct}}. The end time of the period of interest.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{head}}, \code{\link{head_t}}, \code{\link{tail}}, \code{\link{tail_t}} for other methods that extract a subperiod time series.
#' @examples
#' window(ex_uts(), start=as.POSIXct("2007-11-09"))
#' window(ex_uts(), start=as.POSIXct("2007-11-09", tz="America/New_York"))
#' window(ex_uts(), start=as.POSIXct("2007-11-09", tz="Australia/Sydney"))
#' window(ex_uts(), start=as.POSIXct("2007-11-08"), end=as.POSIXct("2007-11-09"))
window.uts <- function(x, start=NULL, end=NULL, ...)
{
  if (is.null(start))
    start <- start(x, 3)
  if (is.null(end))
    end <- end(x, 2)
  
  # Argument checking
  if (length(x) == 0L)
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
#' @note This method is helpful for saving a time series to a human-readable text file.
#' 
#' @param x a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' as.data.frame(ex_uts())
#' 
#' # Save a time series to a text file with nice date formatting
#' uts1 <- uts(rnorm(10), Sys.time() + days(1:10))
#' data <- as.data.frame(uts1)
#' data$time <- format(data$time, format="%Y-%m-%d")
#' \dontrun{write.csv(data, file="random.csv", row.names=FALSE, quote=FALSE)}
as.data.frame.uts <- function(x, ...)
{
  # Argument checking
  if (!is.atomic(x$values))
    stop("Only time series with atomic observation values can be coerced to a data.frame")
  
  # Flatten the data
  if (length(x) == 0L)
    values <- numeric()
  else
    values <- x$values
  data.frame(time=x$times, value=values, stringsAsFactors=FALSE)
}


#' Observation Times
#' 
#' Get the observation times.
#' 
#' @note The observation times of a \code{"uts"} object \code{x} can also be accessed using \code{x$times}. However, using \code{time(x)} relies on the internal object representation.
#' 
#' @return A \code{\link{POSIXct}} object.
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
