######################################################################
# Convert objects back and forth between other R time series classes #
######################################################################

# -----------------
# Generic functions
# -----------------

#' Coercion to uts
#' 
#' Convert other time series objects to \code{"uts"} objects.
#'
#' @return An object of class \code{"uts"}.
#' @param x a time series object of appropriate type.
#' @param \dots arguments passed to or from methods.
as.uts <- function(x, ...) UseMethod("as.uts")


# ----------------------
# Method implementations
# ----------------------

#' @describeIn as.uts convert a \code{\link[stats:ts]{ts}} object
#' 
#' @examples
#' # Convert a "ts" time series
#' ts1 <- ts(1:10, frequency = 4, start = c(1959, 2))
#' as.uts(ts1)
as.uts.ts <- function(x, ...)
{
  # Extract values and times
  times <- date_decimal(as.numeric(time(x)), tz="")
  values <- as.numeric(x)
  
  # Round times for monthly and quarterly frequency
  freq <- tsp(x)[3]
  if (freq %in% c(4, 12))
    times <- floor_date(times + days(5), unit="month")
  uts(values, times)
}


#' @describeIn as.uts convert a \code{\link[zoo:zoo]{zoo}} object
#' 
#' @examples
#' #
#' # Convert a "zoo" time series
#' if (requireNamespace("zoo", quietly = TRUE)) {
#'   zoo1 <- zoo::zoo(1:4, as.Date("2015-01-01") + c(1, 3, 7, 9))
#'   as.uts(zoo1)
#' }
as.uts.zoo <- function(x, ...)
{
  uts(as.numeric(x), as.POSIXct(attr(x, "index")))
}


