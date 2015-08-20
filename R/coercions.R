######################################################################
# Convert objects back and forth between other R time series classes #
######################################################################

#' Coercion to uts
#' 
#' Convert other time series objects to \code{"uts"} objects.
#'
#' @return An object of class \code{"uts"}.
#' @param x a time series object of appropriate type.
#' @param \dots further arguments passed to or from methods.
as.uts <- function(x, ...) UseMethod("as.uts")


#' @describeIn as.uts convert a \code{\link[stats:ts]{ts}} object
#' 
#' @examples
#' # Convert a quarterly "ts" time series
#' ts1 <- ts(1:10, frequency = 4, start = c(1959, 2))
#' as.uts(ts1)
#' 
#' # Convert a monthly "ts" time series
#' ts2 <- ts(1:10, frequency = 12, start = c(1959, 8))
#' as.uts(ts2)
#' 
#' # Convert a yearly 'ts" time series
#' ts3 <- ts(1:10, frequency = 1, start = 1959)
#' as.uts(ts3)
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


#' @describeIn as.uts convert a \code{\link[xts:xts]{xts}} object
#' 
#' @examples
#' #
#' # Convert an "xts" time series
#' if (requireNamespace("xts", quietly = TRUE)) {
#'   xts1 <- xts::xts(1:4, as.Date("2015-01-01") + c(1, 3, 7, 9))
#'   as.uts(xts1)
#' }
as.uts.xts <- function(x, ...)
{
  as.uts(zoo::as.zoo(x, ...))
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


#' Coercion to zoo
#' 
#' @return A \code{\link[zoo:zoo]{zoo}} object.
#' @param x a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' if (requireNamespace("zoo", quietly = TRUE)) {
#'   zoo::as.zoo(ex_uts())
#' }
as.zoo.uts <- function(x)
{
  if (!requireNamespace("zoo", quietly=TRUE))
    stop("Package 'zoo' needed for this function to work")
  zoo::zoo(x$values, x$times)
}


#' Coercion to xts
#' 
#' @return An \code{\link[xts:xts]{xts}} object.
#' @param x a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' if (requireNamespace("xts", quietly = TRUE)) {
#'   xts::as.xts(ex_uts())
#' }
as.xts.uts <- function(x)
{
  if (!requireNamespace("xts", quietly=TRUE))
    stop("Package 'xts' needed for this function to work")
  xts::xts(x$values, x$times)
}

