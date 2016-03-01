######################################################################
# Convert objects back and forth between other R time series classes #
######################################################################

#####################
# Coercion to "uts" #
#####################

#' Coercion to uts
#' 
#' Convert univariate time series objects from other R package to \code{"uts"} objects.
#'
#' @return An object of class \code{"uts"}.
#' @param x a time series object of appropriate type.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' # Convert a quarterly "ts"
#' ts1 <- ts(1:10, frequency = 4, start = c(1959, 2))
#' as.uts(ts1)
#' 
#' # Convert a monthly "ts"
#' ts2 <- ts(1:10, frequency = 12, start = c(1959, 8))
#' as.uts(ts2)
#' 
#' # Convert a yearly 'ts"
#' ts3 <- ts(1:10, frequency = 1, start = 1959)
#' as.uts(ts3)
#' 
#' # Convert an "fts"
#' if (requireNamespace("fts", quietly = TRUE)) {
#'   fts1 <- fts::fts(index=as.POSIXct("2016-01-01") + dhours(c(1, 4, 27)), data=c(5,4,7))
#'   as.uts(fts1)
#' }
#' 
#' # Convert an "irts"
#' if (requireNamespace("tseries", quietly = TRUE)) {
#'   irts1 <- tseries::irts(as.POSIXct("2015-01-01") + days(c(1, 3, 7, 9)), 1:4)
#'   as.uts(irts1)
#' }
#' 
#' # Convert an "its"
#' if (requireNamespace("its", quietly = TRUE)) {
#'   mat <- matrix(1:2, nrow=2)
#'   rownames(mat) <- c("2003-01-01","2003-01-04")
#'   its1 <- its::its(mat)
#'   as.uts(its1)
#' }
#' 
#' # Convert an "xts"
#' if (requireNamespace("xts", quietly = TRUE)) {
#'   xts1 <- xts::xts(1:4, as.Date("2015-01-01") + c(1, 3, 7, 9))
#'   as.uts(xts1)
#' }
#' 
#' # Convert a "zoo"
#' if (requireNamespace("zoo", quietly = TRUE)) {
#'   zoo1 <- zoo::zoo(1:4, as.Date("2015-01-01") + c(1, 3, 7, 9))
#'   as.uts(zoo1)
#' }
#' 
#' @seealso \code{as.uts_vector} (in package \code{utsMultivariate}) for converting multivariate time series. 
as.uts <- function(x, ...) UseMethod("as.uts")


#' @describeIn as.uts convert a \code{\link[stats:ts]{ts}} object
as.uts.ts <- function(x, ...)
{
  # Require univariate time series
  if (!is.null(ncol(x)) && (ncol(x) > 1))
    stop("Only univariate 'ts' objects can be converted to a 'uts' object. Use as.uts_vector() in package utsMultivariate for multivariate time series")  
  
  # Extract values and times
  times <- date_decimal(as.numeric(time(x)), tz="")
  values <- as.numeric(x)
  
  # Round times for monthly and quarterly frequency
  freq <- tsp(x)[3]
  if (freq %in% c(4, 12))
    times <- floor_date(times + days(5), unit="month")
  uts(values, times)
}


#' @describeIn as.uts convert an \code{\link[fts:fts]{fts}} object
as.uts.fts <- function(x, ...)
{
  # Require univariate time series
  if (!is.null(ncol(x)) && (ncol(x) > 1))
    stop("Only univariate 'fts' objects can be converted to a 'uts' object. Use as.uts_vector() in package utsMultivariate for multivariate time series")
  
  # The "fts" class inherits from "zoo"
  as.uts.zoo(x, ...)
}


#' @describeIn as.uts convert an \code{\link[tseries:irts]{irts}} object
as.uts.irts <- function(x, ...)
{
  # Require univariate time series
  if (!is.null(ncol(x)) && (ncol(x) > 1))
    stop("Only univariate 'irts' objects can be converted to a 'uts' object. Use as.uts_vector() in package utsMultivariate for multivariate time series")
  
  # Clean messed up class attributed of observations times
  times <- x$time
  class(times) <- class(as.POSIXct(character()))
  
  uts(x$value, times)
}


#' @describeIn as.uts convert an \code{\link[its:its]{its}} object
as.uts.its <- function(x, ...)
{
  # Require univariate time series
  if (!is.null(ncol(x)) && (ncol(x) > 1))
    stop("Only univariate 'its' objects can be converted to a 'uts' object. Use as.uts_vector() in package utsMultivariate for multivariate time series")
  
  uts(as.vector(x@.Data), x@dates)
}


#' @describeIn as.uts convert an \code{\link[xts:xts]{xts}} object
as.uts.xts <- function(x, ...)
{
  # Require univariate time series
  if (!is.null(ncol(x)) && (ncol(x) > 1))
    stop("Only univariate 'xts' objects can be converted to a 'uts' object. Use as.uts_vector() in package utsMultivariate for multivariate time series")  
  
  as.uts(zoo::as.zoo(x, ...))
}


#' @describeIn as.uts convert a \code{\link[zoo:zoo]{zoo}} object
as.uts.zoo <- function(x, ...)
{
  # Require univariate time series
  if (!is.null(ncol(x)) && (ncol(x) > 1))
    stop("Only univariate 'zoo' objects can be converted to a 'uts' object. Use as.uts_vector() in package utsMultivariate for multivariate time series")
  
  uts(as.numeric(x), as.POSIXct(as.character(attr(x, "index"))))
}


#######################
# Coercion from "uts" #
#######################

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
as.zoo.uts <- function(x, ...)
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
as.xts.uts <- function(x, ...)
{
  if (!requireNamespace("xts", quietly=TRUE))
    stop("Package 'xts' needed for this function to work")
  xts::xts(x$values, x$times)
}


#' Coercion to fts
#' 
#' @return An \code{\link[fts:fts]{fts}} object.
#' @param x a \code{"uts"} object.
#' 
#' @examples
#' if (requireNamespace("fts", quietly = TRUE)) {
#'   fts::as.fts(ex_uts())
#' }
as.fts.uts <- function(x)
{
  if (!requireNamespace("fts", quietly=TRUE))
    stop("Package 'fts' needed for this function to work")
  fts::fts(x$times, x$values)
}


#' Coercion to irts
#' 
#' @return An \code{\link[tseries:irts]{irts}} object.
#' @param x a \code{"uts"} object.
#' 
#' @examples
#' if (requireNamespace("tseries", quietly = TRUE)) {
#'   tseries::as.irts(ex_uts())
#' }
as.irts.uts <- function(x)
{
  if (!requireNamespace("tseries", quietly=TRUE))
    stop("Package 'tseries' needed for this function to work")
  tseries::irts(x$times, x$values)
}


#' Coercion to its
#' 
#' @return An \code{\link[its:its]{its}} object.
#' @param x a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' if (requireNamespace("its", quietly = TRUE)) {
#'   its::as.its(ex_uts())
#' }
as.its.uts <- function(x, ...)
{
  if (!requireNamespace("its", quietly=TRUE))
    stop("Package 'its' needed for this function to work")
  
  data <- matrix(x$values, ncol=1)
  its::its(data, dates=x$times)
}

