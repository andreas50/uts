# ############################################
# UTS (Unevenly-spaced Time Series) S3 class #
# ############################################

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
#' @param times a vector of strictly increasing observation times. Must be a \code{\link{POSIXct}} object or be coercible using \code{\link{as.POSIXct}}.
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
#'
uts <- function(values=c(), times=as.POSIXct(character(0)))
{
  # Argument checking
  if (length(values) != length(times))
    stop("The number of observation values and observation times does not match")
  if (!is.POSIXct("POSIXct"))
    times <- as.POSIXct(times)
  if (any(diff(times) <= 0))
    stop("The observation times need to be a strictly increasing")
  
  # Create "uts" object
  x <- list(values=values, times=times)
  class(x) <- c("uts", "list")
  x   
}
