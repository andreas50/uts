#############################################################################
# Methods for ensuring compatability with base R methods for the "ts" calss #
#############################################################################

#' Compatability with ts class
#' 
#' These methods exist solely to ensure that methods intended for \code{"ts"} objects in base \R are not accidentally applied to \code{"uts"} objects.
#'
#' @param x a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords internal
#' @seealso \code{\link{ts}}
#' @name compatability
NULL


#' @rdname compatability
#' 
#' @return \code{cycle()} and \code{frequency()} give an error message, because \code{"uts"} objects, by definition, do not have a fixed number of observations in a given time interval.
#' 
#' @examples
#' \dontrun{frequency(ex_uts())}
frequency.uts <- function(x, ...)
{
  stop("Unevenly-spaced time series ('uts') objects do not have a frequency attribute")
}


#' @rdname compatability
#'
#' @examples
#' \dontrun{cycle(ex_uts())}
cycle.uts <- function(x, ...)
{
  stop("Unevenly-spaced time series ('uts') objects do not have observation cycles")
}


