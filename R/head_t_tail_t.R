##############################################
# Head and Tail (in terms of temporal length #
##############################################

#' Initial Subperiod
#' 
#' Return a subperiod time series with the initial part, where the length is specified in terms of the \emph{temporal length}.
#' 
#' @note For an evenly-spaced time series, the methods \code{head()} and \code{head_t()} essentially work the same. For unevenly-spaced time series, however, they are quite different. The subperiod window of the former is determined by the observation \emph{values}, while for the latter it is determined by the observation \emph{times}.
#' 
#' @param x a time series object.
#' @param width a \code{\link[lubridate]{duration}} object, specifying the temporal width of the initial subperiod time series.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{head}}, \code{\link{tail}}, \code{\link{tail_t}}, \code{\link{window}} for other methods that extract a subperiod time series.
head_t <- function(x, ...) UseMethod("head_t")


#' @describeIn head_t initial subperiod time seried for \code{"uts"} object.
#' 
#' @examples
#' head_t(ex_uts(), ddays(1))
#' head_t(ex_uts(), ddays(0))  # leaves only the first observation
head_t.uts <- function(x, width, ...)
{
  if (!is.duration(width))
    stop("'width' is not a duration object")
  window(x, end=start(x) + width)
}



#' Terminal Subperiod
#' 
#' Return a subperiod time series with the terminal part, where the length is specified in terms of the \emph{temporal length}.
#' 
#' @note For an evenly-spaced time series, the methods \code{tail()} and \code{tail_t()} essentially work the same. For unevenly-spaced time series, however, they are quite different. The subperiod window of the former is determined by the observation \emph{values}, while for the latter it is determined by the observation \emph{times}.
#' 
#' @param x a time series object.
#' @param width a \code{\link[lubridate]{duration}} object, specifying the temporal width of the terminal subperiod time series.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{head}}, \code{\link{head_t}}, \code{\link{tail}}, \code{\link{window}} for other methods that extract a subperiod time series.
tail_t <- function(x, ...) UseMethod("tail_t")


#' @describeIn tail_t terminal subperiod time seried for \code{"uts"} object.
#' 
#' @examples
#' tail_t(ex_uts(), ddays(1))
#' tail_t(ex_uts(), ddays(0))  # leaves only the last observation
tail_t.uts <- function(x, width, ...)
{
  if (!is.duration(width))
    stop("'width' is not a duration object")
  window(x, start=end(x) - width)
}