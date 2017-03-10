####################################
# First and last observation value #
####################################

#' First Observation Value
#'
#' Return the first value of an \R object.
#' 
#' @param x an \R object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{last}}
#' @seealso \code{\link{start}} and \code{\link{end}} return, respectively, the first and last obvservation \emph{time} from a time series.
first <- function(x, ...) UseMethod("first")


#' @describeIn first Assume that \code{x} is a vector or compatible object and return the first value.
#' 
#' @examples
#' first(5:10)
#' first(list(a="test", b=3))
first.default <- function(x, ...)
{
  if (length(x) > 0)
    x[[1]]
  else
    NULL
}


#' @describeIn first Return the first observation \emph{value} of a \code{"uts"} object. Unlike method \code{first()} in package \href{https://cran.r-project.org/web/packages/xts/}{xts}, arguments \code{n} and \code{keep} are not supported. Instead, this functionality is provided by \code{head()} and \code{heat_t()}.
#' 
#' @examples
#' first(ex_uts())
first.uts <- function(x, ...)
{
  if (length(x) > 0)
    x$values[[1]]
  else
    NULL
}


#' Last Observation Value
#'
#' Return the last value of an \R object.
#' 
#' @param x an \R object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{first}}
#' @seealso \code{\link{start}} and \code{\link{end}} return, respectively, the first and last obvservation \emph{time} from a time series.
last <- function(x, ...) UseMethod("last")


#' @describeIn last Assume that \code{x} is a vector or compatible object and return the last value.
#' 
#' @examples
#' last(5:10)
#' last(list(a="test", b=3))
last.default <- function(x, ...)
{
  if (length(x) > 0)
    x[[length(x)]]
  else
    NULL
}


#' @describeIn last Return the last observation \emph{value} of a \code{"uts"} object. Unlike method \code{last()} in package \href{https://cran.r-project.org/web/packages/xts/}{xts}, arguments \code{n} and \code{keep} are not supported. Instead, this functionality is provided by \code{tail()} and \code{tail_t()}.
#' 
#' @examples
#' first(ex_uts())
last.uts <- function(x, ...)
{
  if (length(x) > 0)
    x$values[[length(x$values)]]
  else
    NULL
}
