####################################
# First and last observation value #
####################################

#' First Value
#'
#' Return the first value of an \R object.
#' 
#' @param x an \R object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{last()}
#' @seealso \code{\link{start}} and \code{\link{end}} return, respectively, the first and last obvservation \emph{time} from a time series.
first <- function(x, ...) UseMethod("first")


#' @describeIn first Return the first observation \emph{value} of a \code{"uts"} object. Unlike the method \code{first()} in package \href{https://cran.r-project.org/web/packages/xts/}{xts}, this implementation does not support arguments \code{n} and \code{keep}, because this functionality is already provided by \code{head()} and \code{heat_t()}.
#' 
#' @examples
#' first(ex_uts())
first.uts <- function(x, ...)
{
  x$values[1]
}



#' Last Value
#'
#' Return the last value of an \R object.
#' 
#' @param x an \R object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{first()}
#' @seealso \code{\link{start}} and \code{\link{end}} return, respectively, the first and last obvservation \emph{time} from a time series.
last <- function(x, ...) UseMethod("last")


#' @describeIn last Return the last observation \emph{value} of a \code{"uts"} object. Unlike the method \code{last()} in package \href{https://cran.r-project.org/web/packages/xts/}{xts}, this implementation does not support arguments \code{n} and \code{keep}, because this functionality is already provided by \code{tail()} and \code{tail_t()}.
#' 
#' @examples
#' first(ex_uts())
last.uts <- function(x, ...)
{
  x$values[length(x$values)]
}
