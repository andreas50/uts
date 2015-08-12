#################################
# which() and related functions #
#################################

#-------#
# which #
#-------#

#' Generic which function
#'
#' The function is needed, because \code{\link[base:which]{which}} of base \R is not generic.
#' 
#' @note
#' As recommended in Section 7.1 ("Adding new generics") of "Writing R Extensions", the implementation of \code{\link{which.default}} has been made a wrapper around \code{\link[base:which]{base::which}}.
#' 
#' @param x an \R object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords internal
which <- function(x, ...) UseMethod("which")


#' @describeIn which simply calls the default implementation of base \R
#' @keywords internal
which.default <- function(x, ...) base::which(x, ...)


#' Which observation values are TRUE?
#' 
#' For a logical \code{"uts"} (i.e. a \code{"uts"} with logical observation values), get the observation times with \code{TRUE} observation value.
#' 
#' @param x a \code{"uts"} object with \code{\link{logical}} observation values.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso The \code{\link{which}} generic function.
#' @examples
#' which(ex_uts() > 48)
which.uts <- function(x, ...)
{
  # Argument checking
  if (!is.logical(x$values))
    stop("The observation values or not logical")
  
  if (length(x) > 0)
    x$times[which(x$values)]
  else
    as.POSIXct(character())
}


#-----------#
# which.max #
#-----------#

#' Generic which.max function
#'
#' The function is needed, because \code{\link[base:which.max]{which.max}} of base \R is not generic.
#' 
#' @note
#' As recommended in Section 7.1 ("Adding new generics") of "Writing R Extensions", the implementation of \code{\link{which.max.default}} has been made a wrapper around \code{\link[base:which.max]{base::which.max}}.
#' 
#' @param x an \R object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords internal
which.max <- function(x, ...) UseMethod("which.max")


#' @describeIn which.max simply calls the default implementation of base \R
#' @method which.max default
#' @keywords internal
which.max.default <- function(x, ...) base::which.max(x)


#' Which observation time has the largest/smallest observation value?
#' 
#' Determine the temporal location (i.e. observation time) of the first maximum or minimum observation value.
#' 
#' @param x a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link[base]{which.max}} and \code{\link[base]{which.min}} for numeric vectors in base \R.
#' @name which.max.uts
NULL


#' @rdname which.max.uts
#' @method which.max uts
#'
#' @examples
#' which.max(ex_uts())
which.max.uts <- function(x, ...)
{
  x$times[which.max(x$values)]
}


#-----------#
# which.min #
#-----------#

#' Generic which.min function
#'
#' The function is needed, because \code{\link[base:which.min]{which.min}} of base \R is not generic.
#' 
#' @note
#' As recommended in Section 7.1 ("Adding new generics") of "Writing R Extensions", the implementation of \code{\link{which.min.default}} has been made a wrapper around \code{\link[base:which.min]{base::which.min}}.
#' 
#' @param x an \R object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords internal
which.min <- function(x, ...) UseMethod("which.min")


#' @describeIn which.min simply calls the default implementation of base \R
#' @method which.min default
#' @keywords internal
which.min.default <- function(x, ...) base::which.min(x)



#' @rdname which.max.uts
#' @method which.min uts
#'
#' @examples
#' which.min(ex_uts())
which.min.uts <- function(x, ...)
{
  x$times[which.min(x$values)]
}
