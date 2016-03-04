##############################################################
# Methods that calculate summaries of the observation values #
##############################################################

#' Summary of Time Series Values
#' 
#' Apply \code{\link{summary}} from base \R to the observation values of a time series.
#' 
#' @note
#' This method only exists because \code{\link{summary.default}} produces an error message.
#'  
#' @param x a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords internal
#' @examples
#' summary(ex_uts())
summary.uts <- function(object, ...)
{
  summary(object$values)
}


#' Generic sd function
#'
#' The function is needed, because \code{\link[stats:sd]{sd}} of base \R is not generic.
#' 
#' @note
#' As recommended in Section 7.1 ("Adding new generics") of "Writing R Extensions", the implementation of \code{\link{sd.default}} has been made a wrapper around \code{\link[stats:sd]{stats::sd}}.
#' 
#' @param x an \R object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords internal
sd <- function(x, ...) UseMethod("sd")

#' @describeIn sd simply calls the default implementation of base \R
#' @keywords internal
sd.default <- function(x, ...) stats::sd(x, ...)


#' Mean, Median, and Standard Deviation of Observation Values
#' 
#' Calculate the mean, median, and standard deviation, respectively, of the observation values.
#' 
#' @param x a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link[base:mean]{mean}}, \code{\link[stats:median]{median}}, and \code{\link[stats:sd]{sd}} in base \R.
#' @keywords internal
#' @examples
#' mean(ex_uts())
mean.uts <- function(x, ...)
{
  mean(x$values, ...)
}


#' @rdname mean.uts
#' 
#' @examples
#' median(ex_uts())
median.uts <- function(x, ...)
{
  median(x$values, ...)
}


#' @rdname mean.uts
#' 
#' @examples
#' sd(ex_uts())
sd.uts <- function(x, ...)
{
  sd(x$values, ...)
}


