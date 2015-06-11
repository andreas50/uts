#' S3 Group Generic Methods for uts
#'
#' S3 group generic methods apply the methods in base \R{} to the values of \code{\link{uts}} objects.
#' 
#' @return An object of class \code{"uts"} with same observation times as the input \code{x}, and observation values the result of applying the provided method to the observation values of input \code{x}.
#' @param x an object of class \code{"uts"}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{groupGeneric}}
NULL


#' @rdname group_generic_uts test
#' 
#' @examples
#' # Summary 
#' min(ex_uts())
#' any(ex_uts())
#' range(ex_uts())
Summary.uts <- function(x, ...)
{
  do.call(.Generic, list(x$values, ...))
}


#' @rdname group_generic_uts Group \code{"Math"}
#' 
#' @examples
#' floor(ex_uts())
#' ceiling(ex_uts())
#' sqrt(ex_uts())
#' cumsum(ex_uts())
Math.uts <- function(x, ...)
{
  x$values <- do.call(.Generic, list(x$values))
  x
}
if (0) {
  test <- uts1
  test$values[1] <- NA
  cumsum(test)
  cumsum(test, na.rm=T)   # does not work since group generic only supports one argument
  cumsum(test[!is.na(test)])  # work-around 
  methods("Math")
}
