#' Summary Group Methods for uts
#' 
#' These methods apply the \code{\link{Summary}} methods in base \R{} to the observation values of \code{\link{uts}} objects.
#' 
#' @param x an object of class \code{"uts"}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{groupGeneric}}
#' 
#' @examples
#' # Get the smallest observation value, ignoring NAs
#' min(ex_uts(), na.rm=TRUE)
#' 
#' # Check if any observation value is larger than 48
#' #any(ex_uts() > 48)
Summary.uts <- function(x, ...)
{
  do.call(.Generic, list(x$values, ...))
}


#' Math Group Methods for uts
#' 
#' These methods apply the \code{\link{Math}} methods in base \R{} to the observation values of \code{\link{uts}} objects.
#' 
#' @param x an object of class \code{"uts"}.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{groupGeneric}}
#' 
#' @examples
#' # Take the base-2 logarithm of the observation values and return the corresponding "uts"
#' log(ex_uts(), base=2)
#' 
#' # Calculate the cumulative product of the observation values and return the corresponding "uts"
#' cumsum(ex_uts())
Math.uts <- function(x, ...)
{
  x$values <- do.call(.Generic, list(x$values, ...))
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
