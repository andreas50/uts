#' Summary Group Methods for uts
#' 
#' Apply the \code{\link{Summary}} methods in base \R{} to the observation values of \code{"uts"} objects.
#' 
#' @param x a \code{"uts"} object.
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
#' Apply the \code{\link{Math}} methods in base \R{} to the observation values of \code{"uts"} objects.
#' 
#' @param x a \code{"uts"} object.
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


# #' Ops Group Methods for uts
# #' 
# #' Apply the \code{\link{Ops}} methods in base \R{} to the observation values of \code{"uts"} objects.
# #' 
# #' @param x, x2 \code{"uts"} objects.
# #' @param \dots further arguments passed to or from methods.
# #' @param tolerance tolerance for numerical noise in observation times. See \code{\link{sorted_union}}.
# #' 
# #' @seealso \code{\link{groupGeneric}}
# #' 
# #' @examples
# #' # Unary oparators
# #' -ex_uts()
# #' !ex_uts()
# #' 
# #' # Binary operators
# #' ex_uts() * 2
# #' ex_uts() + ex_uts()
# #' ex_uts() > 48
# Ops.uts <- function(x, x2, tolerance=.Machine$double.eps ^ 0.5)
# {
#   # Unary operator
#   if (missing(x2)) {
#     x$values <- do.call(.Generic, list(x$values))
#     return(x)
#   }
#   
#   # Binary operator
#   if (is.uts(x) & is.uts(x2)) {
#     # Determine time points of output time series
#     if (min(length(x), length(x2)) == 0)
#       return(uts())
#     if (x$times[1] <= x2$times[1]) {
#       used_t <- x$times[x$times >= x2$times[1]]
#       all_times <- sorted_union(used_t, x2$times, eps=1e-9)
#     } else {
#       used_t <- x2$times[x2$times >= x$times[1]]
#       all_times <- sorted_union(x$times, used_t, eps=1e-9)
#     }
#     attributes(all_times) <- attributes(x$times)
#     
#     # Sample values
#     values1 <- sample_values(x, all_times)
#     values2 <- sample_values(x2, all_times)
#     
#     # Generate output
#     out <- uts(do.call(.Generic, list(values1, values2)), all_times)
#   }  else if (is(x2, "numeric") | is(x2, "integer")) {
#     out <- x
#     out$values <- do.call(.Generic, list(x$values,  x2))
#   } else {
#     out <- x2
#     out$values <- do.call(.Generic, list(x,  x2$values))
#   }
#   out
# }

