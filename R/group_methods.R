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
  methods("Math")
}


#' Ops Group Methods for uts
#' 
#' Apply the \code{\link{Ops}} methods in base \R{} to the observation values of \code{"uts"} objects.
#' 
#' @note For unary oparations, the output time series has the same observation times as the input time series.
#' @note For binary operations involving two time series \code{uts1} and \code{uts2}, the output time series has the union of observation times of \code{uts1} and \code{uts2}, but excluding times before \emph{both} time series have their initial observation. The method for determining these times is unaffected by numerical noise less than \code{sqrt(\link[=.Machine]{.Machine$double.eps})}.
#' 
#' @param e1,e2 either \code{"uts"} objects or numeric numbers.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{groupGeneric}}
#' 
#' @examples
#' # Unary oparators
#' -ex_uts()
#' !ex_uts()
#' 
#' # Binary operators
#' ex_uts() * 2
#' 2 * ex_uts()
#' ex_uts() / ex_uts()
#' ex_uts() > 48
#' 48 >= ex_uts()
Ops.uts <- function(e1, e2)
{
  # Unary operator
  if (missing(e2)) {
    e1$values <- do.call(.Generic, list(e1$values))
    return(e1)
  }
  
  # Binary operator
  if (is.uts(e1) & is.uts(e2)) {
    # Determine time points of output time series
    if (min(length(e1), length(e2)) == 0)
      return(uts())
    all_times <- sorted_union(e1$times, e2$times, tolerance=.Machine$double.eps ^ 0.5)
    all_times <- all_times[all_times >= min(e1$times[1], e2$times[2])]
    attributes(all_times) <- attributes(e1$times)
    
    # Sample values
    values1 <- sample_values(e1, all_times)
    values2 <- sample_values(e2, all_times)
    
    # Generate output
    out <- uts(do.call(.Generic, list(values1, values2)), all_times)
  }  else if (is(e2, "numeric") | is(e2, "integer")) {
    out <- e1
    out$values <- do.call(.Generic, list(e1$values,  e2))
  } else {
    out <- e2
    out$values <- do.call(.Generic, list(e1,  e2$values))
  }
  out
}

