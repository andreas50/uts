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
#' any(ex_uts() > 48)
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
#' # Calculate the cumulative sum of the observation values and return the corresponding "uts"
#' cumsum(ex_uts())
Math.uts <- function(x, ...)
{
  x$values <- do.call(.Generic, list(x$values, ...))
  x
}
if (0) {
  methods("Math")
}


#' Ops Group Methods Helper
#' 
#' A helper function that does the actual work for the \code{\link{Ops.uts}}.
#' 
#' @param e1,e2 see \code{\link{Ops.uts}}.
#' @param .Generic a character vector naming the generic function.
#' 
#' @keywords internal
#' @seealso \code{\link{groupGeneric}}
#' 
#' @examples
#' Ops_uts(ex_uts(), ex_uts(), "+")         # arithmetic
#' Ops_uts(ex_uts(), ex_uts(), ">")         # comparison
#' !Ops_uts(ex_uts() > 48, .Generic="!")    # logical operator
Ops_uts <- function(e1, e2, .Generic)
{
  # Unary operator
  if (missing(e2)) {
    e1$values <- do.call(.Generic, list(e1$values))
    return(e1)
  }
  
  # Binary operator
  if (is.uts(e1) && is.uts(e2)) {
    # Determine time points of output time series
    if (min(length(e1), length(e2)) == 0L)
      return(uts())
    all_times <- sorted_union(e1$times, e2$times, tolerance=.Machine$double.eps ^ 0.5)
    all_times <- all_times[all_times >= max(e1$times[1L], e2$times[1L])]
    attributes(all_times) <- attributes(e1$times)
    
    # Sample values
    values1 <- sample_values(e1, all_times)
    values2 <- sample_values(e2, all_times)
    
    # Generate output
    out <- uts(do.call(.Generic, list(values1, values2)), all_times)
  } else if (is.uts(e1)) {
    if (length(e2) != 1)
      stop("Group methods 'Ops' between a 'uts' and a other objects work only for objects of length one")
    out <- e1
    out$values <- do.call(.Generic, list(e1$values,  e2))
  } else {
    if (length(e1) != 1)
      stop("Group methods 'Ops' between a 'uts' and a other objects work only for objects of length one")
    out <- e2
    out$values <- do.call(.Generic, list(e1,  e2$values))
  }
  out
}


#' Ops Group Methods
#' 
#' Apply the \code{\link{Ops}} group methods in base \R{} to the observation values of \code{"uts"} objects.
#' 
#' For unary oparations or operations involving just one time series and another \R object, the output time series has the same observation times as the input time series.
#' 
#' For binary operations involving two time series \code{e1} and \code{e2}, the output time series has the union of observation times of \code{e1} and \code{e2}, but excluding times before the first observation time (i.e. the start time) of either time series. The method for determining these times is unaffected by numerical noise less than \code{sqrt(\link[=.Machine]{.Machine$double.eps})}.
#' 
#' @note The classes \code{"uts"}, \code{"uts_vector"}, and \code{"uts_matrix"} (see package \code{utsMultivariate} for the latter two) inherit from the abstract class \code{"uts_virtual"}. Because the Ops group methods are implemented via \code{Ops.uts_virtual}, operations such as subtraction can mix the classes.
#' 
#' @note For binary \code{\link{Ops}} group methods, this function is a special case of \code{\link{binary_Ops}}. Specifically, \code{e1+e2} gives the same result as \code{binary_Ops(e1, "+", e2, times="all", interpolation="last")}, and the same is true for the other binary \code{"Ops"} methods.
#' 
#' @param e1,e2 either \code{"uts"}, \code{"uts_vector"} \code{"uts_matrix"} objects, or compatible length-one \R objects, where compatability depends on the type of operation performed.
#' 
#' @aliases Ops.uts
#' @seealso \code{\link{groupGeneric}}, \code{\link{binary_Ops}}
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
Ops.uts_virtual <- function(e1, e2)
{
  # Unary operator
  cl1 <- intersect(class(e1), c("uts_matrix", "uts_vector", "uts"))[1]
  if (is.na(cl1))
    cl1 <- "other"
  if (missing(e2)) {
    if (cl1 == "uts")
      return(Ops_uts(e1, .Generic=.Generic))
    else if (cl1 == "uts_vector")
      if (requireNamespace("utsMultivariate", quietly=TRUE))
        return(utsMultivariate::Ops_uts_vector(e1, .Generic=.Generic))
      else
        stop("Package 'utsMultivariate' needed for this function to work")
    else
      stop("No Ops group methods available for this class")
  }

  
  # Binary operator
  cl2 <- intersect(class(e2), c("uts_matrix", "uts_vector", "uts"))[1]
  if (is.na(cl2))
    cl2 <- "other"
  if ((cl1 == "uts_vector") || (cl2 == "uts_vector")) {
    if (requireNamespace("utsMultivariate", quietly=TRUE))
        utsMultivariate::Ops_uts_vector(e1, e2, .Generic=.Generic)
      else
        stop("Package 'utsMultivariate' needed for this function to work")
  } else if ((cl1 == "uts") || (cl2 == "uts")) {
    Ops_uts(e1, e2, .Generic=.Generic)
  } else
    stop("This class does not support Ops operators")
  
  
  # Call appropriate function
#   if ((cl1 %in% c("uts_matrix", "uts_vector") || (cl1 %in% c("numeric", "integer") && length(e1) == 1)) &
#        (cl2 %in% c("uts_matrix", "uts_vector") || (cl2 %in% c("numeric", "integer") && length(e2) == 1)))
#     Ops_uts_vector(e1, e2, .Generic)
#   else if ((cl1 %in% c("uts_matrix", "uts_vector", "uts", "numeric", "integer")) &&
#        (cl2 %in% c("uts_matrix", "uts_vector", "uts", "numeric", "integer")))
#     Ops_uts_vector_mixed(e1, e2, .Generic)
#   else
#     stop("Class does not support Ops operators.")
}


#' Binary Ops Groups Methods
#' 
#' Apply a binary \code{\link{Ops}} group method to the observation values of \code{"uts"} objects.
#' 
#' @param x,y \code{\link{uts}} objects.
#' @param Ops a binary \code{\link{Ops}} operator.
#' @param times which observation times to use for the output time series: \itemize{
#'    \item \code{"all"}: the union of observation times of \code{x} and \code{y}.
#'    \item \code{"x"}: the observation times of \code{x}.
#'    \item \code{"y"}: the observation times of \code{y}.
#' }
#' In all three cases, times before the first observation time (i.e. the start time) of either time series are excluded. \cr
#' The method for determining these times is unaffected by numerical noise less than \code{sqrt(\link[=.Machine]{.Machine$double.eps})}.
#' @param interpolation either \code{"last"} or \code{"linear"}, denoting the interpolation method to use for sampling from \code{x} and \code{y}. See \code{\link{sample_values}} for a detailed description of the two methods.
#' 
#' @note \code{\link{Ops.uts}}, when using a binary operator, is a special case of this method. Specifically, \code{x+y} gives the same result as \code{binary_Ops(x, "+", y, times="all", interpolation="last")}, and the same is true for the other binary \code{"Ops"} group\code{\link{Ops}} methods.
#' 
#' @note For output times that are observation times of \code{x} (or \code{y}), the interpolation method has no effect, because the sampled value at such a time point is simply the observation value of \code{x} (or \code{y}) at this time point. In particular, the \code{interpolation} argument has no effect for \code{times="all"} (the default).
#' 
#' @seealso \code{\link{Ops.uts}}, \code{\link{groupGeneric}}
#' 
#' @examples
#' 
#' # Create two sample time series
#' x <- ex_uts()
#' y <- head(lag_t(x * 1.1, dhours(1)), 5)
#' 
#' # Vary binary operator
#' binary_Ops(x, "/", y)
#' binary_Ops(x, "-", y)
#' binary_Ops(x, "!=", y)
#' 
#' # Vary time points in output
#' binary_Ops(x, "/", y)
#' binary_Ops(x, "/", y, times="x")
#' binary_Ops(x, "/", y, times="y")
#' 
#' # Vary interpolation method
#' binary_Ops(x, "/", y)
#' binary_Ops(x, "/", y, interpolation="linear")
binary_Ops <- function(x, Ops, y, times="all", interpolation="last")
{
  # Argument checking
  if (!is.uts(x) || !is.uts(y))
    stop("x and y need to be 'uts' objects")
  if (!(times %in% c("all", "x", "y")))
    stop("Illegal 'times' argument")
  if (!(interpolation %in% c("last", "linear")))
    stop("Illegal 'interpolation' argument")
  
  # Sample the input time series
  if (times == "x")
    y <- window(y[x$times, interpolation=interpolation], start=start(y))
  else if (times == "y")
    x <- window(x[y$times, interpolation=interpolation], start=start(x))
  else if (times == "all") {
    # Determine time points of output time series
    if (min(length(x), length(y)) == 0L)
      return(uts())
    all_times <- sorted_union(x$times, y$times, tolerance=.Machine$double.eps ^ 0.5)
    all_times <- all_times[all_times >= max(x$times[1L], y$times[1L])]
    attributes(all_times) <- attributes(x$times)
    
    # Sample the input time series
    x <- x[all_times, interpolation=interpolation]
    y <- y[all_times, interpolation=interpolation]
  }
  
  # Call Ops method
  do.call(Ops, list(x, y))
}


