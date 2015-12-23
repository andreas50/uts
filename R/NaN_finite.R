#' Finite, Infinite and NaN Numbers
#' 
#' Find finite, infinite, and NaN observation values.
#' 
#' @return A logical \code{"uts"} (i.e. a \code{"uts"} with \code{\link{logical}} observation values), indicating which observation values are finite, infinite, or \code{NaN}, respectively.
#' @param x a \code{"uts"} object.
#' 
#' @seealso \code{\link[base:is.finite]{is.finite}}, \code{\link[base:is.infinite]{is.infinite}}, \code{\link[base:is.nan]{is.nan}} in base \R.
#' 
#' @rdname is.finite
#' @examples
#' # Create sample time series
#' test <- ex_uts()
#' test$values[c(2, 4)] <- c(NaN, -Inf)
#' 
#' # Get logical "uts", indicating which observations are finite, infinte, or NaN
#' is.finite(test)
#' is.infinite(test)
#' is.nan(test)
is.finite.uts <- function(x)
{
  uts(is.finite(x$values), x$times)
}


#' @rdname is.finite
is.infinite.uts <- function(x)
{
  uts(is.infinite(x$values), x$times)
}


#' @rdname is.finite
is.nan.uts <- function(x)
{
  uts(is.nan(x$values), x$times)
}
