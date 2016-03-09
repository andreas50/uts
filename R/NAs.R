###################
# Handling of NAs #
###################

#' Remove NA Observation Values
#' 
#' Returns the object with incomplete cases removed.
#' 
#' @param object a time series object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{is.na.uts}}
#' @seealso \code{\link{is.na}}, \code{\link{na.omit}} in base \R.
#' 
#' @examples
#' # Remove NAs from a "uts"
#' test <- ex_uts()
#' test$values[c(2, 4)] <- NA
#' na.omit(test)
na.omit.uts <- function(object, ...)
{
  keep <- !is.na(object$values)
  object$values <- object$values[keep]
  object$times <- object$times[keep]
  object
}


#' Not Available / Missing Observation Values
#' 
#' Find missing values.
#' 
#' @return A logical \code{"uts"} (i.e. a \code{"uts"} with \code{\link{logical}} observation values), indicating which observation values are \code{NA}.
#' @param x a \code{"uts"} object.
#' 
#' @seealso \code{\link{na.omit.uts}}
#' @seealso \code{\link{is.na}}, \code{\link{na.omit}} in base \R.
#' 
#' @examples
#' # Set observation to NA
#' test <- ex_uts()
#' test$values[c(2, 4)] <- NA      # NEXT: use replacement using time points
#' 
#' # Get logical "uts", indicating which observations are NA
#' is.na(test)
is.na.uts <- function(x)
{
  uts(is.na(x$values), x$times)
}
