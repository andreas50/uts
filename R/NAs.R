###################
# Handling of NAs #
###################

#' Remove NAs
#' 
#' Returns the object with incomplete cases removed.
#' 
#' @param object a time series object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link[base:is.na]{is.na}}, \code{\link[stats:na.fail]{na.fail}}, \code{\link[stats:na.fail]{na.omit}} in base \R.
#' @seealso \code{\link{is.na}} for \code{"uts"} objects.
#' @examples
#' # Remove NAs from a "uts"
#' tmp <- ex_uts()
#' tmp$values[c(2, 4)] <- NA
#' na.omit(tmp)
na.omit.uts <- function(object, ...)
{
  keep <- !is.na(object$values)
  object$values <- object$values[keep]
  object$times <- object$times[keep]
  object
}


#' Not Available / Missing Values
#' 
#' Find missing values.
#' 
#' @return A logical \code{"uts"} (i.e. a \code{"uts"} with \code{\link{logical}} observation values), indicating which observation values are \code{NA}.
#' @param x a \code{"uts"} object.
#' @seealso \code{\link[base:is.na]{is.na}}, \code{\link[stats:na.fail]{na.fail}}, \code{\link[stats:na.fail]{na.omit}} in base \R.
#' @seealso \code{\link{na.omit}} for \code{"uts"} objects.
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
