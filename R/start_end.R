#' First and Last Observation Time
#' 
#' Get the first and last observation time, respectively.
#' 
#' @return \code{start()} returns the first observation time.
#' @param x a \code{"uts"} object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @examples
#' start(ex_uts())
start.uts <- function(x, ...)
{
  if (length(x) > 0)
    x$times[1L]
  else
    as.POSIXct(NA)
}


#' @rdname start.uts
#' 
#' @return \code{end()} returns the last observation time.
#' 
#' @examples
#' end(ex_uts())
end.uts <- function(x, ...)
{
  if (length(x) > 0)
    x$times[length(x$times)]
  else
    as.POSIXct(NA)
}