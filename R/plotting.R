
#' Plot a uts
#' 
#' This function is convenience wrapper around \code{\link[graphics:plot.default]{plot.default}} with several sensible default arguments.
#' 
#' @param x a numeric \code{\link{uts}} that should be plotted.
#' @param type what type of plot should be drawn, see \code{\link[graphics:plot.default]{plot.default}}. \code{type="b"} is helpful for highlighting individual observations.
#' @param col the colors for lines and points. See \code{\link[graphics:plot.default]{plot.default}}.
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @param \dots other graphical parameters passed to \code{\link[graphics:plot.default]{plot.default}}.
#' 
#' @examples
#' plot(ex_uts())
#' 
#' # Use custom date formatting for x axis
#' # -) the "format" argument is passed down to axis.POSIXct()
#' # -) this example produces several harmless warning messages, because other
#' #    functions that are part of the call chain have no "format" argument
#' plot(ex_uts(), format="%a %I%p")
#'
plot.uts <- function(x, type="l", col="blue", xlab="", ylab="", ...)
{
  # Argument checking
  if (length(x$values) == 0)
    stop("Cannot plot time series of zero length")
  if (!is.numeric(x$values))
    stop("Can only plot numeric time series")

  # Call default plotting function with sensible defaults
  plot(x$times, x$values, type=type, col=col, xlab=xlab, ylab=ylab, ...)
}
if (0) {
  plot(ex_uts())
  plot(ex_uts(), type="b")
  plot(ex_uts(), type="o")
  plot(ex_uts(), type="n")  # empty plot
  #
  plot(SPX)
  plot(subperiod(SPX, "2000-01-01", "2005-01-01"))
  plot(subperiod(SPX, "2000-01-01", "2001-01-01"))
  plot(subperiod(SPX, "2000-01-01", "2000-01-30"))
  plot(subperiod(SPX, "2000-01-01", "2000-01-30"), format="%m/%d")   # works, but gives tons of warning messages. Only axis.POSIXct() uses this argument
  
  # Manual axis plotting
  # -) bad: still need to pick reasonable tick positions
  # -) NEXT: how to pass 'format' argument directly to plot.default?
  tmp <- subperiod(SPX, "2000-01-01", "2001-01-01")
  plot(tmp, xaxt="n")
  axis.POSIXct(1, at=tmp$times, format="%m/%m/%Y")
}