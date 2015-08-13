######################
# Plotting functions #
######################

#' Split Into Segments
#'
#' Split a time series into the minimum number of segments (i.e. subperiod time series), such that consecutive observation times in the same segment are less than a specified value apart.
#'
#' @return A list of \code{"uts"} objects. Consecutive observation times within each time series are less than \code{max_dt} apart.
#' @param x a \code{"uts"} object.
#' @param max_dt a non-negative \code{\link[lubridate]{duration}} object, specifying the maximum temporal spacing of consecutive observation times in the same segment.
#' 
#' @keywords internal
#' @seealso \code{\link{plot}}, which optionally uses this function to not connect consecutive observations by a line.
#' @examples
#' split_segments(ex_uts(), ddays(0.25))
#' split_segments(ex_uts(), ddays(0.5))
#' split_segments(ex_uts(), ddays(Inf))
#' split_segments(ex_uts(), ddays(0))
split_segments <- function(x, max_dt)
{
  # Argument checking
  if (!is.duration(max_dt))
    stop("'max_dt' is not a duration object")
  if (max_dt < ddays(0))
    stop("'max_dt' cannot be negative")
  
  # Determine split points
  splits <- which(diff(x$times) > max_dt)
  splits <- c(0, splits, length(x))
  
  # Extract segements
  segments <- list()
  for (j in 1:(length(splits) - 1)) {
    used <- (splits[j] + 1):splits[j+1]
    tmp <- x
    tmp$values <- tmp$values[used]
    tmp$times <- tmp$times[used]
    segments[[j]] <- tmp
  }
  segments
}


#' Plot a uts
#' 
#' This function is convenience wrapper around \code{\link[graphics:plot.default]{plot.default}} with several sensible default arguments.
#' 
#' @param x a numeric \code{"uts"} object that should be plotted.
#' @param max_dt a non-negative \code{\link[lubridate]{duration}} object. Consecutive observations are not connected by a line in the graph, if they are more than this amount apart in time.
#' @param type what type of plot should be drawn, see \code{\link[graphics:plot.default]{plot.default}}. \code{type="b"} is helpful for highlighting individual observations.
#' @param col the colors for lines and points. See \code{\link[graphics:plot.default]{plot.default}}.
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @param \dots other graphical parameters passed to \code{\link[graphics:plot.default]{plot.default}}.
#' 
#' @examples
#' plot(ex_uts())
#' plot(ex_uts(), max_dt=dhours(12))   # don't connect points more than 12 hours apart
#' 
#' # Plot time series with NAs
#' tmp <- ex_uts()
#' tmp$values[2] <- NA
#' plot(tmp, type="o")         # isolated points are plotted as circles
#' #plot(na.omit, type="b")
#' 
#' # Use custom date formatting for x axis
#' # -) the "format" argument is passed down to axis.POSIXct()
#' # -) this example produces several harmless warning messages, because other
#' #    functions that are part of the call chain have no "format" argument
#' plot(ex_uts(), format="%a %I%p")
plot.uts <- function(x, max_dt=ddays(Inf), type="l", col="blue", xlab="", ylab="", ...)
{
  # Argument checking
  if (length(x) == 0)
    stop("Cannot plot time series of zero length")
  if (!is.numeric(x$values))
    stop("Can only plot numeric time series")

  # Set up empty plotting canvas
  plot(x$times, x$values, type="n", col=col, xlab=xlab, ylab=ylab, ...)
  
  # Plot individual segments
  segments <- split_segments(x, max_dt=max_dt)
  for (segment in segments)
    lines(segment$times, segment$values, col=col, type=type, ...)
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

