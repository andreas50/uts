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
#' A convenience wrapper around \code{\link{plot.default}} with several sensible default arguments.
#' 
#' @param x a \code{"uts"} object with numeric or logical observation values.
#' @param max_dt a non-negative \code{\link[lubridate]{duration}} object. Consecutive observations are not connected by a line in the graph, if they are more than this amount apart in time.
#' @param type what type of plot should be drawn, see \code{\link[graphics:plot.default]{plot.default}}. \code{type="b"} is helpful for highlighting individual observations.
#' @param col the colors for lines and points. See \code{\link[graphics:plot.default]{plot.default}}.
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @param \dots other graphical parameters passed to \code{\link[graphics:plot.default]{plot.default}}.
#' 
#' @examples
#' plot(ex_uts())
#' plot(ex_uts(), max_dt=dhours(12), type="b")   # don't connect points more than 12 hours apart
#' plot(ex_uts(), max_dt=dhours(2), type="b")
#' 
#' # Plot time series with NAs
#' tmp <- ex_uts()
#' tmp$values[2] <- NA
#' plot(tmp, type="o")         # isolated points are plotted as circles
#' plot(na.omit(tmp), type="b")
#' 
#' # Use custom date formatting for x axis
#' # -) the "format" argument is passed down to axis.POSIXct()
#' # -) this example produces several harmless warning messages, because other
#' #    functions that are part of the call chain have no "format" argument
#' plot(ex_uts(), format="%a %I%p")
plot.uts <- function(x, max_dt=ddays(Inf), type="l", col="blue", xlab="", ylab="", ...)
{
  # Argument checking
  if (length(x) == 0L)
    stop("Cannot plot time series of zero length")
  if (!is.numeric(x$values) && !is.logical(x$values))
    stop("Can only plot time series with numeric or logical observation values")

  # Set up empty plotting canvas
  plot(x$times, x$values, type="n", col=col, xlab=xlab, ylab=ylab, ...)
  
  # Remove plot.default() arguments that are not part of plot.xy()
  args <- c(list(...), type=type, col=col)
  drop <- names(args) %in% c("ann", "asp", "axes", "frame.plot", "log", "main", "panel.first", "sub", "xlab", "xlim", "ylab", "ylim")
  args <- args[!drop]
  
  # Plot individual segments
  segments <- split_segments(x, max_dt=max_dt)
  for (segment in segments) {
    xy <- xy.coords(segment$times, segment$values)
    args_seg <- c(xy=list(xy), args)
    do.call(plot.xy, args_seg)
  }
}

