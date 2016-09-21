######################################
# Sampling values from a time series #
######################################

#' Sample Values
#'
#' Sample observation values from a time series at given sampling times.
#' 
#' @return An vector of sampled values with same length as the input \code{x}.
#' @param x a time series object.
#' @param \dots further arguments passed to or from methods.
sample_values <- function(x, ...) UseMethod("sample_values")


#' @describeIn sample_values sample from a \code{"uts"} object
#'
#' @param time_points a strictly increasing sequence of \code{\link{POSIXct}} date-times.
#' @param interpolation the interpolation method: \itemize{
#'    \item \code{"last"}: for each sampling time, return the most recent (i.e. last available) observation value in \code{x}.
#'    \item \code{"linear"}: for each sampling time, return the linearily-interpolated value in \code{x} of the observation that immediately precedes and follows such sampling time.
#' }
#' @param max_dt a duration object, specifying the maximum time difference between each sampling time and the observation times in \code{x} used for determining the sampled value. Return \code{NA} as sampled value for sampling times for which this threshold is exceeded.
#' @param tolerance tolerance for numerical noise in observation times. See \code{\link{num_leq_sorted}}.
#' 
#' @examples
#' # Sample the most recent observation
#' times <- as.POSIXct(c("2007-11-09", "2007-11-10"))
#' sample_values(ex_uts(), times)
#' 
#' # Sample with linear interpolation
#' sample_values(ex_uts(), times, interpolation="linear")
#' 
#' # Sample a non-numeric time series
#' sample_values(ex_uts2(), times)
#' 
#' # Error, because only numeric time series can be linearly interpolated
#' \dontrun{sample_values(ex_uts2(), as.POSIXct("2007-01-01"), interpolation="linear")}
sample_values.uts <- function(x, time_points, interpolation="last", max_dt=ddays(Inf),
  tolerance=.Machine$double.eps ^ 0.5, ...)
{ 
  # Remark: If it wasn't for the 'max_dt' argument, the approx() function could be used.
  #         For example: approx(x$times, x$values, xout=time_points, interpolation="linear", rule=1:2)
  
  # Argument checking
  if (!is.POSIXct(time_points))
    stop("'time_points' is not a POSIXct' object")
  if (is.unsorted(time_points, strictly=TRUE))
    stop("'time_points' needs to be a strictly increasing time sequence")
  if (!(interpolation %in% c("last", "linear")))
    stop("Unknown sampling 'method'")
  if ((interpolation == "linear") && !is.numeric(x$values) && !is.logical(x$values))
    stop("Sampling with linear interpolation is only supported for time series with numeric or logical observation values")
  if (!is.duration(max_dt))
    stop("'max_dt' is not a duration object")
  if (as.numeric(max_dt) < 0)
    stop("'max_dt' is negative")
  
  # For each sampling time, determine the most recent observation time, and enforce the 'max_dt' threshold 
  sampling_idx_last <- num_leq_sorted(time_points, x$times, tolerance=tolerance)
  sampling_idx_last[sampling_idx_last == 0L] <- NA
  sampled_times_last <- x$times[sampling_idx_last]
  dt_last_observation <- as.duration(time_points - sampled_times_last) 
  if (max_dt < ddays(Inf))
    sampling_idx_last[dt_last_observation > max_dt] <- NA
  sampled_values_last <- x$values[sampling_idx_last]
  
  # Return sampled values for last-point interpolation
  if (interpolation == "last")
    return(sampled_values_last)
  
  ### code for linear interpolation only ###
  
  # For each sampling time, determine the next observation time, and enforce the 'max_dt' threshold
  sampling_idx_next <- pmin(sampling_idx_last + 1, length(x))
  sampled_times_next <- x$times[sampling_idx_next]
  dt_next_observation <- as.duration(sampled_times_next - time_points)
  if (max_dt < ddays(Inf))
    sampling_idx_next[dt_next_observation > max_dt] <- NA
  sampled_values_next <- x$values[sampling_idx_next]
  
  # Linearly interpolate last and next observation value
  w <- pmax(0, as.numeric(dt_next_observation)) / (as.numeric(dt_last_observation) + pmax(0, as.numeric(dt_next_observation)))
  w[sampling_idx_last == length(x)] <- 1
  w * sampled_values_last + (1-w) * sampled_values_next
}


#' Extract or Replace Parts of a uts
#'
#' The accessor method (\code{"["}) extracts a sub-sampled time series with the provided times. The replacement method (\code{"[<-"}) inserts observation values at the provided observation times, replacing observations values for already existing observation times (if any).
#' 
#' @param x a \code{"uts"} object.
#' @param time_points either a strictly increasing sequence of \code{\link{POSIXct}} date-times, or a \code{"uts"} with \code{\link{logical}} observation values.
#' @param \dots further arguments passed to \code{\link{sample_values}}.
#' 
#' @examples
#' # Sample at single time point
#' ex_uts()[as.POSIXct("2000-01-01")]
#' 
#' # Sample at multiple time points, optionally restricting the maximum time difference
#' # between sampling and observation times
#' times <- as.POSIXct(c("2007-11-08 11:01:00", "2007-11-09 15:16:00"))
#' ex_uts()[times]
#' ex_uts()[times, max_dt = dhours(1)]
`[.uts` <- function(x, time_points, ...)
{
  # Sample values
  if (is.POSIXct(time_points))
    values_new <- sample_values(x, time_points, ...)
  else if (inherits(time_points, "uts") && is.logical(time_points$values)) {
    # Sample from logical UTS (use TRUE ticks)
    time_points <- time_points$times[time_points$values]
    values_new <- sample_values(x, time_points, ...)
  } else
    stop("The 'time_points' argument is of the wrong type")
  
  # Return new sampled time series
  uts(values_new, time_points)
}



#' @rdname sub-.uts
#' 
#' @param value a vector of observation values to insert at the time points \code{time_points}.
#' 
#' @examples
#' # Insert multiple numeric values
#' test <- ex_uts()
#' test[Sys.time() + ddays(1:2)] <- c(51, 52)
#' 
#' # Insert non-numeric value
#' test <- ex_uts()
#' test[Sys.time() + ddays(3)] <- list(cat=1, dog=2)
#' 
#' # Replacement times from logical "uts"
#' test <- ex_uts()
#' test[test >= 48] <- 50
`[<-.uts` <- function(x, time_points, ..., value)
{
  # Determine time points for insertion
  if (is.uts(time_points)) {
    if (!is.logical(time_points$values))
      stop("Only time series with logical observation values can be used for sampling")
    time_points <- time_points$times[!is.na(time_points$values) & time_points$values]
  }
  num_times <- length(time_points) 
  
  # Determine values for insertion
  if (length(value) == 1)
    value <- rep(value, num_times)
  num_values <- length(value)
  if ((num_values > num_times) && (num_times == 1)) {
    value <- list(value)
    num_values <- 1
  }
  if (num_values != num_times)
    stop("The number of time points to replace/insert does not match the number of observation values provided")
  
  # Do insertion via merge()
  uts_inseration <- uts(value, time_points)
  merge(uts_inseration, x, ...)
}

