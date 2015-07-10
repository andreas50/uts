##########################################
# Sampling functionality for UTS objects #
##########################################

# -----------------
# Generic functions
# -----------------

#' Sample Values
#'
#' Sample values from a time series at given sampling times.
#' 
#' @return An vector of sampled values with same length as the input \code{x}.
#' @param x a time series object.
#' @param \dots further arguments passed to or from methods.
sample_values <- function(x, ...) UseMethod("sample_values")


# ----------------------
# Method implementations
# ----------------------

#' @describeIn sample_values sample from a \code{"uts"} object
#'
#' @param time_points a strictly increasing sequence of \code{\link{POSIXct}} date-times.
#' @param method the sampling method.
#' \code{"last"} returns the most recent observation value in \code{x} for each sampling time.
#' \code{"linear"} returns for each sampling time the linearily-interpolated  observation values in \code{x} that immediately preceede and follow each sampling time.
#' @param max_dt a duration object, specifying the maximum time difference between each sampling time and the observation times in \code{x} used for determining the sampled value. Return \code{NA} as sampled value for sampling times for which this threshold is exceeded.
#' @param tolerance tolerance for numerical noise in observation times. See \code{\link{num_leq_sorted_arrays}}.
#' 
#' @examples
#' # Create a numeric "uts"
#'  
#' # Sample the most recent observation
#' 
#' # Sample with linear interpolation
sample_values.uts <- function(x, time_points, method="last", max_dt=ddays(Inf),
  tolerance=.Machine$double.eps ^ 0.5, ...)
{  
  # Argument checking
  if (!is.POSIXct(time_points))
    stop("'time_points' is not a POSIXct' object")
  if (any(diff(time_points) <= 0))
    stop("'time_points' needs to be a strictly increasing time sequence")
  #
  if (!(method %in% c("last", "linear")))
    stop("Unknown sampling 'method'")
  if ((method == "linear") & !is.numeric(x$values))
    stop("Sampling with linear interpolation is only supported for numeric time series")
  #
  if (!is.duration(max_dt))
    stop("'max_lag' is not a duration object")
  
  # For each sampling time, determine the most recent observation time, and enforce the 'max_dt' threshold 
  sampling_idx_last <- num_leq_sorted_arrays(time_points, x$times, tolerance=tolerance)
  sampling_idx_last[sampling_idx_last == 0] <- NA
  sampled_times_last <- x$times[sampling_idx_last]
  dt_last_observation <- as.duration(time_points - sampled_times_last) 
  if (max_dt < ddays(Inf))
    sampling_idx_last[dt_last_observation > max_dt] <- NA
  sampled_values_last <- x$values[sampling_idx_last]
  
  # Return samples values for method="last"
  if (method == "last")
    return(sampled_values_last)
  
  ### code for method="linear" only ###
  
  # For each sampling time, determine the next observation time, and enforce the 'max_dt' threshold
  perfect_match <- time_points %in% x$times
  sampling_idx_next <- pmin(length(x), sampling_idx_last + !perfect_match)
  sampled_times_next <- x$times[sampling_idx_next]
  dt_next_observation <- as.duration(sampled_times_next - time_points)
  if (max_dt < ddays(Inf))
    sampling_idx_next[dt_next_observation > max_dt] <- NA
  sampled_values_next <- x$values[sampling_idx_next]
  
  # Linearly interpolate last and next observation value
  w <- pmax(0, dt_next_observation) / (as.numeric(dt_last_observation) + pmax(0, dt_next_observation))
  w[perfect_match] <- 1
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
  if (is.uts(time_points))
    time_points <- time_points$times[!is.na(time_points$values) & time_points$values]
  num_times <- length(time_points)
  
  # Determine values for insertion
  if (length(value) == 1)
    value <- rep(value, num_times)
  num_values <- length(value)
  if ((num_values > num_times) & (num_times == 1)) {
    value <- list(value)
    num_values <- 1
  }
  if (num_values != num_times)
    stop("The number of time points to replace/insert does not match the number of observation values provided.")
  
  # Do insertion via merge()
  uts_inseration <- uts(value, time_points)
  merge(uts_inseration, x, ...)
}

