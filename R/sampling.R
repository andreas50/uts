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
#'
sample_values <- function(x, ...) UseMethod("sample_values")

# ----------------------
# Method implementations
# ----------------------

#' @describeIn sample_values sample from a \code{"uts"}
#'
#' @param sampling_times a strictly increasing sequence of \code{\link{POSIXct}} date-times.
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
#' # Sample with linear interpolatino
#'
sample_values.uts <- function(x, sampling_times, method="last", max_dt=ddays(Inf),
  tolerance=.Machine$double.eps ^ 0.5, ...)
{  
  # Argument checking
  if (!is.POSIXct(sampling_times))
    stop("'sampling_times' is not a POSIXct' object")
  if (!is.duration(max_dt))
    stop("'max_lag' is not a duration object")
  if (!(method %in% c("last", "linear")))
    stop("Unknown sampling 'method'")
  if (length(sampling_times) > 0)
    if (any(diff(as.double(sampling_times)) <= 0))
      stop("'sampling_times' needs to be a strictly increasing time sequence")
  
  # For each sampling time, determine the most recent observation time, and enforce the 'max_dt' threshold 
  sampling_idx_last <- num_leq_sorted_arrays(sampling_times, x$times, tolerance=tolerance)
  sampling_idx_last[sampling_idx_last == 0] <- NA
  sampled_times_last <- x$times[sampling_idx_last]
  dt_last_observation <- as.duration(sampling_times - sampled_times_last) 
  if (max_dt < Inf)
    sampling_idx_last[dt_last_observation > max_dt] <- NA
  sampled_values_last <- x$values[sampling_idx_last]
  
  # Return samples values for method="last"
  if (method == "last")
    return(sampled_values_last)
  
  ### code for method="linear" only ###
  
  # For each sampling time, determine the next observation time, and enforce the 'max_dt' threshold
  perfect_match <- sampling_times %in% x$times
  sampling_idx_next <- pmin(length(x$times), sampling_idx_last + !perfect_match)
  sampled_times_next <- x$times[sampling_idx_next]
  dt_next_observation <- as.duration(sampled_times_next - sampling_times)
  if (max_dt < Inf)
    sampling_idx_next[dt_next_observation > max_dt] <- NA
  sampled_values_next <- x$values[sampling_idx_next]
  
  # Linearly interpolate last and next observation value
  w <- pmax(0, dt_next_observation) / (as.numeric(dt_last_observation) + pmax(0, dt_next_observation))
  w[perfect_match] <- 1
  w * sampled_values_last + (1-w) * sampled_values_next
}


