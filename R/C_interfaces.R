#######################################
# R-C interfaces for helper functions #
#######################################

#' Element Positions of a Sorted Array within Another Sorted Array
#' 
#' Assume given two sorted one-dimensional arrays \code{a} and \code{b}. For each element \code{a[i]}, determine the maximum index \code{j} in \code{b} such that \code{b[j] <= a[i]}, or return \code{NA} if no such index exists.
#' 
#' @return An integer vector of indices of same length as \code{a}.
#' @param a a sorted vector of numbers.
#' @param b a sorted vector of numbers.
#' @param eps	tolerance for numerical noise, relative to the largest absolute element in \code{a} and \code{b}.
#' 
#' @examples
#' sorted_array_position(1:5, 1:5)
#' sorted_array_position(c(-3, 1, 3, 5, 7), c(0, 1, 4, 9, 16))
#' 
#' # Numerical noise < eps has no effect
#' sorted_array_position(1, c(0, 1))
#' sorted_array_position(1 - 1e-13, c(0, 1))
#' sorted_array_position(1 + 1e-13, c(0, 1))
#' 
sorted_array_position <- function(a, b, eps=1e-9)
{
  # Trivial cases
  if (length(a) == 0)
    return(c())
  if (length(b) == 0)
    return(rep(NA, length(a)))
  
  # Convert relative to absolute numerical noise tolerance
  eps_absolute <- eps * max(abs(a), abs(b), na.rm=TRUE)
  if (is.na(eps_absolute))
      return(rep(NA, length(a)))
  
  # Call C function
  res <- integer(length(a))
  res <- .C("sorted_array_position", as.double(a + eps_absolute), as.integer(length(a)), as.double(b),
      as.integer(length(b)), pos = res)$pos
  
  # Set non-found indices to NA 
  res[res == -1] <- NA
  
  # Shift indices by one, because R indicies start at one, while C indices start at zero
  as.integer(res + 1)
}


