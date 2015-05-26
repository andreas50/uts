#######################################
# R-C interfaces for helper functions #
#######################################

#' Position of Elements of a Sorted Array within Another Sorted Array
#' 
#' Assume given two sorted one-dimensional arrays 'a' and 'b'. For each element a[i], determine the
#' maximum index j in 'b' such that b[j] <= a[i], (NA if not found).
#' 
#' @return A vector of indices of same length as vector \code{a}.
#' @param a a sorted vector of numbers.
#' @param b a sorted vector of numbers.
#' @param eps	tolerance for numerical noise
#' 
#' @examples
#' sorted_array_position(c(-3, 1, 3, 5, 7), c(0, 1, 4, 9, 16))
#' 
sorted_array_position <- function(a, b, eps=1e-12)
{
  # Trivial case
  if (length(b) == 0)
    return(rep(NA, length(a)))
  
  # Call C function
  res <- integer(length(a))
  res <- .C("sortedPos", as.double(a + eps), as.integer(length(a)), as.double(b),
      as.integer(length(b)), pos = res)$pos

  # Set non-found indices to NA 
  res[res == -1] <- NA
  
  # Shift indices by one, because R indicies start at one, while C indices start at zero
  res + 1
}
if (0) {
  sortedPos_C(-3, c(0, 1, 4, 9, 16))
  sortedPos_C(c(-3, 1, 3, 5, 7), c(0, 1, 4, 9, 16), not_found=-1)
  sorted_pos(c(-3, 1, 3, 5, 7), c(0, 1, 4, 9, 16))
  # numerical stability
  sortedPos_C(1 - 1e-13, c(0, 1))         # RT noise allows
  sortedPos_C(1 - 1e-13, c(0, 1), eps=0)  # RT no noise allowed
  
  # Speed analysis: 4-5 times faster than R implementation
  tmp <- as.numeric(SPX$times)
  Rprof(interval=0.01)    # 1.7s
  for (i in 1:2000)
    sorted_pos(tmp, tmp)
  Rprof(NULL)
  summaryRprof()
  #
  tmp <- as.numeric(SPX$times)    # 0.38s
  Rprof(interval=0.01)
  for (i in 1:2000)
    sortedPos_C(tmp, tmp)
  Rprof(NULL)
  summaryRprof()
}

