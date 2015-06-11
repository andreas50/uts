#######################################
# R-C interfaces for helper functions #
#######################################

#' Less-Than-Or-Equal Comparison of Sorted Arrays
#' 
#' Assume given two sorted one-dimensional arrays \code{a} and \code{b}. For each element \code{a[i]}, determine the number of elements in \code{b} that are less than or equal (leq) to this value.
#' 
#' @note
#' Equivalently, because the input arrays are sorted, for each element \code{a[i]} determine the maximum index \code{j} with \code{b[j] <= a[i]}. 
#' 
#' @return A vector of same length as \code{a}.
#' @param a a sorted vector of numbers.
#' @param b a sorted vector of numbers.
#' @param tolerance tolerance for numerical noise.
#' 
#' @examples
#' # The second vector has
#' # -) 0 elements leq (less-than-or-equal) -3
#' # -) 2 elements leq 1
#' # -) 2 elements leq 3
#' # -) 3 elements leq 5
#' # -) 3 elements leq 7
#' num_leq_sorted_arrays(c(-3, 1, 3, 5, 7), c(0, 1, 4, 9, 16))
#' 
#' # Numerical noise < tolerance has no effect
#' num_leq_sorted_arrays(1, 0:2, tolerance=1e-12)
#' num_leq_sorted_arrays(1 - 1e-13, 0:2, tolerance=1e-12)
#' num_leq_sorted_arrays(1 + 1e-13, 0:2, tolerance=1e-12)
#' 
#' # Trivial cases
#' num_leq_sorted_arrays(1:5, 1:5)
#' num_leq_sorted_arrays(c(), 1:5)
#' num_leq_sorted_arrays(1:5, c())
num_leq_sorted_arrays <- function(a, b, tolerance=0)
{
  # Trivial cases
  if (length(a) == 0)
    return(numeric())
  if (length(b) == 0)
    return(rep(0, length(a)))
  
  # Input checking
  if (anyNA(a) | anyNA(b))
    stop("NAs are not allowed as input.")
  
  # Call C function
  res <- integer(length(a))
  .C("num_leq_sorted_arrays", as.double(a + tolerance), as.integer(length(a)),
    as.double(b), as.integer(length(b)), pos = res)$pos
}

