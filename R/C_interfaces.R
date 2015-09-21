#######################################
# R-C interfaces for helper functions #
#######################################

#' Less-Than-Or-Equal Comparison of Sorted Vectors
#' 
#' Assume given two sorted numeric vectors \code{a} and \code{b}. For each element \code{a[i]}, determine the number of elements in \code{b} that are less than or equal (leq) to this value.
#' 
#' @note
#' Equivalently, because the input vectors are sorted, for each element \code{a[i]} determine the maximum index \code{j} with \code{b[j] <= a[i]}. 
#' 
#' @return An integer vector of same length as \code{a}.
#' @param a a sorted vector of numbers.
#' @param b a sorted vector of numbers.
#' @param tolerance a non-negative number, indicating the tolerance for numerical noise.
#' 
#' @keywords internal
#' @examples
#' # The second vector has
#' # -) 0 elements leq (less-than-or-equal) -3
#' # -) 2 elements leq 1
#' # -) 2 elements leq 3
#' # -) 3 elements leq 5
#' # -) 3 elements leq 7
#' num_leq_sorted(c(-3, 1, 3, 5, 7), c(0, 1, 4, 9, 16))
#' 
#' # Numerical noise < tolerance has no effect
#' num_leq_sorted(1, 0:2, tolerance=1e-12)
#' num_leq_sorted(1 - 1e-13, 0:2, tolerance=1e-12)
#' num_leq_sorted(1 + 1e-13, 0:2, tolerance=1e-12)
#' 
#' # Trivial cases
#' num_leq_sorted(1:5, 1:5)
#' num_leq_sorted(c(), 1:5)
#' num_leq_sorted(1:5, c())
num_leq_sorted <- function(a, b, tolerance=0)
{
  # Argument checking
  if (is.na(tolerance))
    stop("'tolerance' is NA")
  if (tolerance < 0)
    stop("'tolerance' is negative")
  if (anyNA(a) | anyNA(b))
    stop("NAs are not allowed as input")
  if (any(diff(a) < 0))
    stop("'a' is not strictly increasing")
  if (any(diff(b) < 0))
    stop("'b' is not strictly increasing")
  
  # Call C function
  res <- integer(length(a))
  .C("num_leq_sorted", as.double(a + tolerance), as.integer(length(a)),
    as.double(b), as.integer(length(b)), pos = res)$pos
}


#' R implementation of num_leq_sorted
#'
#' This functions is identical to \code{\link{num_leq_sorted}}, except that a) the input vectors need to be strictly increasing, andb) there is no numerical noise tolerance support. It exists solely for testing the C implementation.
#'
#' @return An integer vector of same length as \code{a}.
#' @param a a strictly increasing vector of numbers.
#' @param b a strictly increasing vector of numbers.
#'
#' @keywords internal
#' @examples
#' num_leq_sorted_R(c(-3, 1, 3, 5, 7), c(0, 1, 4, 9, 16))
num_leq_sorted_R <- function(a, b)
{
  # Argument checking
  if (anyNA(a) | anyNA(b))
    stop("NAs are not allowed as input")
  if (any(diff(a) <= 0))
    stop("'a' is not strictly increasing")
  if (any(diff(b) <= 0))
    stop("'b' is not strictly increasing")
  
  all_values <- sort(union(a, b))
  cum_available <- cumsum(all_values %in% b)
  cum_available[all_values %in% a]
}


#' Sorted Union
#' 
#' For two sorted numeric vectors \code{a} and \code{b}, determine the sorted union of elements.
#' 
#' @return A numeric vector.
#' @param a a sorted vector of numbers.
#' @param b a sorted vector of numbers.
#' @param tolerance a non-negative number, indicating the tolerance for numerical noise. Specifically, numbers in \code{a} and \code{b} are appended one by one and in the appropriate order to the output vector. However, a number is only added if it is more than \code{tolerance} larger than the most recently added number.
#' 
#' @keywords internal
#' @examples
#' sorted_union(1:3, 2:4)
#' 
#' # Numerical noise < tolerance has no effect
#' sorted_union(0, 1e-14)
#' sorted_union(0, 1e-14, tolerance=1e-12)
#' sorted_union(c(0, 1e-14), 2, tolerance=1e-12)
#' 
#' # Trivial cases
#' sorted_union(1:10, c())
#' sorted_union(c(), 1:10)
sorted_union <- function(a, b, tolerance=0)
{
  # Argument checking
  if (is.na(tolerance))
    stop("'tolerance' is NA")
  if (tolerance < 0)
    stop("'tolerance' is negative")
  if (anyNA(a) | anyNA(b))
    stop("NAs are not allowed as input")
  
  # Call C-function
  res <- .C("sorted_union", as.double(a), length(a), as.double(b), length(b),
    tolerance=as.double(tolerance), res=numeric(length(a) + length(b)), length=integer(1))
  res$res[1L:res$length]
}

