#######################################
# R-C interfaces for helper functions #
#######################################

#' Less-Than-Or-Equal Comparison of Sorted Vectors
#' 
#' For two sorted numeric vectors \code{a} and \code{b}, for each element \code{a[i]} determine the number of elements in \code{b} that are less than or equal (leq) to this value.
#' 
#' @note
#' Equivalently, because the input vectors are sorted, for each element \code{a[i]} determine the maximum index \code{j} with \code{b[j] <= a[i]}.
#' 
#' @return An integer vector of same length as \code{a}.
#' @param a a sorted, i.e. non-decreasing, vector of numbers.
#' @param b a sorted, i.e. non-decreasing, vector of numbers.
#' @param tolerance a non-negative number, indicating the tolerance for numerical noise.
#' 
#' @keywords internal
#' @seealso \code{\link{num_less_sorted}} for a less-than comparison of two sorted vectors.
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
  if (anyNA(a) || anyNA(b))
    stop("NAs are not allowed as input")
  if (is.unsorted(a))
    stop("'a' is not sorted")
  if (is.unsorted(b))
    stop("'b' is not sorted")
  
  # Call C function
  # -) using C_num_leq_sorted gives unknown error "NULL value passed as symbol address" -> not used yet
  num_leq <- integer(length(a))
  .C("num_leq_sorted", as.double(a + tolerance), as.integer(length(a)),
    as.double(b), as.integer(length(b)), num_leq=num_leq)$num_leq
  #.C(C_num_leq_sorted, as.double(a + tolerance), as.integer(length(a)),
  #  as.double(b), as.integer(length(b)), num_leq=num_leq)$num_leq
}


#' Less-Than Comparison of Sorted Vectors
#' 
#' For two sorted numeric vectors \code{a} and \code{b}, for each element \code{a[i]} determine the number of elements in \code{b} that are less than this value.
#' 
#' @note
#' Equivalently, because the input vectors are sorted, for each element \code{a[i]} determine the maximum index \code{j} with \code{b[j] < a[i]}. 
#' 
#' @return An integer vector of same length as \code{a}.
#' @param a a sorted, i.e. non-decreasing, vector of numbers.
#' @param b a sorted, i.e. non-decreasing, vector of numbers.
#' 
#' @keywords internal
#' @seealso \code{\link{num_leq_sorted}} for a less-than-or-equqal comparison of two sorted vectors.
#' @examples
#' # The second vector has
#' # -) 0 elements less than -3
#' # -) 1 element less than 1
#' # -) 2 elements less than 3
#' # -) 3 elements less than 5
#' # -) 3 elements less than 7
#' num_less_sorted(c(-3, 1, 3, 5, 7), c(0, 1, 4, 9, 16))
#' 
#' # Trivial cases
#' num_less_sorted(1:5, 1:5)
#' num_less_sorted(c(), 1:5)
#' num_less_sorted(1:5, c())
num_less_sorted <- function(a, b, tolerance=0)
{
  # Argument checking
  if (anyNA(a) || anyNA(b))
    stop("NAs are not allowed as input")
  if (is.unsorted(a))
    stop("'a' is not sorted")
  if (is.unsorted(b))
    stop("'b' is not sorted")
  
  # Call C function
  # -) using C_num_leq_sorted gives unknown error "NULL value passed as symbol address" -> not used yet
  num_less <- integer(length(a))
  .C("num_less_sorted", as.double(a), as.integer(length(a)),
     as.double(b), as.integer(length(b)), num_less=num_less)$num_less
  #.C(C_num_less_sorted, as.double(a), as.integer(length(a)),
  #   as.double(b), as.integer(length(b)), num_less=num_less)$num_less
}


#' R implementation of num_leq_sorted
#'
#' This function is identical to \code{\link{num_leq_sorted}} except that a) the input vectors need to be strictly increasing, and b) there is no numerical noise tolerance support. It exists solely for testing the C implementation.
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
  if (anyNA(a) || anyNA(b))
    stop("NAs are not allowed as input")
  if (is.unsorted(a, strictly=TRUE))
    stop("'a' is not strictly increasing")
  if (is.unsorted(b, strictly=TRUE))
    stop("'b' is not strictly increasing")
  
  all_values <- sort(union(a, b))
  cum_available <- cumsum(all_values %in% b)
  cum_available[all_values %in% a]
}


#' Sorted Union
#' 
#' For two sorted numeric vectors \code{a} and \code{b}, determine the sorted union of unique elements.
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
#' sorted_union(c(), c())
sorted_union <- function(a, b, tolerance=0)
{
  # Argument checking
  if (is.na(tolerance))
    stop("'tolerance' is NA")
  if (tolerance < 0)
    stop("'tolerance' is negative")
  if (anyNA(a) || anyNA(b))
    stop("NAs are not allowed as input")
  if (is.unsorted(a))
    stop("'a' is not sorted")
  if (is.unsorted(b))
    stop("'b' is not sorted")
  
  # Call C-function
  # -) using C_num_leq_sorted gives unknown error "NULL value passed as symbol address" -> not used yet
  na <- length(a)
  nb <- length(b)
  res <- .C("sorted_union", as.double(a), na, as.double(b), nb,
    tolerance=as.double(tolerance), res=numeric(na + nb), length=integer(1L))
  #res <- .C(C_sorted_union, as.double(a), na, as.double(b), nb,
  #  tolerance=as.double(tolerance), res=numeric(na + nb), length=integer(1L))
  res$res[seq_len(res$length)]
}


#' R implementation of sorted_union
#'
#' This function is identical to \code{\link{sorted_union}} except that a) the input vectors don't need to be sorted, and b) there is no numerical noise tolerance support. It exists solely for testing the C implementation.
#'
#' @return A numeric vector.
#' @param a a sorted vector of numbers.
#' @param b a sorted vector of numbers.
#'
#' @keywords internal
#' @examples
#' sorted_union_R(1:3, 2:4)
sorted_union_R <- function(a, b)
{
  unique(sort(c(a, b)))
}

