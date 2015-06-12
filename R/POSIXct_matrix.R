##############################
# Matrices of POSIXt objects #
##############################

# -----------------
# Generic functions
# -----------------

#' Matrix of POSIXct Objects
#' 
#' Create a matrix of \code{POSIXct} objects.
#'
#' @param x an \R{} object of appropriate type.
#' @param \dots arguments passed to or from methods.
#' 
#' @keywords chron classes
#' @seealso \code{\link{POSIXct_vector}}
POSIXct_matrix <- function(x, ...) UseMethod("POSIXct_matrix")


#' Coerce to POSIXct_matrix
#' 
#' Generic function to coerce an object to a POSIXct_matrix.
#'
#' See documentation on method functions for further details.
#'
#' @param x an \R{} object of appropriate type.
#' @param \dots arguments passed to or from methods.
#' @seealso \code{\link{POSIXct_matrix}}
as.POSIXct_matrix <- function(x, ...) UseMethod("as.POSIXct_matrix")


# ----------------------
# Method implementations
# ----------------------

#' @describeIn POSIXct_matrix create a matrix of \code{POSIXct} objects out of a \code{POSIXct_vector}
#' 
#' @param nrow coming soon
#' @param ncol coming soon
#' @param byrow coming soon
#' @param dimnames coming soon
POSIXct_matrix.POSIXct_vector <- function(x, nrow=1, ncol=1, byrow=FALSE, dimnames=NULL, ...)
{
  # Determine matrix dimensions, and reycle arguments if necessary
  if (missing(nrow)) 
    nrow <- ceiling(length(x)/ncol)
  else if (missing(ncol)) 
    ncol <- ceiling(length(x)/nrow)
  num_el <- nrow * ncol
  num_ts <- length(x)
  if (num_el %% num_ts != 0)
    stop("Number of provided time series incompatible with matrix dimensions")
  mult <- num_el / num_ts
  x <- rep(x, mult)
  
  # Re-shuffle elements if byrow=T
  if (byrow) {
    new_pos <- rep(1:nrow, each=ncol) + (rep(1:ncol, nrow) - 1) * nrow
    x[new_pos] <- x
  }
  
  # Define class attributes
  class(x) <- c("POSIXct_matrix", class(x))
  dim(x) <- c(nrow, ncol)
  dimnames(x) <- dimnames
  x
}
if (0) {
  pm1 <- POSIXct_matrix(pv1, 3, 2, byrow=TRUE)
  rownames(pm1) <- c("a", "b", "c")
  colnames(pm1) <- c("A", "B")
}


# Convert POSIXct_vector to POSIXct_matrix
# -) wrapper around "POSIXct_matrix.POSIXct_vector"
as.POSIXct_matrix.POSIXct_vector <- function(x, ...)
{
  out <- POSIXct_matrix.POSIXct_vector(x)
  rownames(out) <- names(x)
  out
}
if (0) {
  as.POSIXct_matrix(pv1)   # RT
}


# Constructor from single POSIXct object (object repeated)
POSIXct_matrix.POSIXct <- function(x, nrow=1, ncol=1, dimnames=NULL, ...)
{
  #chv <- rep(x, nrow*ncol, type="POSIXct_vector")
  tmp <- rep(list(x), nrow*ncol)
  chv <- do.call(POSIXct_vector, tmp)
  POSIXct_matrix(chv, nrow=nrow, ncol=ncol, dimnames=dimnames)
}
if (0) {
  POSIXct_matrix(Sys.time(), 3, 2, dimnames=list(c("a", "b", "c")))   # RT
} 


# Print function
print.POSIXct_matrix <- function(x, ...)
{
  # x         ... object of class gits_matrix to be printed
  
  cat("------------------\n")
  cat(paste(class(x)[1], "object\n"))
  cat("------------------\n")
  
  # Determine length and class of each matrix element
  num_el <- prod(dim(x))
  num_els <- sapply(x, length)
  start_times <- do.call("c", lapply(x, function(x) x[1]))
  end_times <- do.call("c", lapply(x, function(x) x[length(x)]))
  
  # Generate matrix with summary statistics
  brackets <- num_el
  brackets <- paste(num_els, start_times, end_times, sep=", ")
  out <- matrix(paste0("POSIXct[", brackets, "]"), nrow(x), ncol(x))
  dimnames(out) <- dimnames(x)
  print(out, quote=FALSE)
}
if (0) {
  pm1
}


#' Check if Object is POSIXct_matrix
#' 
#' Return \code{TRUE} if \code{x} is an object of class \code{"POSIXct_matrix"}.
#' 
#' @param x an \R object.
is.POSIXct_matrix <- function(x)
{
  inherits(x, "POSIXct_matrix")
}
if (0) {
  is.POSIXct_matrix(pm1)
}
