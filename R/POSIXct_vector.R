#############################
# Vectors of POSIXt objects #
#############################

# -----------------
# Generic functions
# -----------------

#' Vector of POSIXct Objects
#' 
#' Create a vector of \code{POSIXct} objects.
#'
#' @return an object of class \code{"POSIXct_vector"}
#' @param x an \R{} object of appropriate type.
#' @param \dots arguments passed to or from methods.
#' 
#' @keywords chron classes
#' @seealso \code{\link{POSIXct_matrix}}
POSIXct_vector <- function(x, ...) UseMethod("POSIXct_vector")


# ----------------------
# Method implementations
# ----------------------

#' @describeIn POSIXct_vector default constructor
POSIXct_vector.default <- function(...)
{
  # Allocate object for output
  args <- list(...)
  num_el <- 0
  out <- list()
  class(out) <- c("POSIXct_vector", "list")
  
  # Insert individual elements
  for (j in seq(along=args)) {
    obj <- args[[j]]
    if (is.POSIXct(obj)) {
      # Add POSIXct object
      num_el <- num_el + 1
      if (length(names(args)[j]) > 0)   # Extract name of POSIXct object
        el_name <- names(args)[j]
      else
        el_name <- num_el
      out[[el_name]] <- obj
    } else if (is.POSIXct_vector(obj)) {
      # Add POSIXct_VECTOR object
      for (k in seq(along=obj)) {
        num_el <- num_el + 1
        if (length(names(obj)[k]) > 0)  # Extract name of POSIXct_VECTOR[[k]]
          el_name <- names(obj)[k]
        else
          el_name <- num_el
        out[[el_name]] <- obj[[k]]
      }
    } else
      stop("Not all inputs are of type POSIXct or POSIXct_vector.")
  }
  out
}
if (0) {
  pv1 <- POSIXct_vector(a=seq(as.POSIXct("2015-01-01"), length=10, by="quarter"),
      b=seq(as.POSIXct("2015-01-01"), length=5, by="year")) 
  POSIXct_vector()                                                  # RT
  POSIXct_vector(pv1, as.POSIXct(c("2013-01-01", "2013-01-02")))    # RT
  POSIXct_vector(5)                                                 # RT error
}


# Check if object "is a" POSIXct_vector
is.POSIXct_vector <- function(x)
{
  inherits(x, "POSIXct_vector")
}
if (0) {
  is.POSIXct_vector(pv1) 
}


# Print function
print.POSIXct_vector <- function(x, ...)
{
  cat("---------------------\n")
  cat(paste(class(x)[1], "object\n"))
  cat("---------------------\n")
  
  # Extract time series stats
  num_el <- length(x)
  if (num_el == 0) {
    cat("No POSIXct object available at this time\n")
    return()
  }
  Length <- sapply(x, length)
  start_times <- do.call("c", lapply(x, function(x) x[1]))
  end_times <- do.call("c", lapply(x, function(x) x[length(x)]))
  Name <- names(x)
  if (length(Name) < num_el)
    Name <- rep(NA, num_el)
  stats <- data.frame(Name, Length, start_times, end_times)
  rownames(stats) <- 1:num_el
  
  # Print nice description
  cat("\nIndividual POSIXct object characteristics:\n")
  cat("------------------------------------------\n")
  print(stats)
}
if (0) {
  pv1
}
