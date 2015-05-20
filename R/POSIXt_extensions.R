###############################
# Extensions for POSIXt dates #
###############################

# -----------------
# Generic functions
# -----------------

#' Quarter End
#'
#' Generic function to determine the quarter-end of objects representing dates.
#' 
#' See documentation on method functions for further details.
#'
#' 
#' @param x an object representing dates.
#' @param \dots arguments passed to or from methods.
#' @seealso \code{\link[lubridate:ceiling_date]{ceiling_date()}}
quarterend <- function(x, ...) UseMethod("quarterend")


#' Next Business Day
#'
#' Generic function to find the next business day of objects representing dates.
#' 
#' See documentation on method functions for further details.
#'
#' @param x an object representing dates.
#' @param \dots arguments passed to or from methods.
#' @seealso \code{\link{previous_business_day}}
next_business_day <- function(x, ...) UseMethod("next_business_day")


#' Previous Business Day
#' 
#' Generic function to find the previous business day of objects representing dates.
#'
#' See documentation on method functions for further details.
#'
#' 
#' @param x an object representing dates.
#' @param \dots arguments passed to or from methods.
#' @seealso \code{\link{next_business_day}}
previous_business_day <- function(x, ...) UseMethod("previous_business_day")


#
POSIXct_vector <- function(x, ...) UseMethod("POSIXct_vector")
POSIXct_matrix <- function(x, ...) UseMethod("POSIXct_matrix")
as.POSIXct_matrix <- function(x, ...) UseMethod("as.POSIXct_matrix")



# ----------------------
# Method implementations
# ----------------------


# Return last day of quarter (set intraday time to midnight)
##' This function is needed, because \code{\link[lubridate:ceiling_date]{ceiling_date()}} does not have support for unit="quarter".
quarterend.POSIXt <- function(x)
{
  new_months <- ceiling(month(x) / 3) * 3
  ceiling_date(update(x, months=new_months, days=1), unit="month")
}
if (0) {
  quarterend(as.POSIXct("2015-01-01") + days(c(0, 10, 30, 90, 180)))
  #
  tmp <- seq(as.POSIXct("2015-01-01"), length=12, by="month")
  month(tmp) <- month(tmp) + (-month(tmp)) %% 3                 # required calculation in ceiling_date() for unit="quarter" support
}


# Add minmum number of days >= 0 in order to reach weekday
next_business_day.POSIXt <- function(x)
{
  # x     ... an object of class "dates"
  
  day_of_week <- wday(x)
  x[day_of_week == 7] <- x[day_of_week == 7] + days(2)    # fix saturdays
  x[day_of_week == 1] <- x[day_of_week == 1] + days(1)    # fix sundays
  x
}
if (0) {
  next_business_day(as.POSIXct("2015-04-20") + days(0:6))     # 2015-04-20 is a Monday
} 


# Subtract minmum number of days >= 0 in order to reach weekday
previous_business_day.POSIXt <- function(x)
{
  # x     ... an object of class "dates"
  
  day_of_week <- wday(x)
  x[day_of_week == 7] <- x[day_of_week == 7] - days(1)    # fix saturdays
  x[day_of_week == 1] <- x[day_of_week == 1] - days(2)    # fix sundays
  x
}
if (0) {
  previous_business_day(as.POSIXct("2015-04-20") + days(0:6))     # 2015-04-20 is a Monday
}


##################
# POSIXct_vector #
##################

# Default constructor
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


##################
# POSIXct_matrix #
##################

# Constructor from POSIXct_VECTOR
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


# Check if object is a POSIXct_MATRIX
is.POSIXct_matrix <- function(x)
{
  inherits(x, "POSIXct_matrix")
}
if (0) {
  is.POSIXct_matrix(pm1)
}