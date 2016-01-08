#######################
# Example time series #
#######################

#' Example Time Series
#' 
#' Create time series that can be used for code examples and testing.
#' 
#' @return \code{ex_uts()} returns a numeric \code{"uts"} with six observations.
#' 
#' @examples
#' ex_uts()
ex_uts <- function()
{
  dates <- c("2007-11-08", "2007-11-08", "2007-11-08", "2007-11-09", "2007-11-09", "2007-11-09")
  times <- c("7:00:00", "8:01:00", "13:15:00", "7:30:00", "8:51:00", "15:15:00")
  times <- as.POSIXct(paste(dates, times), tz="America/New_York")
  uts(values=c(48.375, 48.5, 48.375, 47, 47.5, 47.35), times=times)
}


#' @rdname ex_uts
#' 
#' @return \code{ex_uts2()} returns a non-numeric \code{"uts"} with three observations.
#' 
#' @examples
#' ex_uts2()
ex_uts2 <- function()
{
  uts(
    values = list(1:5, c("a", "B"), 3.1415),
    times = as.POSIXct(c("2007-11-08 1:01:00", "2007-11-09 7:30:00", "2007-11-09 15:16:00"), tz="America/New_York")
  )
}


#' @rdname ex_uts
#' 
#' @return \code{ex_uts3()} returns a daily \code{"uts"} that starts in 1/1/2000. The observation values are drawn from a normal distribution with mean 0 and standard deviation 1. For reproducability, the random numbers are draw with fixed initial \code{\link[=set.seed]{seed}}.
#' @param n an integer, specifying the length of the desired time series.
#' 
#' @examples
#' plot(ex_uts3())
#' plot(cumsum(ex_uts3()))
ex_uts3 <- function(n=1000L)
{
  seed <- .Random.seed
  on.exit(set.seed(seed))
  
  set.seed(1L)
  uts(rnorm(n), as.POSIXct("2000-01-01") + days(1L:n))
}


