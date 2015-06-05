#######################
# Example time series #
#######################

#' Example Time Series
#' 
#' Create time series that can be used for code examples and testing.
#' 
#' @return \code{ex_ust()} returns a numeric \code{\link{uts}} with six observations.
#' 
#' @examples
#' ex_uts()
#' 
ex_uts <- function()
{
  dates <- c("2007-11-08", "2007-11-08", "2007-11-08", "2007-11-09", "2007-11-09", "2007-11-09")
  times <- c("7:00:00", "8:01:00", "13:15:00", "7:30:00", "8:51:00", "15:15:00")
  uts(values=c(48.375, 48.5, 48.375, 47, 47.5, 47.35), times=paste(dates, times))
}


#' @rdname ex_uts
#' 
#' @return \code{ex_uts2()} returns a non-numeric \code{\link{uts}} with three observations.
#' 
#' @examples
#' ex_uts2()
#' 
ex_uts2 <- function()
{
  uts(
    values = list(1:5, c("a", "B"), 3.1415),
    times = c("2007-11-08 1:01:00", "2007-11-09 7:30:00", "2007-11-09 15:16:00")
  )
}

