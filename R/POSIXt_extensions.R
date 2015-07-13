###############################
# Extensions for POSIXt dates #
###############################

# -----------------
# Generic functions
# -----------------

#' Next Weekday
#'
#' For objects representing dates, find the next weekday.
#' 
#' Add the minimum number of days to reach a weekday (Monday-Friday). The time-of-day information is therefore unaffected. In particular, if the input already is a weekday, then the output is the same as the input.
#'
#' @return An object of the same class and length as the input \code{x}
#' @param x an object representing dates.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords chron
#' @seealso \code{\link{previous_weekday}}
next_weekday <- function(x, ...) UseMethod("next_weekday")


#' Previous Weekday
#' 
#' For objects representing dates, find the previous weekday.
#'
#' Substract the minimum number of days to reach a weekday (Monday-Friday). The time-of-day information is therefore unaffected. In particular, if the input already is a weekday, then the output is the same as the input.
#'
#' @return An object of the same class and length as the input \code{x}
#' @param x an object representing dates.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords chron
#' @seealso \code{\link{next_weekday}}
previous_weekday <- function(x, ...) UseMethod("previous_weekday")


# ----------------------
# Method implementations
# ----------------------


#' @describeIn next_weekday find the next weekday for POSIXt dates
#' 
#' @examples
#' # 2015-04-20 is a Monday
#' next_weekday(as.POSIXct("2015-04-20") + days(0:6))
next_weekday.POSIXt <- function(x, ...)
{
  day_of_week <- wday(x)
  x[day_of_week == 7] <- x[day_of_week == 7] + days(2)    # update Saturdays
  x[day_of_week == 1] <- x[day_of_week == 1] + days(1)    # update Sundays
  x
}


#' @describeIn previous_weekday find the previous weekday for POSIXt dates
#' 
#' @examples
#' # 2015-04-20 is a Monday
#' previous_weekday(as.POSIXct("2015-04-20") + days(0:6))
previous_weekday.POSIXt <- function(x, ...)
{
  day_of_week <- wday(x)
  x[day_of_week == 7] <- x[day_of_week == 7] - days(1)    # update saturdays
  x[day_of_week == 1] <- x[day_of_week == 1] - days(2)    # update sundays
  x
}
