###############################
# Extensions for POSIXt dates #
###############################

# -----------------
# Generic functions
# -----------------

#' Next Business Day
#'
#' For objects representing dates, find the next business day.
#' 
#' Add the minimum number of days to reach a business day. The time-of-day information is therefore unaffected. In particular, if the input already is a business day, then the output is the same as the input.
#'
#' @return An object of the same class and length as the input \code{x}
#' @param x an object representing dates.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords chron
#' @seealso \code{\link{previous_business_day}}
next_business_day <- function(x, ...) UseMethod("next_business_day")


#' Previous Business Day
#' 
#' For objects representing dates, find the previous business day.
#'
#' Substract the minimum number of days to reach a business day. The time-of-day information is therefore unaffected. In particular, if the input already is a business day, then the output is the same as the input.
#'
#' @return An object of the same class and length as the input \code{x}
#' @param x an object representing dates.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords chron
#' @seealso \code{\link{next_business_day}}
previous_business_day <- function(x, ...) UseMethod("previous_business_day")


# ----------------------
# Method implementations
# ----------------------


#' @describeIn next_business_day find the next business day for POSIXt dates
#' 
#' @note For \code{next_business_day.POSIXt()}, non-business days are defined as Saturdays and Sundays. Public holidays are not supported.
#' 
#' @examples
#' # 2015-04-20 is a Monday
#' next_business_day(as.POSIXct("2015-04-20") + days(0:6))
next_business_day.POSIXt <- function(x, ...)
{
  day_of_week <- wday(x)
  x[day_of_week == 7] <- x[day_of_week == 7] + days(2)    # update Saturdays
  x[day_of_week == 1] <- x[day_of_week == 1] + days(1)    # update Sundays
  x
}


#' @describeIn previous_business_day find the previous business day for POSIXt dates
#' 
#' @note For \code{previous_business_day.POSIXt()}, non-business days are defined as Saturdays and Sundays. Public holidays are not supported.
#' 
#' @examples
#' # 2015-04-20 is a Monday
#' previous_business_day(as.POSIXct("2015-04-20") + days(0:6))
previous_business_day.POSIXt <- function(x, ...)
{
  day_of_week <- wday(x)
  x[day_of_week == 7] <- x[day_of_week == 7] - days(1)    # update saturdays
  x[day_of_week == 1] <- x[day_of_week == 1] - days(2)    # update sundays
  x
}
