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
#' @param \dots further arguments passed to or from methods.
#' @seealso \code{\link[lubridate:ceiling_date]{ceiling_date}}
#'
quarterend <- function(x, ...) UseMethod("quarterend")


#' Next Business Day
#'
#' Generic function to find the next business day of objects representing dates.
#' 
#' See documentation on method functions for further details.
#'
#' @param x an object representing dates.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{previous_business_day}}
#'
next_business_day <- function(x, ...) UseMethod("next_business_day")


#' Previous Business Day
#' 
#' Generic function to find the previous business day of objects representing dates.
#'
#' See documentation on method functions for further details.
#'
#' @param x an object representing dates.
#' @param \dots further arguments passed to or from methods.
#' 
#' @seealso \code{\link{next_business_day}}
#'
previous_business_day <- function(x, ...) UseMethod("previous_business_day")


# ----------------------
# Method implementations
# ----------------------

#' Quarter End
#' 
#' For a POSIXt date-time, return the last day of the corresponding quarter.
#' 
#' This function is needed, because \code{\link[lubridate:ceiling_date]{ceiling_date}} does not have support for \code{unit="quarter"}.
#' 
#' @return An object of the same class and length as the input \code{x}.
#' @param x a \code{POSIXt} date-tim object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @keywords chron
#' @seealso \code{\link[lubridate:ceiling_date]{ceiling_date}}
#'
#' @examples
#' # Get the quarter end for a date in 2015Q1 and 2015Q2 
#' quarterend(as.POSIXct("2015-01-05"))
#' quarterend(as.POSIXct("2015-04-15"))
#'
quarterend.POSIXt <- function(x, ...)
{
  new_months <- ceiling(month(x) / 3) * 3
  ceiling_date(update(x, months=new_months, days=28), unit="month") - days(1)
}
if (0) {
  tmp <- seq(as.POSIXct("2015-01-01"), length=12, by="month")
  month(tmp) <- month(tmp) + (-month(tmp)) %% 3                 # required calculation in ceiling_date() for unit="quarter" support
}


#' Next Business Day
#' 
#' For a POSIXt date-time object, add the minimum number of days to reach a business day. The time-of-day information is therefore unaffected. In particular, if the input already is a business day, then the output is the same as the input.
#'
#'
#' @return An object of the same class and length as the input \code{x}.
#' @param x a \code{POSIXt} date-time object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @note Non-business days are defined as Saturdays and Sundays. Public holidays are not supported.
#' @keywords chron
#' @seealso \code{\link{previous_business_day.POSIXt}}
#' 
#' @examples
#' # 2015-04-20 is a Monday
#' next_business_day(as.POSIXct("2015-04-20") + days(0:6))
#'
next_business_day.POSIXt <- function(x, ...)
{
  day_of_week <- wday(x)
  x[day_of_week == 7] <- x[day_of_week == 7] + days(2)    # update saturdays
  x[day_of_week == 1] <- x[day_of_week == 1] + days(1)    # update sundays
  x
}


#' Previous Business Day
#' 
#' For a POSIXt date-time object, substract the minimum number of days to reach a business day. The time-of-day information is therefore unaffected. In particular, if the input already is a business day, then the output is the same as the input.
#'
#' @return An object of the same class and length as the input \code{x}.
#' @param x a \code{POSIXt} date-time object.
#' @param \dots further arguments passed to or from methods.
#' 
#' @note Non-business days are defined as Saturdays and Sundays. Public holidays are not supported.
#' @keywords chron
#' @seealso \code{\link{next_business_day.POSIXt}}
#' 
#' @examples
#' # 2015-04-20 is a Monday
#' previous_business_day(as.POSIXct("2015-04-20") + days(0:6))
#'
previous_business_day.POSIXt <- function(x, ...)
{
  day_of_week <- wday(x)
  x[day_of_week == 7] <- x[day_of_week == 7] - days(1)    # update saturdays
  x[day_of_week == 1] <- x[day_of_week == 1] - days(2)    # update sundays
  x
}
