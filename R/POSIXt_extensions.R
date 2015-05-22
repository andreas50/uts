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
#' @param x an object representing dates.
#' @param \dots arguments passed to or from methods.
#' @seealso \code{\link{next_business_day}}
previous_business_day <- function(x, ...) UseMethod("previous_business_day")


# ----------------------
# Method implementations
# ----------------------

#' Quarter End
#' 
#' For a POSIXt date, return the last day of the corresponding quarter.
#' 
#' This function is needed, because \code{\link[lubridate:ceiling_date]{ceiling_date()}} does not have support for unit="quarter".
#' 
#' @param x a \code{POSIXt} date object.
#' @seealso \code{\link[lubridate:ceiling_date]{ceiling_date()}}
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


#' Next Business Day
#' 
#' For a POSIXt date, get the next business day on or after such date. The time-of-day information is unaffected. In particular, if the input already is a business day, then the output is the same as the input.
#' 
#' This function is needed, because \code{\link[lubridate:ceiling_date]{ceiling_date()}} does not have support for unit="quarter".
#' 
#' @param x a \code{POSIXt} date object.
#' @seealso \code{\link[lubridate:ceiling_date]{ceiling_date()}}
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
