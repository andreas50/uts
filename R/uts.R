# Define generic functions
as_daily <- function(x, ...) UseMethod("as_daily")
as_weekly <- function(x, ...) UseMethod("as_weekly")
as_monthly <- function(x, ...) UseMethod("as_monthly")
as_quarterly <- function(x, ...) UseMethod("as_quarterly")
as_yearly <- function(x, ...) UseMethod("as_yearly")
#
alltimes <- function(x, ...) UseMethod("alltimes")
end_time <- function(x, ...) UseMethod("end_time")
start_time <- function(x, ...) UseMethod("start_time")
most_recent <- function(x, ...) UseMethod("most_recent")
time_points <- function(x, ...) UseMethod("time_points")
#
concatenate <- function(x, ...) UseMethod("concatenate")
cross_sample <- function(x, ...) UseMethod("cross_sample")
first <- function(x, ...) UseMethod("first")
interesting_freqs <- function(x, ...) UseMethod("interesting_freqs")
intra_month_pos <- function(x, ...) UseMethod("intra_month_pos")
intra_week_pos <- function(x, ...) UseMethod("intra_week_pos")
flatten <- function(x, ...) UseMethod("flatten")
lag_t <- function(x, ...) UseMethod("lag_t")
last <- function(x, ...) UseMethod("last")
num_obs <- function(x, ...) UseMethod("num_obs")
rm_initial_NAs <- function(x, ...) UseMethod("rm_initial_NAs")
rm_initial_ticks <- function(x, ...) UseMethod("rm_initial_ticks")
rm_NAs <- function(x, ...) UseMethod("rm_NAs")
rm_ticks <- function(x, ...) UseMethod("rm_ticks")
shift <- function(x, ...) UseMethod("shift")
sparse <- function(x, ...) UseMethod("sparse")
subperiod <- function(x, ...) UseMethod("subperiod")
subsample <- function(x, ...) UseMethod("subsample")
validate <- function(x, ...) UseMethod("validate")


### START Constructors

# Constructor of GITS (generalized intraday time series)
gits <<- function(values=c(), times=chron(), meta=NULL, check_chron_class=TRUE)
{
  # values            ... list (not array!) of objects to be stored
  # times             ... corresponding time points of stored objects
  # meta              ... optional meta-deta (e.g. Cusip, time series name, currency, coupon, maturity, ...)
  # check_chron_class ... whether to check if 'times' is of class 'chron', or conversion necessary
  #                       not checking gives a speed-up for frequent calls
  
  #values <- as.list(values)    # move back in?
  if (!check_chron_class)
    x <- list(values=values, times=times, meta=meta)
  else if (is.dates(times))
    x <- list(values=values, times=times, meta=meta)
  else
    x <- list(values=values, times=chron(times), meta=meta)
  class(x) <- c("gits", "list")
  x   
}
if (1) {
  # RT: constructor
  gits1 <- gits(list(1:5, c("a", "B")), chron(c("11/8/2007", "11/9/2007"), c("1:01:00", "15:16:00")),
      meta=list(name="test", cusip=""))
  #
  gits2 <- gits(list("test", 1:5, list(a=1, b=2, c=3)), chron(c("11/8/2007", "11/9/2007", "11/10/2007"),
          c("1:01:00", "15:16:00", "16:15:00")))
  # RT conversion
  gits(1:5, chron("1/1/2010") - 0:4)$values
}
