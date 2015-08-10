#######################
# Example time series #
#######################

#' Example Time Series
#' 
#' Create time series that can be used for code examples and testing.
#' 
#' @return \code{ex_ust()} returns a numeric \code{"uts"} with six observations.
#' 
#' @examples
#' ex_uts()
ex_uts <- function()
{
  dates <- c("2007-11-08", "2007-11-08", "2007-11-08", "2007-11-09", "2007-11-09", "2007-11-09")
  times <- c("7:00:00", "8:01:00", "13:15:00", "7:30:00", "8:51:00", "15:15:00")
  uts(values=c(48.375, 48.5, 48.375, 47, 47.5, 47.35), times=paste(dates, times))
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
    times = c("2007-11-08 1:01:00", "2007-11-09 7:30:00", "2007-11-09 15:16:00")
  )
}


#' Mauna Loa Atmospheric CO2 Concentration
#' 
#' This function downloads the monthly average atmospheric CO2 concentration (in parts per million), as measured at the Mauna Loa observatory, from the NOAA website. Please see \url{http://www.esrl.noaa.gov/gmd/ccgg/trends/} for a detailed description. The downloaded data is subsequently imported into \R and returned as a \code{\link{uts}} object. 
#' 
#' @param file the download location of the CO2 dataset.
#' 
#' @keywords datasets
#' @seealso The \code{\link[datasets:co2]{co2}} dataset in base \R is very similar, but ends in 1997 and has several missing values filled in using linear interpolation.
#' @examples
#' co2 <- download_co2()
#' plot(co2)
download_co2 <- function(file="ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt")
{
  # Download data into temporary file
  cat(paste0("Downloading the Mauna Loa CO2 data from ", file, "\n"))
  cat("Please see www.esrl.noaa.gov/gmd/ccgg/trends/ for a detailed description.\n")
  tmp_file <- tempfile()
  on.exit(unlink(tmp_file))
  download.file(file, destfile=tmp_file, quiet=TRUE)
  
  # Import data & remove test section
  data <- scan(tmp_file, what="character", quiet=TRUE, sep="\n")
  start_pos <- grep("^1958", data)[1]
  data <- data[start_pos:length(data)]
  data_split <- sapply(data, strsplit, " +", USE.NAMES=FALSE)
  
  # Extract dates and move to middle of month
  year <- sapply(data_split, function(x) x[1])
  month <- sapply(data_split, function(x) x[2])
  start_of_month <- ISOdate(year, month, 1, hour=0, tz="HST")
  end_of_month <- ceiling_date(start_of_month + dseconds(1), unit="month")
  mid_month <- start_of_month + difftime(start_of_month, end_of_month) / 2
  
  # Extract observation values and return "uts" object
  values <- as.numeric(sapply(data_split, function(x) x[4]))
  values[values < 0] <- NA
  co2 <- uts(values, mid_month)
  na.omit(co2)
}
