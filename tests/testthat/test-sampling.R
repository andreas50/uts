context("sampling")

test_that("Univariate sampling of values works",{
  dts <- c("2007-11-08", "2007-11-08", "2007-11-08", "2007-11-09", "2007-11-09", "2007-11-09")
  tms <- c("7:00:00", "8:01:00", "13:15:00", "7:30:00", "8:51:00", "15:15:00")
  uts1 <- uts(values=c(48.375, 48.5, 48.375, 47, 47.5, 47.35), times=paste(dts, tms))
      
  expect_equal(
    sample_values(uts1, as.POSIXct(c("2007-01-01", "2011-01-01")), method="linear"),
    c(NA, 47.350)
  )
  expect_equal(
    sample_values(uts1, as.POSIXct("2011-01-01"), method="linear", max_dt=dyears(1)),
    NA_real_
  )
  expect_equal(
    sample_values(uts1, as.POSIXct(c("2007-11-09 12:01:00", "2007-11-09 15:16:00"))),
    c(47.50, 47.35)
  )
  expect_equal(
    sample_values(uts1, as.POSIXct(c("2007-11-09 12:01:00", "2007-11-09 15:16:00")), method="linear"),
    c(47.42578125, 47.35000)
  )
  
  # Sampling at time that is an observation time
  expect_equal(
    sample_values(uts1, uts1$times),
    uts1$values
  )
  expect_equal(
    sample_values(uts1, uts1$times, method="linear"),
    uts1$values
  )
})

