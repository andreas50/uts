context("sampling")

test_that("sample_values argument checking works",{
  # For the sampling times
  expect_error(sample_values(ex_uts(), "abc"))
  expect_error(sample_values(ex_uts(), as.POSIXct(c("2012-01-01", "2011-01-01"))))
  
  # For max_dt
  expect_error(sample_values(ex_uts(), as.POSIXct(c("2007-01-01", "2011-01-01")), max_dt=5))
  expect_error(sample_values(ex_uts(), as.POSIXct(c("2007-01-01", "2011-01-01")), max_dt=ddays(-5)))
  
  # For the method
  expect_error(sample_values(ex_uts(), as.POSIXct(c("2007-01-01", "2011-01-01")), method="abc"))
  expect_error(sample_values(ex_uts2(), as.POSIXct("2007-01-01"), method="linear"))
})

  
test_that("Sampling of numeric time series works",{
  expect_identical(
    sample_values(ex_uts(), as.POSIXct(c("2007-01-01", "2011-01-01")), method="linear"),
    c(NA, 47.350)
  )
  expect_identical(
    sample_values(ex_uts(), as.POSIXct("2011-01-01"), method="linear", max_dt=dyears(1)),
    NA_real_
  )
  expect_identical(
    sample_values(ex_uts(), as.POSIXct(c("2007-11-09 12:01:00", "2007-11-09 15:16:00"))),
    c(47.50, 47.35)
  )
  expect_identical(
    sample_values(ex_uts(), as.POSIXct(c("2007-11-09 12:01:00", "2007-11-09 15:16:00")), method="linear"),
    c(47.42578125, 47.35)
  )
  
  # Sampling at times that are observation times
  expect_identical(
    sample_values(ex_uts(), ex_uts()$times),
    ex_uts()$values
  )
  expect_identical(
    sample_values(ex_uts(), ex_uts()$times, method="linear"),
    ex_uts()$values
  )
})


test_that("Sampling of non-numeric time series works",{
  expect_identical(
    sample_values(ex_uts2(), as.POSIXct("2007-01-01")),
    list(NULL)
  )
  expect_identical(
    sample_values(ex_uts2(), as.POSIXct(c("2007-11-09 12:01:00", "2007-11-09 15:16:00"))),
    list(c("a", "B"), 3.1415)
  )
  
  # Sampling at times that are observation times
  expect_identical(
    sample_values(ex_uts2(), ex_uts2()$times),
    ex_uts2()$values
  )
})


test_that("[ and [<- argument checking works",{
  ts <- ex_uts()
  expect_error(ts[ts])
  expect_error(ts[ts] <- 5)
})


