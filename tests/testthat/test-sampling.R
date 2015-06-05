context("sampling")

test_that("Sampling of numeric time series works",{
  expect_equal(
    sample_values(ex_uts(), as.POSIXct(c("2007-01-01", "2011-01-01")), method="linear"),
    c(NA, 47.350)
  )
  expect_equal(
    sample_values(ex_uts(), as.POSIXct("2011-01-01"), method="linear", max_dt=dyears(1)),
    NA_real_
  )
  expect_equal(
    sample_values(ex_uts(), as.POSIXct(c("2007-11-09 12:01:00", "2007-11-09 15:16:00"))),
    c(47.50, 47.35)
  )
  expect_equal(
    sample_values(ex_uts(), as.POSIXct(c("2007-11-09 12:01:00", "2007-11-09 15:16:00")), method="linear"),
    c(47.42578125, 47.35)
  )
  
  # Sampling at times that are observation times
  expect_equal(
    sample_values(ex_uts(), ex_uts()$times),
    ex_uts()$values
  )
  expect_equal(
    sample_values(ex_uts(), ex_uts()$times, method="linear"),
    ex_uts()$values
  )
})


test_that("Sampling of non-numeric time series works",{
  expect_equal(
    sample_values(ex_uts2(), as.POSIXct("2007-01-01")),
    list(NULL)
  )
  expect_error(
    sample_values(ex_uts2(), as.POSIXct("2007-01-01"), method="linear"),
  )
  expect_equal(
    sample_values(ex_uts2(), as.POSIXct(c("2007-11-09 12:01:00", "2007-11-09 15:16:00"))),
    list(c("a", "B"), 3.1415)
  )
  
  # Sampling at times that are observation times
  expect_equal(
    sample_values(ex_uts2(), ex_uts2()$times),
    ex_uts2()$values
  )
})

