context("sampling")

test_that("Univariate sampling of values works",{
  expect_equal(
    sample_values(uts1, as.POSIXct(c("2007-01-01", "2011-01-01")), method="linear"),
    c(NA, 47.350)
  )
  expect_equal(
    sample_values(uts1, as.POSIXct("2011-01-01"), method="linear", max_lag=dyears(1)),
    as.numeric(NA)
  )
  expect_equal(
    sample_values(uts1, as.POSIXct(c("2007-11-09 12:01:00", "2007-11-09 15:16:00"))),
    c(47.50, 47.35)
  )
  expect_equal(
    sample_values(uts1, as.POSIXct(c("2007-11-09 12:01:00", "2007-11-09 15:16:00")), method="linear"),
    c(47.42578125, 47.35000)
  )
})

