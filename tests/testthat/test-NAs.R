context("NA")

test_that("na.omit works",{
  # Regression tests
  tmp <- ex_uts()
  tmp$values[c(2, 4)] <- NA
  expect_equal_to_reference(na.omit(tmp), file="test-NAs_1.rds")
  
  # Same time series returned if no NAs present
  expect_identical(
    na.omit(ex_uts()),
    ex_uts()
  )
  expect_identical(
    na.omit(uts()),
    uts()
  )
})


test_that("is.na works",{
  tmp <- ex_uts()
  tmp$values[c(2, 4)] <- NA
  expect_identical(
    which(is.na(tmp)),
    tmp$times[c(2, 4)]
  )
  
  # Empty POSIXct object returned if no NAs present
  expect_identical(
    which(is.na(ex_uts())),
    as.POSIXct(character(), tz="America/New_York")
  )
  expect_identical(
    which(is.na(uts())),
    as.POSIXct(character())
  )
})
