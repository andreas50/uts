context("coercions")

test_that("coercions to 'uts' work",{
  # quarterly ts
  ts1 <- ts(1:10, frequency = 4, start = c(1959, 2))
  expect_equal_to_reference(as.uts(ts1), file="test-coercions_from_ts_1.rds")
 
  # monthly "ts"
  ts2 <- ts(1:10, frequency = 12, start = c(1959, 8))
  expect_equal_to_reference(as.uts(ts2), file="test-coercions_from_ts_2.rds")
 
  # Convert a yearly 'ts"
  ts3 <- ts(1:10, frequency = 1, start = 1959)
  expect_equal_to_reference(as.uts(ts3), file="test-coercions_from_ts_3.rds")
  
  # fts
  if (requireNamespace("fts", quietly = TRUE)) {
    fts1 <- fts::fts(index=as.POSIXct("2016-01-01") + dhours(c(1, 4, 27)), data=c(5,4,7))
    expect_equal_to_reference(as.uts(fts1), file="test-coercions_from_fts.rds")
  }
  
  # irts
  if (requireNamespace("tseries", quietly = TRUE)) {
    irts1 <- tseries::irts(as.POSIXct("2015-01-01") + days(c(1, 3, 7, 9)), 1:4)
    expect_equal_to_reference(as.uts(irts1), file="test-coercions_from_irts.rds")
  }
  
  # xts
  if (requireNamespace("xts", quietly = TRUE)) {
    xts1 <- xts::xts(1:4, as.Date("2015-01-01") + c(1, 3, 7, 9))
    expect_equal_to_reference(as.uts(xts1),file="test-coercions_from_xts.rds")
  }
  
  # zoo
  if (requireNamespace("zoo", quietly = TRUE)) {
    zoo1 <- zoo::zoo(1:4, as.Date("2015-01-01") + c(1, 3, 7, 9))
    expect_equal_to_reference(as.uts(zoo1), file="test-coercions_from_zoo.rds")
  }
})



test_that("coercions from 'uts' with atomic observations works",{
  # ts
  expect_error(as.ts(ex_uts()))
  
  # fts
  if (requireNamespace("fts", quietly = TRUE)) {
    expect_equal_to_reference(fts::as.fts(ex_uts()), file="test-coercions_to_fts.rds")
  }
  
  # irts
  if (requireNamespace("tseries", quietly = TRUE)) {
    expect_equal_to_reference(tseries::as.irts(ex_uts()), file="test-coercions_to_irts.rds")
  }
  
  # xts
  if (requireNamespace("xts", quietly = TRUE)) {
    expect_equal_to_reference(xts::as.xts(ex_uts()), file="test-coercions_to_xts.rds")
  }
  
  # zoo
  if (requireNamespace("zoo", quietly = TRUE)) {
     expect_equal_to_reference(zoo::as.zoo(ex_uts()), file="test-coercions_to_zoo.rds")
  }
})


test_that("time series with non-atomic observations are not converted from 'uts'",{
  # fts: the conversion actually works
  
  # irts
  if (requireNamespace("tseries", quietly = TRUE)) {
    expect_error(tseries::as.irts(ex_uts2()))
  }
  
  # xts
  if (requireNamespace("xts", quietly = TRUE)) {
    expect_error(xts::as.xts(ex_uts2()))
  }
  
  # zoo
  if (requireNamespace("zoo", quietly = TRUE)) {
     expect_error(zoo::as.zoo(ex_uts2()))
  }
})

  
test_that("multivariate time series are not converted",{
  # ts
  z <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 1), frequency = 12)
  expect_error(as.uts(z))
  
  # fts
  if (requireNamespace("fts", quietly = TRUE)) {
    x <- fts::fts(index=seq(from=Sys.Date(),by="months",length.out=24),data=1:24)
    y <- fts::fts(index=seq(from=Sys.Date(),by="months",length.out=12),data=13:24)
    cxy <- cbind(x, y)
    expect_error(as.uts(cxy))
  }
  
  # irts
  if (requireNamespace("tseries", quietly = TRUE)) {
    times <- cumsum(rexp(10, rate = 0.1))
    values <- matrix(rnorm(20), nrow=10)
    x <- tseries::irts(times, values)
    expect_error(as.uts(x))
 } 
  
  # xts
  if (requireNamespace("xts", quietly = TRUE)) {
    x2 <- xts::xts(matrix(1:12, 4, 3), as.Date("2003-01-01") + 0:3)
    expect_error(as.uts(x2))
  }
  
  # zoo
  if (requireNamespace("zoo", quietly = TRUE)) {
    z2 <- zoo::zoo(matrix(1:12, 4, 3), as.Date("2003-01-01") + 0:3)
    expect_error(as.uts(z2))
  }
})

