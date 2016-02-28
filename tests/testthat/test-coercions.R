context("coercions")

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

  # its
  if (requireNamespace("its", quietly = TRUE)) {
    mat <- structure(1:6, dim=c(2,3), dimnames=list(c("2003-01-01","2003-01-04"), letters[1:3]))
    its1 <- its::its(mat)
    expect_error(as.uts(its1))
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

