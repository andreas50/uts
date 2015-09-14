context("diff")

test_that("diff works",{
  # Trivial case
  expect_identical(
    diff(ex_uts(), lag=0),
    uts(rep(0, length(ex_uts())), ex_uts()$times)
  )
  
  # Empty time series as results
  expect_identical(
    diff(ex_uts(), lag=length(ex_uts())),
    uts()
  )
  expect_identical(
    diff(ex_uts(), lag=-Inf),
    uts()
  )
  
  # Regression tests
  expect_equal_to_reference(diff(ex_uts()), file="test-diff_1.rds")
  expect_equal_to_reference(diff(ex_uts(), lag=-3), file="test-lag_minus_3.rds")
  expect_equal_to_reference(diff(ex_uts(), scale="log"), file="test-lag_log_scale.rds")
  
  # Intentional error
  expect_error(diff(ex_uts(), ddays(1)))
})


test_that("diff_t works",{
  # Argument checking
  expect_error(diff_t(ex_uts(), 3))
  expect_error(diff_t(ex_uts(), ddays(Inf)))
  
  # Trivial case
  expect_identical(
    lag_t(ex_uts(), ddays(0)),
    ex_uts()
  )
  
  # Empty time series as results
  expect_identical(
    length(diff_t(ex_uts(), by=as.duration(end(ex_uts()) - start(ex_uts())) + dseconds(1))),
    0L
  )
  
  # Regression tests
  expect_equal_to_reference(diff_t(ex_uts(), ddays(1)), file="test-diff_1_day.rds")
})