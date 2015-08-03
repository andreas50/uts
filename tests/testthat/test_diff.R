context("diff")

test_that("diff works",{
  # Trivial case
  expect_equal(
    diff(ex_uts(), lag=0),
    uts(rep(0, length(ex_uts())), ex_uts()$times)
  )
  
  # Empty time series as results
  expect_equal(
    diff(ex_uts(), lag=length(ex_uts())),
    uts()
  )
  
  # Regression tests
  expect_equal_to_reference(diff(ex_uts()), file="test_diff_1.rds")
  expect_equal_to_reference(diff(ex_uts(), lag=-3), file="test_lag_minus_3.rds")
  expect_equal_to_reference(diff(ex_uts(), scale="log"), file="test_lag_log_scale.rds")
})


test_that("diff_t works",{
  # Trivial case
  expect_equal(
    lag_t(ex_uts(), ddays(0)),
    ex_uts()
  )
  
  # Empty time series as results
  expect_equal(
    length(diff_t(ex_uts(), by=as.duration(end(ex_uts()) - start(ex_uts())) + dseconds(1))),
    0
  )
  
  # Intentional error
  expect_error(
    diff_t(ex_uts(), 3)
  )
  
  # Regression tests
  expect_equal_to_reference(diff_t(ex_uts(), ddays(1)), file="test_diff_1_day.rds")
})