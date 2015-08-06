context("lag")

test_that("lag works",{
  # Trivial case
  expect_equal(
    lag(ex_uts(), k=6),
    uts()
  )
  
  # Regression tests
  expect_equal_to_reference(lag(ex_uts(), k=1), file="test-lag_1.rds")
  expect_equal_to_reference(lag(ex_uts(), k=-2), file="test-lag_minus_2.rds")
})


test_that("lag_t works",{
  # Trivial case
  expect_equal(
    lag_t(ex_uts(), ddays(0)),
    ex_uts()
  )
  
  # Intentional error
  expect_error(
    lag_t(ex_uts(), 3)
  )
  
  # Regression tests
  expect_equal_to_reference(lag_t(ex_uts(), dhours(7.5)), file="test-lag_t.rds")
})