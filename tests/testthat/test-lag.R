context("lag")

test_that("lag works",{
  # Trivial cases
  expect_identical(
    lag(ex_uts(), k=6),
    uts()
  )
  expect_identical(
    lag(ex_uts(), k=Inf),
    uts()
  )
  expect_identical(
    lag(ex_uts(), k=-Inf),
    uts()
  )
  
  # Regression tests
  expect_equal_to_reference(lag(ex_uts(), k=1), file="test-lag_1.rds")
  expect_equal_to_reference(lag(ex_uts(), k=-2), file="test-lag_minus_2.rds")
  
  # Intentional error
  expect_error(lag(ex_uts(), ddays(1)))
})


test_that("lag_t works",{
  # Trivial cases
  expect_identical(
    lag_t(ex_uts(), ddays(0)),
    ex_uts()
  )
  expect_error(lag_t(ex_uts(), ddays(Inf)))
  
  # Intentional error
  expect_error(
    lag_t(ex_uts(), 3)
  )
  
  # Regression tests
  expect_equal_to_reference(lag_t(ex_uts(), dhours(7.5)), file="test-lag_t.rds")
})