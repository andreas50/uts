context("head_t and tail_t")

test_that("head_t works",{
  # Regression tests
  expect_equal_to_reference(head_t(ex_uts(), ddays(1)), file="test-head_t_1.rds")
  expect_equal_to_reference(head_t(ex_uts(), ddays(0)), file="test-head_t_2.rds")
  
  # Error, because window width missing
  expect_error(head_t(uts()))
  
  # Entire time series returned if window width equal to temporal length of time series
  expect_identical(
    head_t(ex_uts(), as.duration(end(ex_uts()) - start(ex_uts()))),
    ex_uts()
  )
  expect_identical(
    head_t(uts(), ddays(1)),
    uts()
  )
})


test_that("tail_t works",{
  # Regression tests
  expect_equal_to_reference(tail_t(ex_uts(), ddays(1)), file="test-tail_t_1.rds")
  expect_equal_to_reference(tail_t(ex_uts(), ddays(0)), file="test-tail_t_2.rds")

  # Error, because window width missing
  expect_error(tail_t(uts()))
  
  # Entire time series returned if window width equal to temporal length of time series
  expect_identical(
    tail_t(ex_uts(), as.duration(end(ex_uts()) - start(ex_uts()))),
    ex_uts()
  )
  expect_identical(
    tail_t(uts(), ddays(1)),
    uts()
  )
})
