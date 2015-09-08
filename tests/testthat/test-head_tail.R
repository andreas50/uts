context("head and tail")

test_that("head works",{
  # Regression tests
  expect_equal_to_reference(head(ex_uts()), file="test-head_1.rds")
  expect_equal_to_reference(head(ex_uts(), 2), file="test-head_2.rds")
  expect_equal_to_reference(head(ex_uts(), -2), file="test-head_3.rds")
  expect_equal_to_reference(head(ex_uts(), -6), file="test-head_4.rds")
  

  # Entire time series returned if requested length >= time series length
  expect_identical(
    head(ex_uts(), length(ex_uts())),
    ex_uts()
  )
})


test_that("tail works",{
  # Regression tests
  expect_equal_to_reference(tail(ex_uts()), file="test-tail_1.rds")
  expect_equal_to_reference(tail(ex_uts(), 2), file="test-tail_2.rds")
  expect_equal_to_reference(tail(ex_uts(), -2), file="test-tail_3.rds")
  expect_equal_to_reference(tail(ex_uts(), -6), file="test-tail_4.rds")
  
  
  # Entire time series returned if requested length >= time series length
  expect_identical(
    tail(ex_uts(), length(ex_uts())),
    ex_uts()
  )
})
