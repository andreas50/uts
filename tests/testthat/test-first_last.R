context("first_last")

test_that("first works",{
  expect_equal(
    first(ex_uts()),
    ex_uts()$values[1]
  )
  expect_equal(
    first(ex_uts2()),
    ex_uts2()$values[[1]]
  )
  
  # Empty time series
  expect_equal(
    first(uts()),
    NULL
  )
})

test_that("last works",{
  expect_equal(
    last(ex_uts()),
    ex_uts()$values[length(ex_uts())]
  )
  expect_equal(
    last(ex_uts2()),
    ex_uts2()$values[[length(ex_uts2())]]
  )
  
  # Empty time series
  expect_equal(
    last(uts()),
    NULL
  )
})