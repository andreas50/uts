context("first_last")

test_that("first.default works",{
  expect_identical(
    first(5:10),
    5L
  )
  expect_identical(
    first(list(a="test", b=3)),
    "test"
  )
  
  # Empty array, list
  expect_identical(
    first(c()),
    NULL
  )
  expect_identical(
    first(list()),
    NULL
  )
})


test_that("last.default works",{
  expect_identical(
    last(5:10),
    10L
  )
  expect_identical(
    last(list(a="test", b=3)),
    3
  )
  
  # Empty array, list
  expect_identical(
    last(c()),
    NULL
  )
  expect_identical(
    last(list()),
    NULL
  )
})


test_that("first.uts works",{
  expect_identical(
    first(ex_uts()),
    ex_uts()$values[1]
  )
  expect_identical(
    first(ex_uts2()),
    ex_uts2()$values[[1]]
  )
  
  # Empty time series
  expect_identical(
    first(uts()),
    NULL
  )
})

test_that("last.uts works",{
  expect_identical(
    last(ex_uts()),
    ex_uts()$values[length(ex_uts())]
  )
  expect_identical(
    last(ex_uts2()),
    ex_uts2()$values[[length(ex_uts2())]]
  )
  
  # Empty time series
  expect_identical(
    last(uts()),
    NULL
  )
})