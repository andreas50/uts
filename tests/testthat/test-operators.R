context("operators")

test_that("'Ops' group methods work",{
  # Argument checking
  expect_error(ex_uts() + 1:2)
  expect_error(1:2 + ex_uts())
  #
  expect_error("a" + ex_uts())
  expect_error(ex_uts() + "a")
  #
  expect_error(ex_uts2() + 1)
  expect_error(1 + ex_uts2())
  
  expect_identical(
    (ex_uts() + 20)$values,
    ex_uts()$values + 20
  )
  expect_identical(
    (20 + ex_uts())$values,
    20 + ex_uts()$values
  )
})


test_that("'Summary' group methods work",{
  expect_identical(
    min(ex_uts()),
    47
  )
  #' #any(ex_uts() > 45)
  expect_identical(
    range(ex_uts()),
    c(47.0, 48.5)
  )
})

test_that("'Math' group methods work",{
  expect_identical(
    floor(ex_uts())$values,
    c(48, 48, 48, 47, 47, 47)
  )
  expect_identical(
    cumsum(ex_uts())$values,
    c(48.375, 96.875, 145.25, 192.25, 239.75, 287.1)
  )
})
