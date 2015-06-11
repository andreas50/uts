context("operators")

test_that("'Summary' group methods work",{
  expect_equal(
    min(ex_uts()),
    47
  )
  #' #any(ex_uts() > 45)
  expect_equal(
    range(ex_uts()),
    c(47.0, 48.5)
  )
})

test_that("'Math' group methods work",{
  expect_equal(
    floor(ex_uts())$values,
    c(48, 48, 48, 47, 47, 47)
  )
  expect_equal(
    cumsum(ex_uts())$values,
    c(48.375, 96.875, 145.25, 192.25, 239.75, 287.1)
  )
})
