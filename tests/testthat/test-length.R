context("length")

test_that("length works",{
  expect_equal(
    length(ex_uts()),
    length(ex_uts()$values)
  )
  expect_equal(
    length(uts()),
    0
  )
})


test_that("length_t works",{
  expect_equal(
    length_t(ex_uts()),
    diff(range(ex_uts()$times))
  )
  expect_equal(
    length_t(uts()),
    as.difftime(NA_real_, units="secs")
  )
})
