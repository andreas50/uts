context("length")

test_that("length works",{
  expect_identical(
    length(ex_uts()),
    length(ex_uts()$values)
  )
  expect_identical(
    length(uts()),
    0L
  )
})


test_that("length_t works",{
  expect_identical(
    length_t(ex_uts()),
    as.duration(diff(range(ex_uts()$times)))
  )
  expect_identical(
    length_t(uts()),
    as.duration(NA)
  )
})
