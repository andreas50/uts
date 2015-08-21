context("summary of values")

test_that("summary works",{
  expect_equal(
    summary(ex_uts()),
    summary(ex_uts()$values)
  )
  expect_equal(
    summary(uts()),
    summary(uts()$values)
  )
})


test_that("sd works",{
  expect_equal(
    sd(ex_uts()),
    sd(ex_uts()$values)
  )
  expect_equal(
    sd(uts()),
    sd(uts()$values)
  )
})


test_that("mean works",{
  expect_equal(
    mean(ex_uts()),
    mean(ex_uts()$values)
  )
  expect_equal(
    mean(uts()),
    mean(uts()$values)
  )
})


test_that("median works",{
  expect_equal(
    median(ex_uts()),
    median(ex_uts()$values)
  )
  expect_equal(
    median(uts()),
    median(uts()$values)
  )
})

