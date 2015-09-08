context("summary of values")

test_that("summary works",{
  expect_identical(
    summary(ex_uts()),
    summary(ex_uts()$values)
  )
  expect_identical(
    summary(uts()),
    summary(uts()$values)
  )
})


test_that("sd works",{
  expect_identical(
    sd(ex_uts()),
    sd(ex_uts()$values)
  )
  expect_identical(
    sd(uts()),
    sd(uts()$values)
  )
})


test_that("mean works",{
  expect_identical(
    mean(ex_uts()),
    mean(ex_uts()$values)
  )
  expect_identical(
    mean(uts()),
    mean(uts()$values)
  )
})


test_that("median works",{
  expect_identical(
    median(ex_uts()),
    median(ex_uts()$values)
  )
  expect_identical(
    median(uts()),
    median(uts()$values)
  )
})

