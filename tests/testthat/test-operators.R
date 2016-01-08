context("group methods")

### Ops ###

test_that("Ops argument checking works",{
  expect_error(ex_uts() + 1:2)
  expect_error("a" + ex_uts())
  expect_error(ex_uts2() + 1)
})


test_that("Ops group methods work",{
  # Numeric uts
  expect_identical(
    (ex_uts() + 20)$values,
    ex_uts()$values + 20
  )
  expect_identical(
    (20 - ex_uts())$values,
    20 - ex_uts()$values
  )
  
  # Logical operations
  ts <- (ex_uts() >= 48)
  expect_identical(
    (ts & TRUE)$values,
    ts$values
  )
  expect_identical(
    (ts & FALSE)$values,
    rep(FALSE, length(ts))
  )
  expect_identical(
    ts & ts,
    ts
  )
  
  # Comparisons
  expect_identical(
    (ex_uts() >= ex_uts())$values,
    rep(TRUE, length(ex_uts()))
  )
  expect_identical(
    (ex_uts() > max(ex_uts()))$values,
    rep(FALSE, length(ex_uts()))
  )
})


### Summary ###

test_that("Summary group methods work",{
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


### Math ###

test_that("Math group methods work",{
  expect_identical(
    floor(ex_uts())$values,
    c(48, 48, 48, 47, 47, 47)
  )
  expect_identical(
    cumsum(ex_uts())$values,
    c(48.375, 96.875, 145.25, 192.25, 239.75, 287.1)
  )
})
