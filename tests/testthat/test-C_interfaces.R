context("C utility functions")

test_that("num_leq_sorted works",{
  expect_identical(
    num_leq_sorted(c(-3, 1, 3, 5, 7), c(0, 1, 4, 9, 16)),
    as.integer(c(0, 2, 2, 3, 3))
  )
})

test_that("num_leq_sorted robust to numerical noise",{
  expect_identical(
    num_leq_sorted(1 - 1e-13, 0:2, tolerance=1e-12),
    2L
  )
  expect_identical(
    num_leq_sorted(1 + 1e-13, 0:2, tolerance=1e-12),
    2L
  )
})

test_that("num_leq_sorted correctly handles trivial cses",{
  expect_identical(
    num_leq_sorted(1:5, 1:5),
    1:5
  )
  expect_identical(
    num_leq_sorted(c(), 1:5),
    integer()
  )
  expect_identical(
    num_leq_sorted(1:5, c()),
    rep(0L, 5)
  )
})