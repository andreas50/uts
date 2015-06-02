context("C utility functions")

test_that("num_leq_sorted_arrays works",{
  expect_equal(
    num_leq_sorted_arrays(c(-3, 1, 3, 5, 7), c(0, 1, 4, 9, 16)),
    c(0, 2, 2, 3, 3)
  )
})

test_that("num_leq_sorted_arrays robust to numerical noise",{
  expect_equal(
    num_leq_sorted_arrays(1 - 1e-13, 0:2, eps=1e-12),
    2
  )
  expect_equal(
    num_leq_sorted_arrays(1 + 1e-13, 0:2, eps=1e-12),
    2
  )
})

test_that("num_leq_sorted_arrays correctly handles trivial cses",{
  expect_equal(
    num_leq_sorted_arrays(1:5, 1:5),
    1:5
  )
  expect_equal(
    num_leq_sorted_arrays(c(), 1:5),
    numeric(0)
  )
  expect_equal(
    num_leq_sorted_arrays(1:5, c()),
    rep(0, 5)
  )
})