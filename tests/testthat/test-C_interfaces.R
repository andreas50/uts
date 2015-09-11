context("C utility functions")

test_that("num_leq_sorted works",{
  # Argument checking
  expect_error(num_leq_sorted(c(), NA))
  expect_error(num_leq_sorted(c(), c(), tolerance=NA))
  expect_error(num_leq_sorted(c(), c(), tolerance=-1))
  
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
  
  expect_identical(
    num_leq_sorted(seq(to=1, by=0.8e-10, length=3), 1, tolerance=1e-10),
    as.integer(c(0, 1, 1))
  )
})

test_that("num_leq_sorted correctly handles trivial cases",{
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



test_that("sorted_union works",{
  # Argument checking
  expect_error(sorted_union(c(), NA))
  expect_error(sorted_union(c(), c(), tolerance=NA))
  expect_error(sorted_union(c(), c(), tolerance=-1))
  
  expect_identical(
    sorted_union(1:3, 2:4),
    as.numeric(1:4)
  )
  expect_identical(
    sorted_union(c(), 1:10),
    as.numeric(1:10)
  )
  expect_identical(
    sorted_union(1:10, c()),
    as.numeric(1:10)
  )
})

test_that("sorted_union robust to numerical noise",{
  expect_identical(
    sorted_union(0, 1e-14),
    c(0, 1e-14)
  )
  expect_identical(
    sorted_union(0, 1e-14, tolerance=1e-12),
    as.numeric(0)
  )
  expect_identical(
    sorted_union(c(0, 1e-14), 2, tolerance=1e-12),
    as.numeric(c(0, 2))
  )
  
  expect_identical(
    sorted_union(seq(0, 2, by=0.5), c(), tolerance=1),
    c(0, 1.5)
  )
  expect_identical(
    sorted_union(0, 1, tolerance=1),
    as.numeric(0)
  )
})


