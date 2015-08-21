context("start_end")

test_that("start works",{
  expect_equal(
    start(ex_uts()),
    ex_uts()$times[1]
  )
  
  expect_equal(
    start(uts()),
    as.POSIXct(NA)
  )
})


test_that("end works",{
  expect_equal(
    end(ex_uts()),
    ex_uts()$times[length(ex_uts())]
  )
  
  expect_equal(
    end(uts()),
    as.POSIXct(NA)
  )
})
