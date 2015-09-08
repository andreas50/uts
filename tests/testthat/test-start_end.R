context("start_end")

test_that("start works",{
  expect_identical(
    start(ex_uts()),
    ex_uts()$times[1]
  )
  
  expect_identical(
    start(uts()),
    as.POSIXct(NA)
  )
})


test_that("end works",{
  expect_identical(
    end(ex_uts()),
    ex_uts()$times[length(ex_uts())]
  )
  
  expect_identical(
    end(uts()),
    as.POSIXct(NA)
  )
})
