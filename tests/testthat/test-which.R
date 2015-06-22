context("which")

test_that("Which works",{
  expect_equal(
    which(ex_uts() > 48),
    as.POSIXct(c("2007-11-08 07:00:00", "2007-11-08 08:01:00", "2007-11-08 13:15:00"))
  )
  
  # Intentional error
  expect_error(
    which(ex_uts())
  )
})


