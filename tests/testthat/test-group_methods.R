context("group_methods")

test_that("Summary works",{
  expect_equal(
    min(ex_uts(), na.rm=TRUE),
    min(ex_uts()$values)
  )
  expect_true(
    any(ex_uts() > 48)
  )
  expect_false(
    all(ex_uts() > 48)
  )
})


test_that("Math works",{
  expect_equal(
    log(ex_uts(), base=2)$values,
    log(ex_uts()$values, base=2)
  )
  expect_equal(
    cumsum(ex_uts())$values,
    cumsum(ex_uts()$values)
  )
})


test_that("Ops works",{
  # Regression tests - unary operators
  expect_equal_to_reference(-ex_uts(), file="test-group_methods_Ops_1.rds")
  expect_equal_to_reference(!ex_uts(), file="test-group_methods_Ops_2.rds")
  
  # Regression tests - binary operators
  expect_equal_to_reference(ex_uts() * 2, file="test-group_methods_Ops_3.rds")
  expect_equal_to_reference(2 * ex_uts(), file="test-group_methods_Ops_4.rds")
  expect_equal_to_reference(ex_uts() / ex_uts(), file="test-group_methods_Ops_5.rds")
  expect_equal_to_reference(ex_uts() > 48, file="test-group_methods_Ops_6.rds")
  expect_equal_to_reference(48 >= ex_uts(), file="test-group_methods_Ops_7.rds")
})
