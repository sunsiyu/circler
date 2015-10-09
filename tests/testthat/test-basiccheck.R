context("Basic Checks")

test_that("Expected error in case of missing argument", {
  expect_error(deg2rad())
  expect_error(rad2deg())
})

test_that("Expected error in case of non-numeric argument", {
  expect_error(getarea("radius", "non-numeric"))
  expect_error(getradius("radius", "non-numeric"))
  expect_error(getdiameter("radius", "non-numeric"))
  expect_error(getcircumference("radius", "non-numeric"))
})