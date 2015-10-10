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

test_that("Expected correct calculation", {
  expect_equal(deg2rad(180), pi)
  expect_equal(rad2deg(pi), 180)
  expect_equal(getarea("radius", 1), pi)
  expect_equal(getarea("diameter", 2), pi)
  expect_equal(getarea("area", 1), 1)
  expect_equal(getarea("circumference", 1), 0.25/pi)
  expect_equal(getdiameter("radius", 1), 2)
  expect_equal(getdiameter("diameter", 1), 1)
  expect_equal(getradius("radius", 1), 1)
  expect_equal(getradius("diameter", 1), 0.5)
})