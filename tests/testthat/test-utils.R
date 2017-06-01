context("utility functions")

test_that("Mode works", {
  
})

test_that("round_caaqs works", {
  test_zero <- c(0.49, 0.5, 0.51, 1.49, 1.5, 1.51, 2.49, 2.5)
  test_one <- test_zero / 10
  
  expected_zero <- c(0, 1, 1, 1, 2, 2, 2, 3)
  expected_one <- c(0.0, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.3)
  
  expect_equal(round_caaqs(test_zero, 0), expected_zero)
  expect_equal(round_caaqs(test_one, 1), expected_one)
})
