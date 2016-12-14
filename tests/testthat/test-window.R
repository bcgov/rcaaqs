context("mean_na")

test_that("returns NA if no non-NA values", {
  ret <- mean_na(NA)
  expect_equal(mean_na(NA), NA)
})

test_that("still works with NA values", {
  expect_equal(mean_na(c(3,NA)), 3)
})

context("rolling_sum")

test_that("works with widths smaller than input vector", {
  expect_equal(rolling_sum(1:8, width = 3), c(1,3,6,9,12,15,18,21))
})

test_that("works with widths bigger than input vector", {
  expect_equal(rolling_sum(1:8, width = 9), cumsum(1:8))
})

test_that("returns all zeroes for a 0 width", {
  expect_equal(rolling_sum(1:8, width = 0), rep(0,8))
})

test_that("returns an error for a negative width", {
  expect_error(rolling_sum(1:8, width = -1))
})

context("rolling_mean")

test_that("works with widths smaller than input vector", {
  expect_equal(rolling_mean(1:8, width = 3), c(1, 1.5, 2, 3, 4, 5, 6, 7))
})

test_that("works with widths bigger than input vector", {
  expect_equal(rolling_mean(1:8, width = 9), ((1:8)+1)/2)
})

test_that("returns all NaN for a 0 width", {
  expect_equal(rolling_mean(1:8, width = 0), rep(NaN,8))
})

test_that("returns an error for a negative width", {
  expect_error(rolling_mean(1:8, width = -1))
})

context("n_within_window")

test_that("works with basic numeric", {
  expect_equal(n_within_window(c(1,2,7,8), interval = 1, window = 2), c(1,2,1,2))
})

test_that("works with dates", {
  dates <- as.Date("2000-01-01") + c(1,2,7,8)
  expect_equal(n_within_window(dates, interval = 1, window = 2), c(1,2,1,2))
})

test_that("works with date-times", {
  date_times <- as.POSIXct(as.Date("2000-01-01") + c(1,2,7,8))
  expect_equal(n_within_window(date_times, interval = 60*60*24, window = 2), c(1,2,1,2))
})
