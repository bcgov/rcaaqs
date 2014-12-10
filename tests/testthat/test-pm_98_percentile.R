library("bcairquality")

context("Percent valid days")

test_that("Works with a full time series", {
  dates <- seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), by = "days")
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = 100))
  
  # And in a leap year
  dates <- seq.Date(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), by = "days")
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = 100))
})
