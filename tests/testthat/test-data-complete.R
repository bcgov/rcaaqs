context("Percent valid days")

dates <- seq(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), by = "1 day")
ly_dates <- seq(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), by = "1 day")

test_that("Only accepts POSIX objects with time interval of one day", {
  dt <- seq(as.POSIXct("2012-01-01"), as.POSIXct("2012-12-31"), by = "1 hour")
  
  expect_error(percent_valid_days(dt, q = "year"))
})

test_that("Works with a full time series", {
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = 100))
  
  # And in a leap year
  expect_equal(percent_valid_days(dates, q = "year"), c(year = 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = 100))
})

test_that("Works with missing dates", {
  dates <- seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), by = "days")
  # Remove three from each quarter
  dates <- dates[-c(20, 40, 70, 100, 130, 170, 200, 230, 250, 300, 320, 350)]
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = (365 - 12) / 365 * 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = (90 - 3) / 90 * 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = (91 - 3) / 91 * 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = (92 - 3) / 92 * 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = (92 - 3) / 92 * 100))
  
  # And in a leap year
  dates <- seq.Date(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), by = "days")
  dates <- dates[-c(20, 40, 70, 100, 130, 170, 200, 230, 250, 300, 320, 350)]
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = (366 - 12) / 366 * 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = (91 - 3) / 91 * 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = (91 - 3) / 91 * 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = (92 - 3) / 92 * 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = (92 - 3) / 92 * 100))
})

test_that("Works with NA dates", {
  dates <- seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), by = "days")
  # Remove three from each quarter
  dates[c(20, 40, 70, 100, 130, 170, 200, 230, 250, 300, 320, 350)] <- NA
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = (365 - 12) / 365 * 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = (90 - 3) / 90 * 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = (91 - 3) / 91 * 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = (92 - 3) / 92 * 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = (92 - 3) / 92 * 100))
  
  # And in a leap year
  dates <- seq.Date(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), by = "days")
  dates[c(20, 40, 70, 100, 130, 170, 200, 230, 250, 300, 320, 350)] <- NA
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = (366 - 12) / 366 * 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = (91 - 3) / 91 * 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = (91 - 3) / 91 * 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = (92 - 3) / 92 * 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = (92 - 3) / 92 * 100))
})

test_that("Works with a full quarter missing", {
  dates <- seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-09-30"), by = "days")
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = length(dates) / 365 * 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = 0))
  
  # And in a leap year
  dates <- seq.Date(from = as.Date("2012-01-01"), to = as.Date("2012-09-30"), by = "days")
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = length(dates) / 366 * 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = 0))
})

context("pm_data_complete")

multi_id <- readRDS("hourly.rds")

one_id <- multi_id[multi_id$id == "a", ]

test_one <- pm_data_complete(one_id, "date", val = "val")
test_mult <- pm_data_complete(multi_id, "date", "val", by = "id")

test_that("Only accepts date-time objects (POSIXt)", {
  dt <- seq(Sys.Date(), by = "1 day", length.out = 10)
  test <- data.frame(dt, val = rnorm(length(dt), 20, 5))
  
  expect_error(pm_data_complete(test, "dt", "val"))
})

test_that("is a data frame", {
  expect_is(test_one, "data.frame")
  expect_is(test_mult, "data.frame")
})

test_that("Has correct column types and dimensions", {
  expect_equal(dim(test_one), c(3, 10))
  expect_equal(dim(test_mult), c(6, 11))
})

test_that("Number of days is correct", {
  expect_equal(test_one$n_days, c(358L, 352L, 353L))
  expect_equal(test_mult$n_days, c(358L, 352L, 353L, 363L, 240L, 164L))
})

test_that("Percentages are <= 100 and >= 0", {
  expect_less_than(max(as.matrix(test_one[,3:7])), 100 + 1e-10)
  expect_more_than(min(as.matrix(test_one[,3:7])), -1e-10)
  
  expect_less_than(max(as.matrix(test_mult[,4:8])), 100 + 1e-10)
  expect_more_than(min(as.matrix(test_mult[,4:8])), -1e-10)
})


