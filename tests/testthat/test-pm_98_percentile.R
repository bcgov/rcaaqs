context("Percent valid days")

dates <- seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), by = "days")
ly_dates <- seq.Date(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), by = "days")

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
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = (365-12)/365 * 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = (90-3)/90 * 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = (91-3)/91 * 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = (92-3)/92 * 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = (92-3)/92 * 100))
  
  # And in a leap year
  dates <- seq.Date(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), by = "days")
  dates <- dates[-c(20, 40, 70, 100, 130, 170, 200, 230, 250, 300, 320, 350)]
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = (366-12)/366 * 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = (91-3)/91 * 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = (91-3)/91 * 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = (92-3)/92 * 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = (92-3)/92 * 100))
})

test_that("Works with NA dates", {
  dates <- seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), by = "days")
  # Remove three from each quarter
  dates[c(20, 40, 70, 100, 130, 170, 200, 230, 250, 300, 320, 350)] <- NA
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = (365-12)/365 * 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = (90-3)/90 * 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = (91-3)/91 * 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = (92-3)/92 * 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = (92-3)/92 * 100))
  
  # And in a leap year
  dates <- seq.Date(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), by = "days")
  dates[c(20, 40, 70, 100, 130, 170, 200, 230, 250, 300, 320, 350)] <- NA
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = (366-12)/366 * 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = (91-3)/91 * 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = (91-3)/91 * 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = (92-3)/92 * 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = (92-3)/92 * 100))
})

test_that("Works with a full quarter missing", {
  dates <- seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-09-30"), by = "days")
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = length(dates)/365 * 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = 0))
  
  # And in a leap year
  dates <- seq.Date(from = as.Date("2012-01-01"), to = as.Date("2012-09-30"), by = "days")
  
  expect_equal(percent_valid_days(dates, q = "year"), c(year = length(dates)/366 * 100))
  expect_equal(percent_valid_days(dates, q = "Q1"), c(Q1 = 100))
  expect_equal(percent_valid_days(dates, q = "Q2"), c(Q2 = 100))
  expect_equal(percent_valid_days(dates, q = "Q3"), c(Q3 = 100))
  expect_equal(percent_valid_days(dates, q = "Q4"), c(Q4 = 0))
})

test_that("Only accepts Date objects", {
  posix <- seq.POSIXt(as.POSIXct("2012-01-01"), as.POSIXct("2012-12-31"), by = "hour")
  
  expect_error(percent_valid_days(posix, q = "year"))

})

test_that("Duplicated dates cause a warning", {
  dates <- seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), by = "days")
  dates <- c(dates, dates)
  
  expect_warning(percent_valid_days(dates, q = "year"))
})

context("pm 98 percentile")

one_year <- data.frame(dates, val = rnorm(length(dates), 20,5))

test_that("Is a data frame", {
  expect_is(pm_98_percentile(one_year, datecol = "dates", valcol = "val"), "data.frame")
})

test_that("Has the right column names and dimensions", {
  test <- pm_98_percentile(one_year, datecol = "dates", valcol = "val")
  expected_names <- c("year", "n_days", "percent_valid_annual", 
                      "percent_valid_q1", "percent_valid_q2", 
                      "percent_valid_q3", "percent_valid_q4", 
                      "ann_98_percentile")
  expect_equal(names(test), expected_names)
  expect_equal(dim(test), c(1, 8))
})
