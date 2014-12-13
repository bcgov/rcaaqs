context("cut rank")

test_that("cuts are correct", {
  expect_error(cut_rank(-1))
  expect_error(cut_rank(367))
  expect_true(all.equal(cut_rank(1), cut_rank(50), 1))
  expect_true(all.equal(cut_rank(51), cut_rank(100), 2))
  expect_true(all.equal(cut_rank(101), cut_rank(150), 3))
  expect_true(all.equal(cut_rank(151), cut_rank(200), 4))
  expect_true(all.equal(cut_rank(201), cut_rank(250), 5))
  expect_true(all.equal(cut_rank(251), cut_rank(300), 6))
  expect_true(all.equal(cut_rank(301), cut_rank(350), 7))
  expect_true(all.equal(cut_rank(351), cut_rank(366), 8))
})

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

test_that("Only accepts date-type objects with time interval of one day", {
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

# Make a multi-year df
dates2 <- seq.Date(from = as.Date("2012-01-01"), 
                            to = as.Date("2012-12-31"), by = "days")
mult_years <- data.frame(id = c(rep("a", length(dates)), rep("b", length(dates2))), 
                         dates = c(dates, dates2), 
                         val = rnorm(length(dates) + length(dates2), 20, 5), 
                         stringsAsFactors = FALSE)

test_one <- pm_98_percentile(one_year, datecol = "dates", valcol = "val")
test_mult <- pm_98_percentile(mult_years, "dates", "val", "id")

test_that("Only accepts date-type objects with time interval of one day", {
  posix <- seq.POSIXt(as.POSIXct("2012-01-01"), as.POSIXct("2012-01-31"), by = "hour")
  test <- data.frame(posix, val = rnorm(length(posix), 20,5))
  
  expect_error(pm_98_percentile(test, "posix", "val"))
})

test_that("Is a data frame", {
  expect_is(pm_98_percentile(one_year, datecol = "dates", valcol = "val"), "data.frame")
})

test_that("Has the right column names and dimensions", {
  expected_names <- c("year", "n_days", "percent_valid_annual", 
                      "percent_valid_q1", "percent_valid_q2", 
                      "percent_valid_q3", "percent_valid_q4", 
                      "ann_98_percentile", "annual_valid", "quarters_valid", 
                      "exceed")
  expect_equal(names(test_one), expected_names)
  expect_equal(dim(test_one), c(1, 11))
  
  # For multiple years:
  expect_equal(names(test_mult), c("id", expected_names))
  expect_equal(dim(test_mult), c(2, 12))
})

test_that("Columns are the right class", {
  classes <- c("integer", "integer", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "logical", "logical", "logical")
  expect_equal(unname(sapply(test_one, class)), classes)
  expect_equal(unname(sapply(test_mult, class)), c("character", classes))
})

test_that("Number of days is correct", {
  expect_equal(test_one$n_days, 365)
  expect_equal(test_mult$n_days, c(365, 366))
  
  one_year <- one_year[-c(23, 57, 182),] # Remove three days
  test_one <- pm_98_percentile(one_year, "dates", "val")
  
  mult_years <- mult_years[-c(23, 57, 182, 400, 520, 600),] # Remove three days
  test_mult <- pm_98_percentile(mult_years, "dates", "val", "id")
  
  expect_equal(test_one$n_days, 362)
  expect_equal(test_mult$n_days, c(362, 363))
})

test_that("Percentages are <= 100 and >= 0", {
  expect_less_than(max(as.matrix(test_one[,3:8])), 100 + 1e-10)
  expect_more_than(min(as.matrix(test_one[,3:8])), -1e-10)
  
  expect_less_than(max(as.matrix(test_mult[,4:9])), 100 + 1e-10)
  expect_more_than(min(as.matrix(test_mult[,4:9])), -1e-10)
})

test_that("Valid annual works", {
  mult_years <- mult_years[-sample(365, 365 * 0.26),]
  res <- pm_98_percentile(mult_years, "dates", "val")
  expect_false(res$annual_valid[1])
  expect_true(res$annual_valid[2])
})
