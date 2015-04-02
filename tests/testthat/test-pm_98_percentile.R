context("cut rank")

test_that("only accepts numbers", {
  expect_error(cut_rank("foo"))
})

test_that("only accepts valid range", {
  expect_error(cut_rank(-1))
  expect_error(cut_rank(367))
})

test_that("cuts are correct", {
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

test_that("Only accepts date-type objects with time interval of one day", {
  posix <- seq.POSIXt(as.POSIXct("2012-01-01"), as.POSIXct("2012-12-31"), by = "hour")
  
  expect_error(percent_valid_days(posix, q = "year"))
})

test_that("Duplicated dates cause a warning", {
  dates <- seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), by = "days")
  dates <- c(dates, dates)
  
  expect_warning(percent_valid_days(dates, q = "year"))
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

context("pm 98 percentile without completeness")

multi_id <- readRDS("daily_averages.rds")

one_id <- multi_id[multi_id$id == "a",]

# Make a multi-year df
# dates2 <- seq.Date(from = as.Date("2012-01-01"), 
#                             to = as.Date("2012-12-31"), by = "days")
# mult_years <- data.frame(id = c(rep("a", length(dates)), rep("b", length(dates2))), 
#                          dates = c(dates, dates2), 
#                          val = rnorm(length(dates) + length(dates2), 20, 5), 
#                          stringsAsFactors = FALSE)

test_one <- pm_98_percentile(one_id, datecol = "dates", valcol = "val", 
                             completeness = FALSE)
test_mult <- pm_98_percentile(multi_id, datecol = "dates", valcol = "val", by = "id", 
                              completeness = FALSE)

test_that("Only accepts date-type objects with time interval of one day", {
  posix <- seq.POSIXt(as.POSIXct("2012-01-01"), as.POSIXct("2012-01-31"), by = "hour")
  test <- data.frame(posix, val = rnorm(length(posix), 20,5))
  
  expect_error(pm_98_percentile(test, "posix", "val", completeness = FALSE))
})

test_that("Is a data frame", {
  expect_is(test_one, "data.frame")
  expect_is(test_mult, "data.frame")
})

test_that("Has the right column names and dimensions", {
  expected_names <- c("year", "n_days", "rep_date", "ann_98_percentile", "exceed")
  expect_equal(names(test_one), expected_names)
  expect_equal(dim(test_one), c(3, 5))
  
  # For multiple years:
  expect_equal(names(test_mult), c("id", expected_names))
  expect_equal(dim(test_mult), c(6, 6))
})

test_that("Columns are the right class", {
  classes <- c("integer", "integer", "Date", "numeric", "logical")
  expect_equal(unname(sapply(test_one, class)), classes)
  expect_equal(unname(sapply(test_mult, class)), c("character", classes))
})

test_that("Exceed works", {
  expect_false(any(test_mult$exceed))
  
  set.seed(42)
  multi_id$val <- rnorm(nrow(multi_id), 35, 1)
  res <- pm_98_percentile(multi_id, datecol = "dates", valcol = "val", by = "id", 
                          std = 28, completeness = FALSE)
  expect_true(all(res$exceed))
})

test_that("Number of days is correct", {
  expect_equal(test_one$n_days, c(358L, 352L, 353L))
  expect_equal(test_mult$n_days, c(358L, 352L, 353L, 363L, 240L, 164L))
})

context("pm_data_complete")

test_one <- pm_data_complete(one_id, datecol = "dates", valcol = "val")
test_mult <- pm_data_complete(multi_id, "dates", "val", by = "id")

test_that("Only accepts date-type objects with time interval of one day", {
  posix <- seq.POSIXt(as.POSIXct("2012-01-01"), as.POSIXct("2012-01-31"), by = "hour")
  test <- data.frame(posix, val = rnorm(length(posix), 20,5))
  
  expect_error(pm_data_complete(test, "posix", "val"))
})

test_that("is a data frame", {
  expect_is(test_one, "data.frame")
  expect_is(test_mult, "data.frame")
})

test_that("Has correct column types and dimensions", {
  expect_equal(dim(test_one), c(3, 9))
  expect_equal(dim(test_mult), c(6, 10))
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


