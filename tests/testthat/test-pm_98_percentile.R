context("quantile2")

test_that("only accepts numbers for x", {
  expect_error(quantile2("foo"))
})

test_that("type is either 'caaqs' or 0:10", {
  expect_error(quantile2(1:5, type = "foo"))
  expect_error(quantile2(1:5, type = 0))
  expect_error(quantile2(1:5, type = 10))
})

test_that("percentiles are correct with type = 'caaq'", {
  expect_true(all.equal(quantile2(1:25, probs = 0.98, type = "caaqs"), 25))
  expect_true(all.equal(quantile2(1:50, probs = 0.98, type = "caaqs"), 50))
  expect_true(all.equal(quantile2(1:51, probs = 0.98, type = "caaqs"), 50))
  expect_true(all.equal(quantile2(1:100, probs = 0.98, type = "caaqs"), 99))
  expect_true(all.equal(quantile2(1:101, probs = 0.98, type = "caaqs"), 99))
  expect_true(all.equal(quantile2(1:150, probs = 0.98, type = "caaqs"), 148))
  expect_true(all.equal(quantile2(1:151, probs = 0.98, type = "caaqs"), 148))
  expect_true(all.equal(quantile2(1:200, probs = 0.98, type = "caaqs"), 197))
  expect_true(all.equal(quantile2(1:201, probs = 0.98, type = "caaqs"), 197))
  expect_true(all.equal(quantile2(1:250, probs = 0.98, type = "caaqs"), 246))
  expect_true(all.equal(quantile2(1:251, probs = 0.98, type = "caaqs"), 246))
  expect_true(all.equal(quantile2(1:300, probs = 0.98, type = "caaqs"), 295))
  expect_true(all.equal(quantile2(1:301, probs = 0.98, type = "caaqs"), 295))
  expect_true(all.equal(quantile2(1:350, probs = 0.98, type = "caaqs"), 344))
  expect_true(all.equal(quantile2(1:351, probs = 0.98, type = "caaqs"), 344))
  expect_true(all.equal(quantile2(1:366, probs = 0.98, type = "caaqs"), 359))
})

test_that("has names", {
  expect_true(names(quantile2(1:10, probs = 0.98, names = TRUE, type = "caaqs")) == "98%")
})

test_that("fails with NAs if na.rm = FALSE", {
  x <- 1:10
  x[3] <- NA
  expect_error(quantile2(x, na.rm = FALSE, type = "caaqs"))
})

test_that("works with NA if na.rm = TRUE", {
  x <- 1:10
  x[3] <- NA
  expect_equal(quantile2(x, na.rm = TRUE, type = "caaqs"), 10)
})

test_that("works with length(probs) > 1 and type = 'caaqs'", {
  expect_equal(quantile2(1:100, probs = c(0.1, 0.5, 0.98), type = "caaqs"), 
               c(11, 51, 99))
})

test_that("passes correctly to quantile", {
  expect_equal(lapply(1:9, function(x) {
    set.seed(42)
    quantile(rnorm(100), probs = seq(0, 1, 0.25), names = TRUE, type = x)
  }), 
  lapply(1:9, function(x) {
    set.seed(42)
    quantile2(rnorm(100), probs = seq(0, 1, 0.25), names = TRUE, type = x)
  }))
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

context("pm 98 percentile without completeness")

multi_id <- readRDS("daily_averages.rds")

one_id <- multi_id[multi_id$id == "a", ]

# Make a multi-year df
# dates2 <- seq.Date(from = as.Date("2012-01-01"), 
#                             to = as.Date("2012-12-31"), by = "days")
# mult_years <- data.frame(id = c(rep("a", length(dates)), rep("b", length(dates2))), 
#                          dates = c(dates, dates2), 
#                          val = rnorm(length(dates) + length(dates2), 20, 5), 
#                          stringsAsFactors = FALSE)

test_one <- pm_98_percentile(one_id, date = "dates", val = "val", 
                             completeness = FALSE)
test_mult <- pm_98_percentile(multi_id, date = "dates", val = "val", by = "id", 
                              completeness = FALSE)

test_that("Only accepts date-type objects with time interval of one day", {
  posix <- seq.POSIXt(as.POSIXct("2012-01-01"), as.POSIXct("2012-01-31"), by = "hour")
  test <- data.frame(posix, val = rnorm(length(posix), 20, 5))
  
  expect_error(pm_98_percentile(test, "posix", "val", completeness = FALSE))
})

test_that("Is a data frame", {
  expect_is(test_one, "data.frame")
  expect_is(test_mult, "data.frame")
})

test_that("Has the right column names and dimensions", {
  expected_names <- c("year", "n_days", "ann_98_percentile", "exceed")
  expect_equal(names(test_one), expected_names)
  expect_equal(dim(test_one), c(3, 4))
  
  # For multiple years:
  expect_equal(names(test_mult), c("id", expected_names))
  expect_equal(dim(test_mult), c(6, 5))
})

test_that("Columns are the right class", {
  classes <- c("integer", "integer", "numeric", "logical")
  expect_equal(unname(sapply(test_one, class)), classes)
  expect_equal(unname(sapply(test_mult, class)), c("character", classes))
})

test_that("Exceed works", {
  expect_false(any(test_mult$exceed))
  
  set.seed(42)
  multi_id$val <- rnorm(nrow(multi_id), 35, 1)
  res <- pm_98_percentile(multi_id, date = "dates", val = "val", by = "id", 
                          std = 28, completeness = FALSE)
  expect_true(all(res$exceed))
})

test_that("Number of days is correct", {
  expect_equal(test_one$n_days, c(317L, 332L, 317L))
  expect_equal(test_mult$n_days, c(317L, 332L, 317L, 333L, 218L, 146L))
})

context("pm_data_complete")

test_one <- pm_data_complete(one_id, date = "dates", val = "val")
test_mult <- pm_data_complete(multi_id, "dates", "val", by = "id")

test_that("Only accepts date-type objects with time interval of one day", {
  posix <- seq.POSIXt(as.POSIXct("2012-01-01"), as.POSIXct("2012-01-31"), by = "hour")
  test <- data.frame(posix, val = rnorm(length(posix), 20, 5))
  
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


