context("no2 98 percentile")

multi_id <- readRDS("daily_averages.rds")
one_id <- multi_id[multi_id$id == "a", ]

test_one <- no2_yearly_98(one_id, dt = "dates", val = "val")
test_mult <- no2_yearly_98(multi_id, dt = "dates", val = "val", by = "id")

test_that("Only accepts date objects", {
  posix <- seq.POSIXt(as.POSIXct("2012-01-01"), as.POSIXct("2012-01-31"), by = "hour")
  test <- data.frame(posix, val = rnorm(length(posix), 20, 5))

  expect_error(no2_yearly_98(test, "posix", "val"))
})

test_that("Is a data frame", {
  expect_is(test_one, "data.frame")
  expect_is(test_mult, "data.frame")
})

 
test_that("Has the right column names and dimensions", {
  expected_names <-c("year", "valid_in_year", "quarter_1", "quarter_2", "quarter_3", 
                     "quarter_4", "ann_98_percentile", "valid_year")
  expect_equal(names(test_one), expected_names)
  expect_equal(dim(test_one), c(4, 8))

  # For multiple years:
  expect_equal(names(test_mult), c("id", expected_names))
  expect_equal(dim(test_mult), c(8, 9))
})
 
test_that("Columns are the right class", {
  classes <- c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "logical")
  expect_equal(unname(sapply(test_one, class)), classes)
  expect_equal(unname(sapply(test_mult, class)), c("character", classes))
})
 
test_that("Number of valid days in year correct", {
  expect_equal(c(365,365,366,365) * test_one$valid_in_year, c(1L, 361L, 358L, 357L))
  expect_equal(c(365,365,366,365) * test_mult$valid_in_year, c(1L, 361L, 358L, 357L, 1L, 365L, 243L, 170L))
})

test_that("can exclude data rows", {
  # Take out values 9.0 or above to make sure it changes
  high_dates <- 
    multi_id$date[multi_id$id == "a" & strftime(multi_id$dates,"%Y") == 2011 & 
                    !is.na(multi_id$val)  & multi_id$val >= 9.0]
  excl_df <-
    data.frame(id = "a",
               start = high_dates,
               stop = high_dates + 1,
               stringsAsFactors = FALSE)
  
  ret <- no2_yearly_98(multi_id, dt = "dates", val = "val", by = "id", 
                      exclude_df = excl_df, exclude_df_dt = c("start", "stop"))
  
  expect_equal(ret$ann_98_percentile[ret$id == "a" & ret$year == 2011], 8.4)
})

