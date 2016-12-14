context("o3 daily 8hr max")

hourly_data <- readRDS("hourly.rds")

test_that("returns a data frame", {
  ret <- o3_daily_max(hourly_data, "date", "val", "id")
  expect_is(ret, "data.frame")
})

test_that("has correct dimensions", {
  ret <- o3_daily_max(hourly_data, "date", "val", "id")
  nrows <- length(unique(as.Date(hourly_data$date[hourly_data$id == "a"]))) + 
    length(unique(as.Date(hourly_data$date[hourly_data$id == "b"])))
  expect_equal(dim(ret), c(nrows, 7))
})

test_that("has correct classes", {
  ret <- o3_daily_max(hourly_data, "date", "val", "id")
  expect_is(ret$id, "character")
  expect_is(ret$date, "Date")
  expect_is(ret$n_readings, "integer")
  expect_is(ret$max8hr, "numeric")
  expect_is(ret$exceed, "logical")
  expect_is(ret$valid_max8hr, "logical")
  expect_is(ret$flag_max8hr_incomplete, "logical")
})

test_that("can exclude data rows", {
  excl_df <-
    data.frame(id = "a",
               start = hourly_data$date[2],
               stop = hourly_data$date[4],
               stringsAsFactors = FALSE)
  ret <- o3_daily_max(hourly_data, "date", "val", "id", excl_df, c("start", "stop"))
  expect_equal(ret$max8hr[1], round(max(hourly_data$val[c(1,5:22)]),1))
})

