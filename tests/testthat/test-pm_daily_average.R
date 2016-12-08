context("pm average daily")

hourly_data <- readRDS("hourly.rds")

test_that("returns a data frame", {
  ret <- pm_daily_avg(hourly_data, "date", "val", "id")
  expect_is(ret, "data.frame")
})

test_that("has correct dimensions", {
  ret <- pm_daily_avg(hourly_data, "date", "val", "id")
  nrows <- length(unique(as.Date(hourly_data$date[hourly_data$id == "a"]))) + 
  length(unique(as.Date(hourly_data$date[hourly_data$id == "b"])))
  expect_equal(dim(ret), c(nrows, 7))
})

test_that("has correct classes", {
  ret <- pm_daily_avg(hourly_data, "date", "val", "id")
  
  expect_is(ret$id, "character")
  expect_is(ret$date, "Date")
  expect_is(ret$n_readings, "integer")
  expect_is(ret$avg_24h, "numeric")
  expect_is(ret$exceed, "logical")
  expect_is(ret$valid_avg_24h, "logical")
  expect_is(ret$flag_avg_24hr_incomplete, "logical")
})

test_that("can exclude data rows", {
  exclude_df <-
    data.frame(id = "a",
               start = hourly_data$date[2],
               stop = hourly_data$date[4])
  ret <- pm_daily_avg(hourly_data, "date", "val", "id", exclude_df, c("start", "stop"))
  
  expect_is(ret$id, "character")
  expect_is(ret$date, "Date")
  expect_is(ret$n_readings, "integer")
  expect_is(ret$avg_24h, "numeric")
  expect_is(ret$exceed, "logical")
  expect_is(ret$valid_avg_24h, "logical")
  expect_is(ret$flag_avg_24hr_incomplete, "logical")
})


# 
# exclude_data(hourly_data, exclude_df)
# 
# pm_daily_avg(hourly_data, "date", "val", "id", exclude_df)
