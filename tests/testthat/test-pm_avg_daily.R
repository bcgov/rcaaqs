context("pm average daily")

hourly_data <- readRDS("hourly.rds")

test_that("returns a data frame", {
  ret <- pm_avg_daily(hourly_data, "date", "val", "id")
  
  expect_is(ret, "data.frame")
})

test_that("has correct dimensions", {
  ret <- pm_avg_daily(hourly_data, "date", "val", "id")
  
  nrows <- length(unique(as.Date(hourly_data$date[hourly_data$id == "a"]))) + 
  length(unique(as.Date(hourly_data$date[hourly_data$id == "b"])))
  
  expect_equal(dim(ret), c(nrows, 5))
})

test_that("has correct classes", {
  ret <- pm_avg_daily(hourly_data, "date", "val", "id")
  
  expect_is(ret$id, "character")
  expect_is(ret$date, "Date")
  expect_is(ret$n_readings, "integer")
  expect_is(ret$avg_24hr, "numeric")
  expect_is(ret$year, "integer")
})
