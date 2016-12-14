context("so2 daily max")

hourly_data <- readRDS("hourly.rds")

test_that("returns a data frame", {
  ret <- so2_daily_max(hourly_data, "date", "val", "id")
  expect_is(ret, "data.frame")
})

test_that("has correct dimensions", {
  ret <- so2_daily_max(hourly_data, "date", "val", "id")
  nrows <- length(unique(as.Date(hourly_data$date[hourly_data$id == "a"]))) + 
    length(unique(as.Date(hourly_data$date[hourly_data$id == "b"])))
  expect_equal(dim(ret), c(nrows, 7))
})

test_that("has correct classes", {
  ret <- so2_daily_max(hourly_data, "date", "val", "id")
  expect_is(ret$id, "character")
  expect_is(ret$date, "Date")
  expect_is(ret$n_readings, "integer")
  expect_is(ret$max_24h, "numeric")
  expect_is(ret$exceed, "logical")
  expect_is(ret$valid_max_24h, "logical")
  expect_is(ret$flag_max_24hr_incomplete, "logical")
})

test_that("can exclude data rows", {
  excl_df <-
    data.frame(id = "a",
               start = hourly_data$date[2],
               stop = hourly_data$date[4],
               stringsAsFactors = FALSE)
  ret <- so2_daily_max(hourly_data, "date", "val", "id", excl_df, c("start", "stop"))
  expect_equal(ret$max_24h[1], round(max(hourly_data$val[c(1,5:22)]),1))
})

