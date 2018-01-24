context("pm average daily")
library(magrittr)

#hourly_data <- readRDS("hourly.rds")
hourly_data <- pm25_sample_data[1:100, -3]

ret <- pm_daily_avg(hourly_data, dt = "date_time", val = "value", by = "ems_id")

test_that("returns a data frame", {
  expect_is(ret, "data.frame")
})

test_that("has correct dimensions", {
  nrows <- dplyr::group_by(hourly_data, .data$ems_id) %>% 
    dplyr::summarize(n = length(unique(lubridate::as_date(date_time)))) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(n = sum(n)) %>%
    dplyr::pull()
  expect_equal(dim(ret), c(nrows, 7))
})

test_that("has correct classes", {
  expect_is(ret$ems_id, "character")
  expect_is(ret$date, "Date")
  expect_is(ret$n_readings, "integer")
  expect_is(ret$avg_24h, "numeric")
  expect_is(ret$exceed, "logical")
  expect_is(ret$valid_avg_24h, "logical")
  expect_is(ret$flag_avg_24hr_incomplete, "logical")
})

test_that("can exclude data rows", {
  excl_df <-
    data.frame(ems_id = "0220205",
               start = hourly_data$date_time[2],
               stop = hourly_data$date_time[4],
               stringsAsFactors = FALSE)
  ret <- pm_daily_avg(hourly_data, "date_time", "value", "ems_id", excl_df, c("start", "stop"))
  expect_equal(ret$avg_24h[1], round(mean(hourly_data$value[c(1,5:22)]),1))
})


