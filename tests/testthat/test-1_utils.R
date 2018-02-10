context("utility functions")

test_that("Mode works", {
  
})

test_that("round_caaqs works", {
  test_zero <- c(0.49, 0.5, 0.51, 1.49, 1.5, 1.51, 2.49, 2.5)
  test_one <- test_zero / 10
  
  expected_zero <- c(0, 1, 1, 1, 2, 2, 2, 3)
  expected_one <- c(0.0, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.3)
  
  expect_equal(round_caaqs(test_zero, 0), expected_zero)
  expect_equal(round_caaqs(test_one, 1), expected_one)
})

d <- dplyr::filter(no2_sample_data, ems_id %in% c("E231866", "0500886")) %>%
  dplyr::group_by(ems_id) %>%
  dplyr::slice(1:50) %>%
  dplyr::ungroup()

test_that("check_group catches duplicates", {
  expect_error(check_groups(d, dt = "date_time"),
               "Duplicate values in 'date_time'")
  expect_silent(check_groups(dplyr::group_by(d, ems_id, site), 
                             dt = "date_time"))
})

test_that("check_class gets appropriate match", {
  expect_error(check_class("value", no2_sample_data, "character"))
  expect_error(check_class("value", no2_sample_data, "POSIXct"))
  expect_silent(check_class("value", no2_sample_data, "numeric"))
  
  expect_error(check_class("date_time", no2_sample_data, "character"))
  expect_silent(check_class("date_time", no2_sample_data, "POSIXct"))
  expect_error(check_class("date_time", no2_sample_data, "numeric"))
  
  expect_silent(check_class("ems_id", no2_sample_data, "character"))
  expect_error(check_class("ems_id", no2_sample_data, "POSIXct"))
  expect_error(check_class("ems_id", no2_sample_data, "numeric"))
  
})

test_that("initial_check catches hourly problems", {
  
  d <- d[-c(100:110),]
  expect_gt(nrow(initial_check(d, dt = "date_time", val = "value", by = c("ems_id", "site"))),
            nrow(d))
  
  d$date_time <- seq(min(d$date_time), max(d$date_time), by = "20 min")[1:nrow(d)]
  expect_error(initial_check(d, dt = "date_time", val = "value", by = c("ems_id", "site")),
               "Data resolution is less than hourly, summarize to hourly first")
  
})
