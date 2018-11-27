context("so2 caaqs wrappers")
d1 <- readRDS("so2_raw1.rds")
d2 <- readRDS("so2_raw2.rds")
so2_caaqs_3yr_one <- NULL
so2_caaqs_1yr_one <- NULL
so2_caaqs_3yr_mult <- NULL
so2_caaqs_1yr_mult <- NULL

test_that("classes and extractors for so2 3yr", {
  expect_message(so2_caaqs_3yr_one <<- so2_3yr_caaqs(d1), 
                 "Calculating SO2 daily maximum")
  expect_is(so2_caaqs_3yr_one, c("caaqs", "so2_3yr"))
  expect_length(so2_caaqs_3yr_one, 4)
  expect_named(so2_caaqs_3yr_one, c("daily_max", "yearly_99", "three_yr_rolling", "caaqs"))
  for (d in so2_caaqs_3yr_one) {
    expect_is(d, "data.frame")
  }
  expect_is(get_caaqs(so2_caaqs_3yr_one), "data.frame")
  expect_is(get_daily(so2_caaqs_3yr_one),"data.frame")
  expect_is(get_yearly(so2_caaqs_3yr_one), "data.frame")
  expect_is(get_three_yr_rolling(so2_caaqs_3yr_one), "data.frame")
})

test_that("classes and extractors for so2 1yr", {
  expect_message(so2_caaqs_1yr_one <<- so2_1yr_caaqs(d1), 
                 "Calculating SO2 annual average CAAQS metric")
  expect_is(so2_caaqs_1yr_one, c("caaqs", "so2_1yr"))
  expect_length(so2_caaqs_1yr_one, 3)
  expect_named(so2_caaqs_1yr_one, c("hourly", "yearly_hr", "caaqs"))
  for (d in so2_caaqs_1yr_one) {
    expect_is(d, "data.frame")
  }
  expect_is(get_hourly(so2_caaqs_1yr_one), "data.frame")
  expect_is(get_caaqs(so2_caaqs_1yr_one), "data.frame")
  expect_error(get_daily(so2_caaqs_1yr_one))
  expect_is(get_yearly(so2_caaqs_1yr_one), "data.frame")
  expect_error(get_three_yr_rolling(so2_caaqs_1yr_one))
})

test_that("so2_3yr_caaqs single", {
  caaqs <- get_caaqs(so2_caaqs_3yr_one)
  expect_length(caaqs$caaqs, 3)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("so2_3yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("so2_3yr")] 
                  == "Not Achieved", na.rm = TRUE))
  expect_length(get_caaqs(so2_caaqs_3yr_one), 10)
  expect_length(get_daily(so2_caaqs_3yr_one), 6)
  expect_length(get_yearly(so2_caaqs_3yr_one), 11)
  expect_length(get_three_yr_rolling(so2_caaqs_3yr_one), 18)
})

test_that("so2_3yr_caaqs groups", {
  expect_message(
    so2_caaqs_3yr_mult <<- so2_3yr_caaqs(d2, by = c("ems_id", "site")), 
    "Calculating SO2 daily maximum"
  )
  caaqs <- get_caaqs(so2_caaqs_3yr_mult)
  expect_length(caaqs$caaqs, 6)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("so2_3yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("so2_3yr")] 
                  == "Not Achieved", na.rm = TRUE)) 
  expect_length(get_caaqs(so2_caaqs_3yr_mult), 12)
  expect_length(get_daily(so2_caaqs_3yr_mult), 8)
  expect_length(get_yearly(so2_caaqs_3yr_mult), 13)
  expect_length(get_three_yr_rolling(so2_caaqs_3yr_mult), 20)
})

test_that("so2_1yr_caaqs single", {
  caaqs <- get_caaqs(so2_caaqs_1yr_one)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("so2_1yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("so2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))
  expect_is(caaqs, "data.frame")
  expect_length(caaqs$caaqs, 3)
  expect_length(get_caaqs(so2_caaqs_1yr_one), 6)
  expect_length(get_yearly(so2_caaqs_1yr_one), 11)
  expect_length(get_hourly(so2_caaqs_1yr_one), 8)
})

test_that("so2_1yr_caaqs groups", {
  expect_message(
    so2_caaqs_1yr_mult <<- so2_1yr_caaqs(d2, by = c("ems_id", "site")), 
    "Calculating SO2 annual average CAAQS metric"
  )
  caaqs <- get_caaqs(so2_caaqs_1yr_mult)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("so2_1yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("so2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))  
  expect_is(caaqs, "data.frame")
  expect_length(caaqs$caaqs, 6)
  expect_length(get_caaqs(so2_caaqs_1yr_mult), 8)
  expect_length(get_yearly(so2_caaqs_1yr_mult), 13)
  expect_length(get_hourly(so2_caaqs_1yr_mult), 8)
})

test_that("caaqs_management for so2 1yr", {
  expect_is(so2_caaqs_1yr_one_mgmt <- caaqs_management(so2_caaqs_1yr_one), 
            "caaqs_mgmt")
  expect_length(so2_caaqs_1yr_one_mgmt, 3)
  expect_length(get_caaqs(so2_caaqs_1yr_one_mgmt), 9)
  expect_length(get_yearly(so2_caaqs_1yr_one_mgmt), 14)
  expect_length(get_hourly(so2_caaqs_1yr_one_mgmt), 8)
})

test_that("caaqs_management for so2 3yr", {
  expect_is(so2_caaqs_3yr_one_mgmt <- caaqs_management(so2_caaqs_3yr_one), 
            "caaqs_mgmt")
  expect_length(so2_caaqs_3yr_one_mgmt, 4)
  expect_length(get_caaqs(so2_caaqs_3yr_one_mgmt), 13)
  expect_length(get_daily(so2_caaqs_3yr_one_mgmt), 6)
  expect_length(get_yearly(so2_caaqs_3yr_one_mgmt), 14)
  expect_length(get_three_yr_rolling(so2_caaqs_3yr_one_mgmt), 21)
})

test_that("caaqs_management for so2 1yr with groups", {
  expect_is(so2_caaqs_1yr_mult_mgmt <- caaqs_management(so2_caaqs_1yr_mult), 
            "caaqs_mgmt")
  expect_length(so2_caaqs_1yr_mult_mgmt, 3)
  expect_length(get_caaqs(so2_caaqs_1yr_mult_mgmt), 11)
  expect_length(get_yearly(so2_caaqs_1yr_mult_mgmt), 16)
  expect_length(get_hourly(so2_caaqs_1yr_mult_mgmt), 8)
})

test_that("caaqs_management for so2 3yr with groups", {
  expect_is(so2_caaqs_3yr_mult_mgmt <- caaqs_management(so2_caaqs_3yr_mult), 
            "caaqs_mgmt")
  expect_length(so2_caaqs_3yr_mult_mgmt, 4)
  expect_length(get_caaqs(so2_caaqs_3yr_mult_mgmt), 15)
  expect_length(get_daily(so2_caaqs_3yr_mult_mgmt), 8)
  expect_length(get_yearly(so2_caaqs_3yr_mult_mgmt), 16)
  expect_length(get_three_yr_rolling(so2_caaqs_3yr_mult_mgmt), 23)
})

