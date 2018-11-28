context("pm caaqs wrappers")

d1 <- readRDS("pm_raw1.rds")
d2 <- readRDS("pm_raw2.rds")
pm2.5_24h_caaqs_one <- NULL
pm2.5_24h_caaqs_mult <- NULL
pm2.5_annual_caaqs_one <- NULL
pm2.5_annual_caaqs_mult <- NULL

test_that("classes and extractors for pm2.5_24h", {
  expect_message(pm2.5_24h_caaqs_one <<- pm_24h_caaqs(d1), 
                 "Calculating PM 2.5 daily average")
  expect_is(pm2.5_24h_caaqs_one, c("caaqs", "pm2.5_24h"))
  expect_length(pm2.5_24h_caaqs_one, 4)
  expect_named(pm2.5_24h_caaqs_one, c("daily_avg", "yearly_98", "three_yr_rolling", "caaqs"))
  for (d in pm2.5_24h_caaqs_one) {
    expect_is(d, "data.frame")
  }
  expect_is(get_caaqs(pm2.5_24h_caaqs_one), "data.frame")
  expect_is(get_daily(pm2.5_24h_caaqs_one),"data.frame")
  expect_is(get_yearly(pm2.5_24h_caaqs_one), "data.frame")
  expect_is(get_three_yr_rolling(pm2.5_24h_caaqs_one), "data.frame")
  saveRDS(pm2.5_24h_caaqs_one, "pm_24h_caaqs1.rds")
})

test_that("pm2.5_24h_caaqs single", {
  caaqs <- get_caaqs(pm2.5_24h_caaqs_one)
  expect_length(caaqs$caaqs, 3)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("pm2.5_24h")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("pm2.5_24h")] 
                  == "Not Achieved", na.rm = TRUE))
  expect_length(get_caaqs(pm2.5_24h_caaqs_one), 10)
  expect_length(get_daily(pm2.5_24h_caaqs_one), 6)
  expect_length(get_yearly(pm2.5_24h_caaqs_one), 12)
  expect_length(get_three_yr_rolling(pm2.5_24h_caaqs_one), 19)
})

test_that("pm2.5_24h_caaqs groups", {
  expect_message(
    pm2.5_24h_caaqs_mult <<- pm_24h_caaqs(d2, by = c("ems_id", "site")), 
    "Calculating PM 2.5 daily average"
  )
  caaqs <- get_caaqs(pm2.5_24h_caaqs_mult)
  expect_length(caaqs$caaqs, 6)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("pm2.5_24h")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("pm2.5_24h")] 
                  == "Not Achieved", na.rm = TRUE)) 
  expect_length(get_caaqs(pm2.5_24h_caaqs_mult), 12)
  expect_length(get_daily(pm2.5_24h_caaqs_mult), 8)
  expect_length(get_yearly(pm2.5_24h_caaqs_mult), 14)
  expect_length(get_three_yr_rolling(pm2.5_24h_caaqs_mult), 21)
  saveRDS(pm2.5_24h_caaqs_mult, "pm_24h_caaqs2.rds")
})


test_that("caaqs_management for pm2.5_24h", {
  expect_is(pm2.5_24h_caaqs_one_mgmt <- caaqs_management(pm2.5_24h_caaqs_one), 
            "caaqs_mgmt")
  expect_length(pm2.5_24h_caaqs_one_mgmt, 4)
  expect_length(get_caaqs(pm2.5_24h_caaqs_one_mgmt), 13)
  expect_length(get_daily(pm2.5_24h_caaqs_one_mgmt), 6)
  expect_length(get_yearly(pm2.5_24h_caaqs_one_mgmt), 15)
  expect_length(get_three_yr_rolling(pm2.5_24h_caaqs_one_mgmt), 22)
})

test_that("caaqs_management for pm2.5_24h with groups", {
  expect_is(pm2.5_24h_caaqs_mult_mgmt <- caaqs_management(pm2.5_24h_caaqs_mult), 
            "caaqs_mgmt")
  expect_length(pm2.5_24h_caaqs_mult_mgmt, 4)
  expect_length(get_caaqs(pm2.5_24h_caaqs_mult_mgmt), 15)
  expect_length(get_daily(pm2.5_24h_caaqs_mult_mgmt), 8)
  expect_length(get_yearly(pm2.5_24h_caaqs_mult_mgmt), 17)
  expect_length(get_three_yr_rolling(pm2.5_24h_caaqs_mult_mgmt), 24)
})

test_that("classes and extractors for pm2.5_annual", {
  expect_message(pm2.5_annual_caaqs_one <<- pm_annual_caaqs(d1), 
                 "Calculating PM 2.5 daily average")
  expect_is(pm2.5_annual_caaqs_one, c("caaqs", "pm2.5_annual"))
  expect_length(pm2.5_annual_caaqs_one, 4)
  expect_named(pm2.5_annual_caaqs_one, c("daily_avg", "yearly_avg", "three_yr_rolling", "caaqs"))
  for (d in pm2.5_annual_caaqs_one) {
    expect_is(d, "data.frame")
  }
  expect_is(get_caaqs(pm2.5_annual_caaqs_one), "data.frame")
  expect_is(get_daily(pm2.5_annual_caaqs_one),"data.frame")
  expect_is(get_yearly(pm2.5_annual_caaqs_one), "data.frame")
  expect_is(get_three_yr_rolling(pm2.5_annual_caaqs_one), "data.frame")
  saveRDS(pm2.5_annual_caaqs_one, "pm_annual_caaqs1.rds")
})

test_that("pm2.5_annual_caaqs single", {
  caaqs <- get_caaqs(pm2.5_annual_caaqs_one)
  expect_length(caaqs$caaqs, 3)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("pm2.5_annual")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("pm2.5_annual")] 
                  == "Not Achieved", na.rm = TRUE))
  expect_length(get_caaqs(pm2.5_annual_caaqs_one), 10)
  expect_length(get_daily(pm2.5_annual_caaqs_one), 6)
  expect_length(get_yearly(pm2.5_annual_caaqs_one), 12)
  expect_length(get_three_yr_rolling(pm2.5_annual_caaqs_one), 19)
})

test_that("pm2.5_annual_caaqs groups", {
  expect_message(
    pm2.5_annual_caaqs_mult <<- pm_annual_caaqs(d2, by = c("ems_id", "site")), 
    "Calculating PM 2.5 daily average"
  )
  caaqs <- get_caaqs(pm2.5_annual_caaqs_mult)
  expect_length(caaqs$caaqs, 6)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("pm2.5_annual")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("pm2.5_annual")] 
                  == "Not Achieved", na.rm = TRUE)) 
  expect_length(get_caaqs(pm2.5_annual_caaqs_mult), 12)
  expect_length(get_daily(pm2.5_annual_caaqs_mult), 8)
  expect_length(get_yearly(pm2.5_annual_caaqs_mult), 14)
  expect_length(get_three_yr_rolling(pm2.5_annual_caaqs_mult), 21)
  saveRDS(pm2.5_annual_caaqs_mult, "pm_annual_caaqs2.rds")
})


test_that("caaqs_management for pm2.5_annual", {
  expect_is(pm2.5_annual_caaqs_one_mgmt <- caaqs_management(pm2.5_annual_caaqs_one), 
            "caaqs_mgmt")
  expect_length(pm2.5_annual_caaqs_one_mgmt, 4)
  expect_length(get_caaqs(pm2.5_annual_caaqs_one_mgmt), 13)
  expect_length(get_daily(pm2.5_annual_caaqs_one_mgmt), 6)
  expect_length(get_yearly(pm2.5_annual_caaqs_one_mgmt), 15)
  expect_length(get_three_yr_rolling(pm2.5_annual_caaqs_one_mgmt), 22)
})

test_that("caaqs_management for pm2.5_annual with groups", {
  expect_is(pm2.5_annual_caaqs_mult_mgmt <- caaqs_management(pm2.5_annual_caaqs_mult), 
            "caaqs_mgmt")
  expect_length(pm2.5_annual_caaqs_mult_mgmt, 4)
  expect_length(get_caaqs(pm2.5_annual_caaqs_mult_mgmt), 15)
  expect_length(get_daily(pm2.5_annual_caaqs_mult_mgmt), 8)
  expect_length(get_yearly(pm2.5_annual_caaqs_mult_mgmt), 17)
  expect_length(get_three_yr_rolling(pm2.5_annual_caaqs_mult_mgmt), 24)
})
