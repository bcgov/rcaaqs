context("pm caaqs wrappers")

test_that("classes and extractors for pm2.5 24hr", {
  d <- readRDS("pm_raw1.rds")
  pm_caaqs <- pm_24h_caaqs(d)
  expect_is(pm_caaqs, c("caaqs", "pm_24h"))
  expect_length(pm_caaqs, 4)
  expect_named(pm_caaqs, c("daily_avg", "yearly_98", "three_yr_rolling", "caaqs"))
  for (d in pm_caaqs) {
    expect_is(d, "data.frame")
  }
  expect_is(extract_caaqs(pm_caaqs), "data.frame")
  expect_is(extract_daily(pm_caaqs), "data.frame")
  expect_is(extract_yearly(pm_caaqs), "data.frame")
  expect_is(extract_three_yr_rolling(pm_caaqs), "data.frame")
})

test_that("classes and extractors for pm2.5 annual", {
  d <- readRDS("pm_raw1.rds")
  pm_caaqs <- pm_annual_caaqs(d)
  expect_is(pm_caaqs, c("caaqs", "pm_annual"))
  expect_length(pm_caaqs, 4)
  expect_named(pm_caaqs, c("daily_avg", "yearly_avg", "three_yr_rolling", "caaqs"))
  for (d in pm_caaqs) {
    expect_is(d, "data.frame")
  }
  expect_is(extract_caaqs(pm_caaqs), "data.frame")
  expect_is(extract_daily(pm_caaqs), "data.frame")
  expect_is(extract_yearly(pm_caaqs), "data.frame")
  expect_is(extract_three_yr_rolling(pm_caaqs), "data.frame")
})

test_that("pm_24h_caaqs single", {
  d <- readRDS("pm_raw1.rds")

  expect_error(expect_message(pm_caaqs <- pm_24h_caaqs(d), 
                              "Calculating PM 2.5 daily average"),
               NA)
  caaqs <- extract_caaqs(pm_caaqs)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("pm2.5_24h")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("pm2.5_24h")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "pm_24h_caaqs1.rds")
})

test_that("pm_24h_caaqs groups", {
  d <- readRDS("pm_raw2.rds")
  
  expect_error(expect_message(pm_caaqs <- pm_24h_caaqs(d, by = c("ems_id", "site")), 
                              "Calculating PM 2.5 daily average"), NA)
  caaqs <- extract_caaqs(pm_caaqs)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("pm2.5_24h")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("pm2.5_24h")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "pm_24h_caaqs2.rds")
})

test_that("pm_annual_caaqs single", {
  d <- readRDS("pm_raw1.rds")
  
  expect_error(expect_message(pm_caaqs <- pm_annual_caaqs(d), 
                              "Calculating PM 2.5 daily average"),
               NA)
  caaqs <- extract_caaqs(pm_caaqs)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("pm2.5_annual")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("pm2.5_annual")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "pm_annual_caaqs1.rds")
})

test_that("pm_annual_caaqs groups", {
  d <- readRDS("pm_raw2.rds")
  
  expect_error(expect_message(pm_caaqs <- pm_annual_caaqs(d, by = c("ems_id", "site")), 
                              "Calculating PM 2.5 daily average"), NA)
  caaqs <- extract_caaqs(pm_caaqs)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("pm2.5_annual")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("pm2.5_annual")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "pm_annual_caaqs2.rds")
})
