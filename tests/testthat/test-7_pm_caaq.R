context("pm caaq wrappers")


test_that("pm_24h_caaq single", {
  d <- readRDS("pm_raw1.rds")

  expect_error(expect_message(caaq <- pm_24h_caaq(d), 
                              "Calculating PM 2.5 daily average"),
               NA)
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("pm2.5_24h")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("pm2.5_24h")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaq, "pm_24h_caaq1.rds")
})

test_that("pm_24h_caaq groups", {
  d <- readRDS("pm_raw2.rds")
  
  expect_error(expect_message(caaq <- pm_24h_caaq(d, by = c("ems_id", "site")), 
                              "Calculating PM 2.5 daily average"), NA)
  
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("pm2.5_24h")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("pm2.5_24h")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaq, "pm_24h_caaq2.rds")
})

test_that("pm_24h_caaq single returns all", {
  d <- readRDS("pm_raw1.rds")
  
  expect_error(expect_message(caaq <- pm_24h_caaq(d, return_all = TRUE), 
                              "Calculating PM 2.5 daily average"),
               NA)
  expect_is(caaq, "data.frame")
  expect_length(caaq, 4)
  expect_equivalent(tidyr::unnest(caaq, caaqs), readRDS("pm_24h_caaq1.rds"))
})

test_that("pm_24h_caaq group returns all", {
  d <- readRDS("pm_raw2.rds")
  
  expect_error(expect_message(caaq <- pm_24h_caaq(d, 
                                                  by = c("ems_id", "site"),
                                                  return_all = TRUE), 
                              "Calculating PM 2.5 daily average"),
               NA)
  expect_is(caaq, "data.frame")
  expect_length(caaq, 6)
  expect_equivalent(tidyr::unnest(caaq, caaqs), readRDS("pm_24h_caaq2.rds"))
})

test_that("pm_annual_caaq single", {
  d <- readRDS("pm_raw1.rds")
  
  expect_error(expect_message(caaq <- pm_annual_caaq(d), 
                              "Calculating PM 2.5 daily average"),
               NA)
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("pm2.5_annual")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("pm2.5_annual")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaq, "pm_annual_caaq1.rds")
})

test_that("pm_annual_caaq groups", {
  d <- readRDS("pm_raw2.rds")
  
  expect_error(expect_message(caaq <- pm_annual_caaq(d, by = c("ems_id", "site")), 
                              "Calculating PM 2.5 daily average"), NA)
  
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("pm2.5_annual")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("pm2.5_annual")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaq, "pm_annual_caaq2.rds")
})

test_that("pm_annaul_caaq single returns all", {
  d <- readRDS("pm_raw1.rds")
  
  expect_error(expect_message(caaq <- pm_annual_caaq(d, return_all = TRUE), 
                              "Calculating PM 2.5 daily average"),
               NA)
  expect_is(caaq, "data.frame")
  expect_length(caaq, 4)
  expect_equivalent(tidyr::unnest(caaq, caaqs), readRDS("pm_annual_caaq1.rds"))
})

test_that("pm_annual_caaq group returns all", {
  d <- readRDS("pm_raw2.rds")
  
  expect_error(expect_message(caaq <- pm_annual_caaq(d, 
                                                  by = c("ems_id", "site"),
                                                  return_all = TRUE), 
                              "Calculating PM 2.5 daily average"),
               NA)
  expect_is(caaq, "data.frame")
  expect_length(caaq, 6)
  expect_equivalent(tidyr::unnest(caaq, caaqs), readRDS("pm_annual_caaq2.rds"))
})
