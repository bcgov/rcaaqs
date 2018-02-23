context("pm caaqs wrappers")


test_that("pm_24h_caaqs single", {
  d <- readRDS("pm_raw1.rds")

  expect_error(expect_message(caaqs <- pm_24h_caaqs(d), 
                              "Calculating PM 2.5 daily average"),
               NA)
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
  
  expect_error(expect_message(caaqs <- pm_24h_caaqs(d, by = c("ems_id", "site")), 
                              "Calculating PM 2.5 daily average"), NA)
  
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("pm2.5_24h")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("pm2.5_24h")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "pm_24h_caaqs2.rds")
})

test_that("pm_24h_caaqs single returns all", {
  d <- readRDS("pm_raw1.rds")
  
  expect_error(expect_message(caaqs <- pm_24h_caaqs(d, return_all = TRUE), 
                              "Calculating PM 2.5 daily average"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 4)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("pm_24h_caaqs1.rds"))
})

test_that("pm_24h_caaqs group returns all", {
  d <- readRDS("pm_raw2.rds")
  
  expect_error(expect_message(caaqs <- pm_24h_caaqs(d, 
                                                  by = c("ems_id", "site"),
                                                  return_all = TRUE), 
                              "Calculating PM 2.5 daily average"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 6)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("pm_24h_caaqs2.rds"))
})

test_that("pm_annual_caaqs single", {
  d <- readRDS("pm_raw1.rds")
  
  expect_error(expect_message(caaqs <- pm_annual_caaqs(d), 
                              "Calculating PM 2.5 daily average"),
               NA)
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
  
  expect_error(expect_message(caaqs <- pm_annual_caaqs(d, by = c("ems_id", "site")), 
                              "Calculating PM 2.5 daily average"), NA)
  
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("pm2.5_annual")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("pm2.5_annual")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "pm_annual_caaqs2.rds")
})

test_that("pm_annaul_caaqs single returns all", {
  d <- readRDS("pm_raw1.rds")
  
  expect_error(expect_message(caaqs <- pm_annual_caaqs(d, return_all = TRUE), 
                              "Calculating PM 2.5 daily average"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 4)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("pm_annual_caaqs1.rds"))
})

test_that("pm_annual_caaqs group returns all", {
  d <- readRDS("pm_raw2.rds")
  
  expect_error(expect_message(caaqs <- pm_annual_caaqs(d, 
                                                  by = c("ems_id", "site"),
                                                  return_all = TRUE), 
                              "Calculating PM 2.5 daily average"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 6)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("pm_annual_caaqs2.rds"))
})
