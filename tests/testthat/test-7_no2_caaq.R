context("no2 caaq wrappers")


test_that("no2_3yr_caaq single", {
  d <- readRDS("no2_raw1.rds")

  expect_error(expect_message(caaq <- no2_3yr_caaq(d), 
                              "Calculating NO2 daily maximum"),
               NA)
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("no2_3yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("no2_3yr")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaq, "no2_3yr_caaq1.rds")
})

test_that("no2_3yr_caaq groups", {
  d <- readRDS("no2_raw2.rds")
  
  expect_error(expect_message(caaq <- no2_3yr_caaq(d, by = c("ems_id", "site")), 
                 "Calculating NO2 daily maximum"), NA)
  
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("no2_3yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("no2_3yr")] 
                  == "Not Achieved", na.rm = TRUE)) 
  saveRDS(caaq, "no2_3yr_caaq2.rds")
})

test_that("no2_3yr_caaq single returns all", {
  d <- readRDS("no2_raw1.rds")
  
  expect_error(expect_message(caaq <- no2_3yr_caaq(d, return_all = TRUE), 
                              "Calculating NO2 daily maximum"),
               NA)
  expect_is(caaq, "data.frame")
  expect_length(caaq, 4)
  expect_equivalent(tidyr::unnest(caaq, caaqs), readRDS("no2_3yr_caaq1.rds"))
})

test_that("no2_3yr_caaq group returns all", {
  d <- readRDS("no2_raw2.rds")
  
  expect_error(expect_message(caaq <- no2_3yr_caaq(d, 
                                                   by = c("ems_id", "site"),
                                                   return_all = TRUE), 
                              "Calculating NO2 daily maximum"),
               NA)
  expect_is(caaq, "data.frame")
  expect_length(caaq, 6)
  expect_equivalent(tidyr::unnest(caaq, caaqs), readRDS("no2_3yr_caaq2.rds"))
})




test_that("no2_1yr_caaq single", {
  d <- readRDS("no2_raw1.rds")
  
  expect_error(expect_message(caaq <- no2_1yr_caaq(d), 
                              "Calculating NO2 annual average"),
               NA)
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("no2_1yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("no2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaq, "no2_1yr_caaq1.rds")
})

test_that("no2_1yr_caaq groups", {
  d <- readRDS("no2_raw2.rds")
  
  expect_error(expect_message(caaq <- no2_1yr_caaq(d, by = c("ems_id", "site")), 
                              "Calculating NO2 annual average"), NA)
  
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("no2_1yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("no2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaq, "no2_1yr_caaq2.rds")
})

test_that("no2_1yr_caaq single returns all", {
  d <- readRDS("no2_raw1.rds")
  
  expect_error(expect_message(caaq <- no2_1yr_caaq(d, return_all = TRUE), 
                              "Calculating NO2 annual average"),
               NA)
  expect_is(caaq, "data.frame")
  expect_length(caaq, 2)
  expect_equivalent(tidyr::unnest(caaq, caaqs), readRDS("no2_1yr_caaq1.rds"))
})

test_that("no2_1yr_caaq group returns all", {
  d <- readRDS("no2_raw2.rds")
  
  expect_error(expect_message(caaq <- no2_1yr_caaq(d, 
                                                   by = c("ems_id", "site"),
                                                   return_all = TRUE), 
                              "Calculating NO2 annual average"),
               NA)
  expect_is(caaq, "data.frame")
  expect_length(caaq, 4)
  expect_equivalent(tidyr::unnest(caaq, caaqs), readRDS("no2_1yr_caaq2.rds"))
})

