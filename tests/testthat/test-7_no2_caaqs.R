context("no2 caaqs wrappers")


test_that("no2_3yr_caaqs single", {
  d <- readRDS("no2_raw1.rds")

  expect_error(expect_message(caaqs <- no2_3yr_caaqs(d), 
                              "Calculating NO2 daily maximum"),
               NA)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("no2_3yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("no2_3yr")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "no2_3yr_caaqs1.rds")
})

test_that("no2_3yr_caaqs groups", {
  d <- readRDS("no2_raw2.rds")
  
  expect_error(expect_message(caaqs <- no2_3yr_caaqs(d, by = c("ems_id", "site")), 
                 "Calculating NO2 daily maximum"), NA)
  
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("no2_3yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("no2_3yr")] 
                  == "Not Achieved", na.rm = TRUE)) 
  saveRDS(caaqs, "no2_3yr_caaqs2.rds")
})

test_that("no2_3yr_caaqs single returns all", {
  d <- readRDS("no2_raw1.rds")
  
  expect_error(expect_message(caaqs <- no2_3yr_caaqs(d, return_all = TRUE), 
                              "Calculating NO2 daily maximum"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 4)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("no2_3yr_caaqs1.rds"))
})

test_that("no2_3yr_caaqs group returns all", {
  d <- readRDS("no2_raw2.rds")
  
  expect_error(expect_message(caaqs <- no2_3yr_caaqs(d, 
                                                   by = c("ems_id", "site"),
                                                   return_all = TRUE), 
                              "Calculating NO2 daily maximum"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 6)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("no2_3yr_caaqs2.rds"))
})




test_that("no2_1yr_caaqs single", {
  d <- readRDS("no2_raw1.rds")
  
  expect_error(expect_message(caaqs <- no2_1yr_caaqs(d), 
                              "Calculating NO2 annual average"),
               NA)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("no2_1yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("no2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "no2_1yr_caaqs1.rds")
})

test_that("no2_1yr_caaqs groups", {
  d <- readRDS("no2_raw2.rds")
  
  expect_error(expect_message(caaqs <- no2_1yr_caaqs(d, by = c("ems_id", "site")), 
                              "Calculating NO2 annual average"), NA)
  
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("no2_1yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("no2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "no2_1yr_caaqs2.rds")
})

test_that("no2_1yr_caaqs single returns all", {
  d <- readRDS("no2_raw1.rds")
  
  expect_error(expect_message(caaqs <- no2_1yr_caaqs(d, return_all = TRUE), 
                              "Calculating NO2 annual average"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 2)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("no2_1yr_caaqs1.rds"))
})

test_that("no2_1yr_caaqs group returns all", {
  d <- readRDS("no2_raw2.rds")
  
  expect_error(expect_message(caaqs <- no2_1yr_caaqs(d, 
                                                   by = c("ems_id", "site"),
                                                   return_all = TRUE), 
                              "Calculating NO2 annual average"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 4)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("no2_1yr_caaqs2.rds"))
})

