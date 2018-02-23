context("so2 caaqs wrappers")


test_that("so2_3yr_caaqs single", {
  d <- readRDS("so2_raw1.rds")

  expect_error(expect_message(caaqs <- so2_3yr_caaqs(d), 
                              "Calculating SO2 daily maximum"),
               NA)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("so2_3yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("so2_3yr")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "so2_3yr_caaqs1.rds")
})

test_that("so2_3yr_caaqs groups", {
  d <- readRDS("so2_raw2.rds")
  
  expect_error(expect_message(caaqs <- so2_3yr_caaqs(d, by = c("ems_id", "site")), 
                 "Calculating SO2 daily maximum"), NA)
  
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("so2_3yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("so2_3yr")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "so2_3yr_caaqs2.rds")
})


test_that("so2_3yr_caaqs single returns all", {
  d <- readRDS("so2_raw1.rds")
  
  expect_error(expect_message(caaqs <- so2_3yr_caaqs(d, return_all = TRUE), 
                              "Calculating SO2 daily maximum"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 4)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("so2_3yr_caaqs1.rds"))
})

test_that("so2_3yr_caaqs group returns all", {
  d <- readRDS("so2_raw2.rds")
  
  expect_error(expect_message(caaqs <- so2_3yr_caaqs(d, 
                                              by = c("ems_id", "site"),
                                              return_all = TRUE), 
                              "Calculating SO2 daily maximum"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 6)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("so2_3yr_caaqs2.rds"))
})




test_that("so2_1yr_caaqs single", {
  d <- readRDS("so2_raw1.rds")
  
  expect_error(expect_message(caaqs <- so2_1yr_caaqs(d), 
                              "Calculating SO2 annual average"),
               NA)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("so2_1yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("so2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "so2_1yr_caaqs1.rds")
})

test_that("so2_1yr_caaqs groups", {
  d <- readRDS("so2_raw2.rds")
  
  expect_error(expect_message(caaqs <- so2_1yr_caaqs(d, by = c("ems_id", "site")), 
                              "Calculating SO2 annual average"), NA)
  
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("so2_1yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("so2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "so2_1yr_caaqs2.rds")
})

test_that("so2_1yr_caaqs single returns all", {
  d <- readRDS("so2_raw1.rds")
  
  expect_error(expect_message(caaqs <- so2_1yr_caaqs(d, return_all = TRUE), 
                              "Calculating SO2 annual average"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 2)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("so2_1yr_caaqs1.rds"))
})

test_that("so2_1yr_caaqs group returns all", {
  d <- readRDS("so2_raw2.rds")
  
  expect_error(expect_message(caaqs <- so2_1yr_caaqs(d, 
                                                   by = c("ems_id", "site"),
                                                   return_all = TRUE), 
                              "Calculating SO2 annual average"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 4)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("so2_1yr_caaqs2.rds"))
})
