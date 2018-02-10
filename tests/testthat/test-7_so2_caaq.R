context("so2 caaq wrappers")


test_that("so2_3yr_caaq single", {
  d <- readRDS("so2_raw1.rds")

  expect_error(expect_message(caaq <- so2_3yr_caaq(d), 
                              "Calculating SO2 daily maximum"),
               NA)
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("so2_3yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("so2_3yr")] 
                  == "Not Achieved", na.rm = TRUE))  
})

test_that("so2_3yr_caaq groups", {
  d <- readRDS("so2_raw2.rds")
  
  expect_error(expect_message(caaq <- so2_3yr_caaq(d, by = c("ems_id", "site")), 
                 "Calculating SO2 daily maximum"), NA)
  
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("so2_3yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("so2_3yr")] 
                  == "Not Achieved", na.rm = TRUE))  
})

test_that("so2_1yr_caaq single", {
  d <- readRDS("so2_raw1.rds")
  
  expect_error(expect_message(caaq <- so2_1yr_caaq(d), 
                              "Calculating SO2 annual average"),
               NA)
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("so2_1yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("so2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))  
})

test_that("so2_1yr_caaq groups", {
  d <- readRDS("so2_raw2.rds")
  
  expect_error(expect_message(caaq <- so2_1yr_caaq(d, by = c("ems_id", "site")), 
                              "Calculating SO2 annual average"), NA)
  
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("so2_1yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("so2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))  
})
