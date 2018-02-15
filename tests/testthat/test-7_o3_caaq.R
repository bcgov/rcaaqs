context("o3 caaq wrappers")


test_that("o3_caaq single", {
  d <- readRDS("o3_raw1.rds")

  expect_error(expect_message(caaq <- o3_caaq(d), 
                              "Calculating O3 daily maximum"),
               NA)
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("o3")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("o3")] 
                  == "Not Achieved", na.rm = TRUE))
  saveRDS(caaq, "o3_caaq1.rds")
})

test_that("o3_caaq groups", {
  d <- readRDS("o3_raw2.rds")
  
  expect_error(expect_message(caaq <- o3_caaq(d, by = c("ems_id", "site")), 
                              "Calculating O3 daily maximum"), NA)
  
  expect_true(all(is.na(caaq$metric_value[caaq$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaq$metric_value[caaq$caaqs != "Insufficient Data"])))
  expect_true(all(caaq$caaqs[caaq$metric_value <= get_std("o3")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaq$caaqs[caaq$metric_value > get_std("o3")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaq, "o3_caaq2.rds")
})

test_that("o3_caaq single returns all", {
  d <- readRDS("o3_raw1.rds")
  
  expect_error(expect_message(caaq <- o3_caaq(d, return_all = TRUE), 
                              "Calculating O3 daily maximum"),
               NA)
  expect_is(caaq, "data.frame")
  expect_length(caaq, 5)
  expect_equivalent(tidyr::unnest(caaq, caaqs), readRDS("o3_caaq1.rds"))
})

test_that("o3_caaq group returns all", {
  d <- readRDS("o3_raw2.rds")
  
  expect_error(expect_message(caaq <- o3_caaq(d, 
                                              by = c("ems_id", "site"),
                                              return_all = TRUE), 
                              "Calculating O3 daily maximum"),
               NA)
  expect_is(caaq, "data.frame")
  expect_length(caaq, 7)
  expect_equivalent(tidyr::unnest(caaq, caaqs), readRDS("o3_caaq2.rds"))
})
