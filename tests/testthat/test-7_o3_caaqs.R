context("o3 caaqs wrappers")


test_that("o3_caaqs single", {
  d <- readRDS("o3_raw1.rds")

  expect_error(expect_message(caaqs <- o3_caaqs(d), 
                              "Calculating O3 daily maximum"),
               NA)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("o3")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("o3")] 
                  == "Not Achieved", na.rm = TRUE))
  saveRDS(caaqs, "o3_caaqs1.rds")
})

test_that("o3_caaqs groups", {
  d <- readRDS("o3_raw2.rds")
  
  expect_error(expect_message(caaqs <- o3_caaqs(d, by = c("ems_id", "site")), 
                              "Calculating O3 daily maximum"), NA)
  
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("o3")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("o3")] 
                  == "Not Achieved", na.rm = TRUE))  
  saveRDS(caaqs, "o3_caaqs2.rds")
})

test_that("o3_caaqs single returns all", {
  d <- readRDS("o3_raw1.rds")
  
  expect_error(expect_message(caaqs <- o3_caaqs(d, return_all = TRUE), 
                              "Calculating O3 daily maximum"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 5)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("o3_caaqs1.rds"))
})

test_that("o3_caaqs group returns all", {
  d <- readRDS("o3_raw2.rds")
  
  expect_error(expect_message(caaqs <- o3_caaqs(d, 
                                              by = c("ems_id", "site"),
                                              return_all = TRUE), 
                              "Calculating O3 daily maximum"),
               NA)
  expect_is(caaqs, "data.frame")
  expect_length(caaqs, 7)
  expect_equivalent(tidyr::unnest(caaqs, caaqs), readRDS("o3_caaqs2.rds"))
})
