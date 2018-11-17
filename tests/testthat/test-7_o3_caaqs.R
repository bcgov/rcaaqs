context("o3 caaqs wrappers")

test_that("classes and extractors for o3", {
  d <- readRDS("o3_raw1.rds")
  o3_caaqs <- o3_caaqs(d)
  expect_is(o3_caaqs, c("caaqs", "o3"))
  expect_length(o3_caaqs, 4)
  expect_named(o3_caaqs, c("daily_max", "ann_4th_highest", "three_yr_rolling", "caaqs"))
  for (d in o3_caaqs) {
    expect_is(d, "data.frame")
  }
  expect_is(extract_caaqs(o3_caaqs), "data.frame")
  expect_is(extract_daily(o3_caaqs), "data.frame")
  expect_is(extract_yearly(o3_caaqs), "data.frame")
  expect_is(extract_three_yr_rolling(o3_caaqs), "data.frame")
})

test_that("o3_caaqs single", {
  d <- readRDS("o3_raw1.rds")

  expect_error(expect_message(o3_caaqs <- o3_caaqs(d), 
                              "Calculating O3 daily maximum"),
               NA)
  caaqs <- extract_caaqs(o3_caaqs)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("o3")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("o3")] 
                  == "Not Achieved", na.rm = TRUE))
})

test_that("o3_caaqs groups", {
  d <- readRDS("o3_raw2.rds")
  
  expect_error(expect_message(o3_caaqs <- o3_caaqs(d, by = c("ems_id", "site")), 
                              "Calculating O3 daily maximum"), NA)
  
  caaqs <- extract_caaqs(o3_caaqs)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("o3")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("o3")] 
                  == "Not Achieved", na.rm = TRUE))  
})
