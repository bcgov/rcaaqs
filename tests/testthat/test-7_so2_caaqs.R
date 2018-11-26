context("so2 caaqs wrappers")

test_that("classes and extractors for so2 3yr", {
  d <- readRDS("so2_raw1.rds")
  so2_caaqs <- so2_3yr_caaqs(d)
  expect_is(so2_caaqs, c("caaqs", "so2_3yr"))
  expect_length(so2_caaqs, 4)
  expect_named(so2_caaqs, c("daily_max", "yearly_99", "three_yr_rolling", "caaqs"))
  for (d in so2_caaqs) {
    expect_is(d, "data.frame")
  }
  expect_is(extract_caaqs(so2_caaqs), "data.frame")
  expect_is(extract_daily(so2_caaqs), "data.frame")
  expect_is(extract_yearly(so2_caaqs), "data.frame")
  expect_is(extract_three_yr_rolling(so2_caaqs), "data.frame")
})

test_that("classes and extractors for so2 1yr", {
  d <- readRDS("so2_raw1.rds")
  so2_caaqs <- so2_1yr_caaqs(d)
  expect_is(so2_caaqs, c("caaqs", "so2_1yr"))
  expect_length(so2_caaqs, 3)
  expect_named(so2_caaqs, c("hourly", "yearly_hr", "caaqs"))
  for (d in so2_caaqs) {
    expect_is(d, "data.frame")
  }
  expect_is(extract_caaqs(so2_caaqs), "data.frame")
  expect_error(extract_daily(so2_caaqs))
  expect_is(extract_yearly(so2_caaqs), "data.frame")
  expect_error(extract_three_yr_rolling(so2_caaqs))
})


test_that("so2_3yr_caaqs single", {
  d <- readRDS("so2_raw1.rds")

  expect_error(expect_message(so2_caaqs <- so2_3yr_caaqs(d), 
                              "Calculating SO2 daily maximum"),
               NA)
  caaqs <- extract_caaqs(so2_caaqs)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("so2_3yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("so2_3yr")] 
                  == "Not Achieved", na.rm = TRUE))  
})

test_that("so2_3yr_caaqs groups", {
  d <- readRDS("so2_raw2.rds")
  
  expect_error(expect_message(so2_caaqs <- so2_3yr_caaqs(d, by = c("ems_id", "site")), 
                 "Calculating SO2 daily maximum"), NA)
  caaqs <- extract_caaqs(so2_caaqs)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("so2_3yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("so2_3yr")] 
                  == "Not Achieved", na.rm = TRUE))  

})

test_that("so2_1yr_caaqs single", {
  d <- readRDS("so2_raw1.rds")
  
  expect_error(expect_message(so2_caaqs <- so2_1yr_caaqs(d), 
                              "Calculating SO2 annual average"),
               NA)
  caaqs <- extract_caaqs(so2_caaqs)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("so2_1yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("so2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))  
})

test_that("so2_1yr_caaqs groups", {
  d <- readRDS("so2_raw2.rds")
  
  expect_error(expect_message(so2_caaqs <- so2_1yr_caaqs(d, by = c("ems_id", "site")), 
                              "Calculating SO2 annual average"), NA)
  caaqs <- extract_caaqs(so2_caaqs)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("so2_1yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("so2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))
})
