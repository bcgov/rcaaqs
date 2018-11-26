context("no2 caaqs wrappers")

test_that("classes and extractors for no2 3yr", {
  d <- readRDS("no2_raw1.rds")
  no2_caaqs <- no2_3yr_caaqs(d)
  expect_is(no2_caaqs, c("caaqs", "no2_3yr"))
  expect_length(no2_caaqs, 4)
  expect_named(no2_caaqs, c("daily_max", "yearly_98", "three_yr_rolling", "caaqs"))
  for (d in no2_caaqs) {
    expect_is(d, "data.frame")
  }
  expect_is(extract_caaqs(no2_caaqs), "data.frame")
  expect_is(extract_daily(no2_caaqs), "data.frame")
  expect_is(extract_yearly(no2_caaqs), "data.frame")
  expect_is(extract_three_yr_rolling(no2_caaqs), "data.frame")
})

test_that("classes and extractors for no2 1yr", {
  d <- readRDS("no2_raw1.rds")
  no2_caaqs <- no2_1yr_caaqs(d)
  expect_is(no2_caaqs, c("caaqs", "no2_1yr"))
  expect_length(no2_caaqs, 3)
  expect_named(no2_caaqs, c("hourly", "yearly_hr", "caaqs"))
  for (d in no2_caaqs) {
    expect_is(d, "data.frame")
  }
  expect_is(extract_caaqs(no2_caaqs), "data.frame")
  expect_error(extract_daily(no2_caaqs))
  expect_is(extract_yearly(no2_caaqs), "data.frame")
  expect_error(extract_three_yr_rolling(no2_caaqs))
})

test_that("no2_3yr_caaqs single", {
  d <- readRDS("no2_raw1.rds")
  expect_error(expect_message(no2_caaqs <- no2_3yr_caaqs(d), 
                              "Calculating NO2 daily maximum"),
               NA)
  caaqs <- extract_caaqs(no2_caaqs)
  expect_length(caaqs$caaqs, 3)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("no2_3yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("no2_3yr")] 
                  == "Not Achieved", na.rm = TRUE))
})

test_that("no2_3yr_caaqs groups", {
  d <- readRDS("no2_raw2.rds")
  
  expect_error(expect_message(no2_caaqs <- no2_3yr_caaqs(d, by = c("ems_id", "site")), 
                 "Calculating NO2 daily maximum"), NA)
  caaqs <- extract_caaqs(no2_caaqs)
  expect_length(caaqs$caaqs, 6)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("no2_3yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("no2_3yr")] 
                  == "Not Achieved", na.rm = TRUE)) 
})

test_that("no2_1yr_caaqs single", {
  d <- readRDS("no2_raw1.rds")
  
  expect_error(expect_message(no2_caaqs <- no2_1yr_caaqs(d), 
                              "Calculating NO2 annual average"),
               NA)
  caaqs <- extract_caaqs(no2_caaqs)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("no2_1yr")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("no2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))
  expect_is(caaqs, "data.frame")
  expect_length(caaqs$caaqs, 3)
})

test_that("no2_1yr_caaqs groups", {
  d <- readRDS("no2_raw2.rds")
  
  expect_error(expect_message(no2_caaqs <- no2_1yr_caaqs(d, by = c("ems_id", "site")), 
                              "Calculating NO2 annual average"), NA)
  
  caaqs <- extract_caaqs(no2_caaqs)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("no2_1yr")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("no2_1yr")] 
                  == "Not Achieved", na.rm = TRUE))  
  expect_is(caaqs, "data.frame")
  expect_length(caaqs$caaqs, 6)
})
