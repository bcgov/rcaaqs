context("o3 caaqs wrappers")

d1 <- readRDS("o3_raw1.rds")
d2 <- readRDS("o3_raw2.rds")
o3_caaqs_one <- NULL
o3_caaqs_mult <- NULL

test_that("classes and extractors for o3 3yr", {
  expect_message(o3_caaqs_one <<- o3_caaqs(d1), 
                 "Calculating O3 daily maximum")
  expect_is(o3_caaqs_one, c("caaqs", "o3"))
  expect_length(o3_caaqs_one, 4)
  expect_named(o3_caaqs_one, c("daily_max", "ann_4th_highest", "three_yr_rolling", "caaqs"))
  for (d in o3_caaqs_one) {
    expect_is(d, "data.frame")
  }
  expect_is(get_caaqs(o3_caaqs_one), "data.frame")
  expect_is(get_daily(o3_caaqs_one),"data.frame")
  expect_is(get_yearly(o3_caaqs_one), "data.frame")
  expect_is(get_three_yr_rolling(o3_caaqs_one), "data.frame")
})

test_that("o3_3yr_caaqs single", {
  caaqs <- get_caaqs(o3_caaqs_one)
  expect_length(caaqs$caaqs, 3)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("o3")] 
                  == "Achieved", na.rm = TRUE))  
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("o3")] 
                  == "Not Achieved", na.rm = TRUE))
  expect_length(get_caaqs(o3_caaqs_one), 10)
  expect_length(get_daily(o3_caaqs_one), 6)
  expect_length(get_yearly(o3_caaqs_one), 11)
  expect_length(get_three_yr_rolling(o3_caaqs_one), 18)
})

test_that("o3_3yr_caaqs groups", {
  expect_message(
    o3_caaqs_mult <<- o3_caaqs(d2, by = c("ems_id", "site")), 
    "Calculating O3 daily maximum"
  )
  caaqs <- get_caaqs(o3_caaqs_mult)
  expect_length(caaqs$caaqs, 6)
  expect_true(all(is.na(caaqs$metric_value[caaqs$caaqs == "Insufficient Data"])))
  expect_true(all(!is.na(caaqs$metric_value[caaqs$caaqs != "Insufficient Data"])))
  expect_true(all(caaqs$caaqs[caaqs$metric_value <= get_std("o3")] 
                  == "Achieved", na.rm = TRUE))
  expect_true(all(caaqs$caaqs[caaqs$metric_value > get_std("o3")] 
                  == "Not Achieved", na.rm = TRUE)) 
  expect_length(get_caaqs(o3_caaqs_mult), 12)
  expect_length(get_daily(o3_caaqs_mult), 8)
  expect_length(get_yearly(o3_caaqs_mult), 13)
  expect_length(get_three_yr_rolling(o3_caaqs_mult), 20)
})


test_that("caaqs_management for o3 3yr", {
  expect_is(o3_caaqs_one_mgmt <- caaqs_management(o3_caaqs_one), 
            "caaqs_mgmt")
  expect_length(o3_caaqs_one_mgmt, 4)
  expect_length(get_caaqs(o3_caaqs_one_mgmt), 13)
  expect_length(get_daily(o3_caaqs_one_mgmt), 6)
  expect_length(get_yearly(o3_caaqs_one_mgmt), 14)
  expect_length(get_three_yr_rolling(o3_caaqs_one_mgmt), 21)
})

test_that("caaqs_management for o3 3yr with groups", {
  expect_is(o3_caaqs_mult_mgmt <- caaqs_management(o3_caaqs_mult), 
            "caaqs_mgmt")
  expect_length(o3_caaqs_mult_mgmt, 4)
  expect_length(get_caaqs(o3_caaqs_mult_mgmt), 15)
  expect_length(get_daily(o3_caaqs_mult_mgmt), 8)
  expect_length(get_yearly(o3_caaqs_mult_mgmt), 16)
  expect_length(get_three_yr_rolling(o3_caaqs_mult_mgmt), 23)
})
