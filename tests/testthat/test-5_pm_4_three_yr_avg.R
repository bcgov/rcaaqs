context("pm three year rolling average")

pm1 <- readRDS("pm_yearlyavg_1.rds")
pm2 <- readRDS("pm_yearlyavg_2.rds")

test_that("Runs silently and same for both metrics", {
  pm1$ann_98_percentile <- pm1$ann_avg
  pm2$ann_98_percentile <- pm2$ann_avg
  
  # Annual
  expect_silent(r1 <- pm_three_yr_avg(pm1, val = "ann_avg"))
  expect_silent(r2 <- pm_three_yr_avg(pm2, val = "ann_avg",
                                      by = c("ems_id", "site")))
  
  # 98th percentile
  expect_silent(r3 <- pm_three_yr_avg(pm1))
  expect_silent(r4 <- pm_three_yr_avg(pm2, by = c("ems_id", "site")))
  
  # Expect whole numbers for 98_percentile
  expect_true(all(r3$pm_metric %% 1 == 0 | is.na(r3$pm_metric)))
  expect_true(all(r4$pm_metric %% 1 == 0 | is.na(r3$pm_metric)))
  
  # Expect equal if all rounded
  r1$pm_metric <- round_caaqs(r1$pm_metric, 0)
  r2$pm_metric <- round_caaqs(r2$pm_metric, 0)
  
  for(n in names(r1)) expect_equal(r1[[n]], r3[[n]], info = paste0("Column: ", n))
  for(n in names(r2)) expect_equal(r2[[n]], r4[[n]], info = paste0("Column: ", n))
  
  r1$ann_98_percentile <- NULL
  r2$ann_98_percentile <- NULL
  
  saveRDS(r1, "pm_3yr_1.rds")
  saveRDS(r2, "pm_3yr_2.rds")
})

ret1 <- readRDS("pm_3yr_1.rds")
ret2 <- readRDS("pm_3yr_2.rds")

test_that("has correct classes", {
  for(r in list(ret1, ret2)){
    expect_is(r, "data.frame")
    expect_is(r$year, "numeric")
    expect_is(r$valid_in_year, "numeric")
    expect_is(r$quarter_1, "numeric")
    expect_is(r$quarter_2, "numeric")
    expect_is(r$quarter_3, "numeric")
    expect_is(r$quarter_4, "numeric")
    expect_is(r$ann_avg, "numeric")
    expect_is(r$excluded, "logical")
    expect_is(r$exceed, "logical")
    expect_is(r$flag_daily_incomplete, "logical")
    expect_is(r$pm_metric, "numeric")
    expect_is(r$n_val, "numeric")
    expect_is(r$flag_two_of_three_years, "logical")
  }
})

test_that("has correct dimensions", {
  nrows <- nrow(pm1)
  expect_equal(dim(ret1), c(nrows, 20))
  
  nrows <- nrow(pm2)
  expect_equal(dim(ret2), c(nrows, 22))
})

test_that("has correct data", {
  for(r in list(ret1, ret2[ret2$ems_id == "0220205",])){
    expect_equivalent(r$valid_in_year[1:3], c(0.770, 0.934, 0.690), tolerance = 0.001)
    expect_equivalent(r$quarter_1[1:3], c(0.344, 0.978, 0.578), tolerance = 0.001)
    expect_equivalent(r$quarter_2[1:3], c(0.868, 1, 0.176), tolerance = 0.001)
    expect_equivalent(r$quarter_3[1:3], c(0.859, 0.989, 1), tolerance = 0.001)
    expect_equivalent(r$quarter_4[1:3], c(1, 0.772, 1), tolerance = 0.001)
    expect_equivalent(r$ann_avg[1:3], c(NA, 2.7, NA), tolerance = 0.001)
    expect_equivalent(r$n_val[1:3], c(0, 1, 1))
    expect_equal(r$flag_two_of_three_years[1:3], c(FALSE, FALSE, FALSE))
    expect_equal(r$pm_metric[1:3], as.numeric(c(NA, NA, NA)))
  }
})

test_that("performs data completeness accurately", {
  for(r in list(ret1, ret2)){
    expect_true(all(is.na(r$flag_daily_incomplete)))
    expect_true(all(is.na(r$pm_metric[r$n_val < 2])))
    expect_true(all(r$flag_two_of_three_years[r$n_val == 2]))
  }
})
