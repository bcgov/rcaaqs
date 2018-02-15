context("no2 three year rolling average")

no1 <- readRDS("no2_yearly98_1.rds")
no2 <- readRDS("no2_yearly98_2.rds")

test_that("Runs silently", {
  expect_silent(r1 <- no2_three_yr_avg(no1))
  expect_silent(r2 <- no2_three_yr_avg(no2, by = c("ems_id", "site")))
  
  saveRDS(r1, "no2_3yr_1.rds")
  saveRDS(r2, "no2_3yr_2.rds")
})

ret1 <- readRDS("no2_3yr_1.rds")
ret2 <- readRDS("no2_3yr_2.rds")

test_that("has correct classes", {
  for(r in list(ret1, ret2)){
    expect_is(r, "data.frame")
    expect_is(r$year, "numeric")
    expect_is(r$valid_in_year, "numeric")
    expect_is(r$quarter_1, "numeric")
    expect_is(r$quarter_2, "numeric")
    expect_is(r$quarter_3, "numeric")
    expect_is(r$quarter_4, "numeric")
    expect_is(r$ann_98_percentile, "numeric")
    expect_is(r$excluded, "logical")
    expect_is(r$exceed, "logical")
    expect_is(r$flag_daily_incomplete, "logical")
    expect_is(r$no2_metric, "numeric")
    expect_is(r$n_val, "numeric")
    expect_is(r$flag_two_of_three_years, "logical")
  }
})

test_that("has correct dimensions", {
  nrows <- nrow(no1)
  expect_equal(dim(ret1), c(nrows, 19))
  
  nrows <- nrow(no2)
  expect_equal(dim(ret2), c(nrows, 21))
})

test_that("has correct data", {
  for(r in list(ret1, ret2[ret2$ems_id == "E231866",])){
    expect_equivalent(r$valid_in_year[1:3], c(0.956, 0.981, 0.989), tolerance = 0.001)
    expect_equivalent(r$quarter_1[1:3], c(0.878, 1, 0.967), tolerance = 0.001)
    expect_equivalent(r$quarter_2[1:3], c(1, 0.978, 1), tolerance = 0.001)
    expect_equivalent(r$quarter_3[1:3], c(1, 0.978, 0.989), tolerance = 0.001)
    expect_equivalent(r$quarter_4[1:3], c(0.946, 0.967, 1), tolerance = 0.001)
    expect_equivalent(r$ann_98_percentile[1:3], c(36.7, 35.4, 37.3), tolerance = 0.001)
    expect_equivalent(r$n_val[1:3], c(1, 2, 3))
    expect_equal(r$flag_two_of_three_years[1:3], c(FALSE, TRUE, FALSE))
    expect_equal(r$no2_metric[1:3], as.numeric(c(NA, 36.1, 36.5)))
    expect_equal(r$min_year, c(2013, 2013, 2013))
    expect_equal(r$max_year, c(2013, 2014, 2015))
  }
})

test_that("performs data completeness accurately", {
  for(r in list(ret1, ret2)){
    expect_true(all(is.na(r$no2_metric[r$n_val < 2])))
    expect_true(all(r$flag_two_of_three_years[r$n_val == 2]))
  }
})
