context("no2 hourly by year")
library(magrittr)

no1 <- dplyr::filter(no2_sample_data, 
                     ems_id == "E231866")
no2 <- dplyr::filter(no2_sample_data, 
                     ems_id %in% c("E231866", "0500886"),
                     !(ems_id == "0500886" & 
                         date_time > as.Date("2013-01-05") &
                         date_time < as.Date("2013-06-30"))) %>%
  dplyr::mutate(value = replace(value, ems_id == "0500886" & 
                                  date_time > as.POSIXct("2014-01-02 00:00:00") &
                                  date_time < as.POSIXct("2014-07-03 23:00:00"),
                                NA),
                value = replace(value, ems_id == "0500886" & 
                                  date_time > as.POSIXct("2014-04-02 08:00:00") &
                                  date_time < as.POSIXct("2014-07-02 13:00:00"),
                                get_std("no2_1yr") + 30))

test_that("Runs with silently", {
  expect_silent(r1 <- no2_avg_hourly_by_year(no1))
  expect_silent(r2 <- no2_avg_hourly_by_year(no2, by = c("ems_id", "site")))
  saveRDS(r1, "no2_avg_hr_1.rds")
  saveRDS(r2, "no2_avg_hr_2.rds")
})

ret1 <- readRDS("no2_avg_hr_1.rds")
ret2 <- readRDS("no2_avg_hr_2.rds")

test_that("has correct classes", {
  for(r in list(ret1, ret2)){
    expect_is(r, "data.frame")
    expect_is(r$year, "numeric")
    expect_is(r$valid_in_year, "numeric")
    expect_is(r$quarter_1, "numeric")
    expect_is(r$quarter_2, "numeric")
    expect_is(r$quarter_3, "numeric")
    expect_is(r$quarter_4, "numeric")
    expect_is(r$avg_yearly, "numeric")
    expect_is(r$exclude, "logical")
    expect_is(r$exceed, "logical")
    expect_is(r$flag_daily_incomplete, "logical")
    expect_is(r$flag_yearly_incomplete, "logical")
  }
})

test_that("has correct dimensions", {
  nrows <- length(unique(format(no1$date_time, "%Y")))
  expect_equal(dim(ret1), c(nrows, 12))
  
  nrows <- dplyr::group_by(no2, ems_id, site) %>%
    dplyr::mutate(year = format(date_time, "%Y")) %>%
    dplyr::summarize(n = length(unique(year))) %>%
    dplyr::pull(n) %>%
    sum(.)
  expect_equal(dim(ret2), c(nrows, 14))
})

test_that("has correct data", {
  for(r in list(ret1, ret2[ret2$ems_id == "E231866",])){
    expect_equivalent(r$valid_in_year[1:3], c(0.918, 0.945, 0.947), tolerance = 0.001)
    expect_equivalent(r$quarter_1[1:3], c(0.857, 0.954, 0.930), tolerance = 0.001)
    expect_equivalent(r$quarter_2[1:3], c(0.952, 0.944, 0.955), tolerance = 0.001)
    expect_equivalent(r$quarter_3[1:3], c(0.952, 0.939, 0.951), tolerance = 0.001)
    expect_equivalent(r$quarter_4[1:3], c(0.911, 0.942, 0.952), tolerance = 0.001)
    expect_equivalent(r$avg_yearly[1:3], c(9.00, 8.40, 8.60), tolerance = 0.001)
  }
})

test_that("performs data completeness accurately", {
  for(r in list(ret1, ret2)){
    expect_true(all(!r$valid_year[(r$quarter_1 < 0.6 |
                                     r$quarter_2 < 0.6|
                                     r$quarter_3 < 0.6|
                                     r$quarter_4 < 0.6) | r$valid_in_year < 0.75]))
    expect_true(all(r$valid_year[!((r$quarter_1 < 0.6 |
                                      r$quarter_2 < 0.6|
                                      r$quarter_3 < 0.6|
                                      r$quarter_4 < 0.6) | r$valid_in_year < 0.75)]))
    expect_true(all(is.na(r$avg_yearly[!r$valid_year])))
    expect_true(all(is.na(r$flag_yearly_incomplete)))
  }
})

test_that("can exclude data rows", {
  
  expect_silent(
    high_dates <- dplyr::filter(no2, value > get_std("no2_1yr")) %>%
      dplyr::mutate(date = as.Date(date_time)) %>%
      dplyr::select(ems_id, site, date) %>%
      dplyr::distinct())
  expect_gt(nrow(high_dates), 0)
  
  expect_silent(ret3 <- no2_avg_hourly_by_year(no2, by = c("ems_id", "site"),
                                               exclude_df = high_dates, 
                                               exclude_df_dt = c("date"), 
                                               quiet = TRUE))
  
  expect_equivalent(dplyr::select(ret2, "ems_id", "site", "year", 
                                  "valid_in_year", "quarter_1", 
                                  "quarter_2", "quarter_3", "quarter_4", 
                                  "valid_year", 
                                  "flag_daily_incomplete"),
                    dplyr::select(ret3, "ems_id", "site", "year", 
                                  "valid_in_year", "quarter_1", 
                                  "quarter_2", "quarter_3", "quarter_4", 
                                  "valid_year", 
                                  "flag_daily_incomplete"))
  
  expect_false(all(ret2$avg_yearly == ret3$avg_yearly, na.rm = TRUE))
  expect_true(all(ret2$avg_yearly >= ret3$avg_yearly, na.rm = TRUE))
  expect_equal(ret3$exclude, c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equivalent(ret3$avg_yearly, c(NA, NA, 4.60, 5.70, 5.90, 5.90))
  expect_false(all(ret2$exceed == ret3$exceed))
})
