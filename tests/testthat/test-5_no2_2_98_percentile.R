context("no2 98 percentile")

# Load data and create artificial exceedances
no1 <- readRDS("no2_daily1.rds")
no2 <- readRDS("no2_daily2.rds") %>%
  dplyr::mutate(max_24h = replace(max_24h, ems_id == "0500886" &
                                    ((date > as.Date("2013-01-20") &
                                    date < as.Date("2013-02-20")) |
                                    (date > as.Date("2013-09-01") &
                                       date < as.Date("2013-11-01"))), 
                                  NA),
                max_24h = replace(max_24h, ems_id == "0500886" &
                                    date > as.Date("2013-06-20") &
                                    date < as.Date("2013-07-01"), 
                                  get_std("no2_3yr") + 1),
                max_24h = replace(max_24h, ems_id == "0500886" &
                                    date > as.Date("2015-06-20") &
                                    date < as.Date("2015-07-01"), 
                                  get_std("no2_3yr") + 1))



test_that("Runs with silently", {
  expect_silent(r1 <- no2_yearly_98(no1))
  expect_silent(r2 <- no2_yearly_98(no2, by = c("ems_id", "site")))
  saveRDS(r1, "no2_yearly98_1.rds")
  saveRDS(r2, "no2_yearly98_2.rds")
})

ret1 <- readRDS("no2_yearly98_1.rds")
ret2 <- readRDS("no2_yearly98_2.rds")

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
    expect_is(r$flag_yearly_incomplete, "logical")
  }
})

test_that("has correct dimensions", {
  nrows <- length(unique(format(no1$date, "%Y")))
  expect_equal(dim(ret1), c(nrows, 12))
  
  nrows <- dplyr::mutate(no2, year = format(date, "%Y")) %>%
    dplyr::summarize(n = length(unique(year))) %>%
    dplyr::pull(n) %>%
    sum(.)
  expect_equal(dim(ret2), c(nrows, 14))
})

test_that("has correct data", {
  for(r in list(ret1, ret2[ret2$ems_id == "E231866",])){
    expect_equivalent(r$valid_in_year[1:3], c(0.956, 0.981, 0.989), tolerance = 0.001)
    expect_equivalent(r$quarter_1[1:3], c(0.878, 1, 0.967), tolerance = 0.001)
    expect_equivalent(r$quarter_2[1:3], c(1, 0.978, 1), tolerance = 0.001)
    expect_equivalent(r$quarter_3[1:3], c(1, 0.978, 0.989), tolerance = 0.001)
    expect_equivalent(r$quarter_4[1:3], c(0.946, 0.967, 1), tolerance = 0.001)
    expect_equivalent(r$ann_98_percentile[1:3], c(36.7, 35.4, 37.3), tolerance = 0.001)
  }
})

test_that("performs data completeness accurately", {
  for(r in list(ret1, ret2)){
    expect_true(all(!r$valid_year[(r$quarter_1 < 0.6 |
                                     r$quarter_2 < 0.6|
                                     r$quarter_3 < 0.6|
                                     r$quarter_4 < 0.6) | 
                                    r$valid_in_year < 0.75]))
    expect_true(all(r$valid_year[!((r$quarter_1 < 0.6 |
                                     r$quarter_2 < 0.6|
                                     r$quarter_3 < 0.6|
                                     r$quarter_4 < 0.6) | 
                                   r$valid_in_year < 0.75)]))
    expect_true(all(is.na(r$ann_98_percentile[!r$valid_year])))
    expect_true(all(!is.na(r$ann_98_percentile[r$valid_year])))
    expect_true(all(is.na(r$flag_yearly_incomplete)))
  }
})

test_that("can exclude data rows", {
  
  expect_silent(
    high_dates <- dplyr::filter(no2, max_24h > get_std("no2_3yr")) %>%
      dplyr::select(ems_id, site, date))
  expect_gt(nrow(high_dates), 0)
  
  expect_silent(ret3 <- no2_yearly_98(no2, by = c("ems_id", "site"),
                                      exclude_df = high_dates, 
                                      exclude_df_dt = c("date"), quiet = TRUE))
  
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
  
  expect_false(all(ret2$ann_98_percentile == ret3$ann_98_percentile, na.rm = TRUE))
  expect_true(all(ret2$ann_98_percentile >= ret3$ann_98_percentile, na.rm = TRUE))
  expect_equal(ret3$excluded, c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equivalent(ret3$ann_98_percentile, c(NA, NA, 30.9, 36.7, 35.4, 37.3))
  expect_false(all(ret2$exceed == ret3$exceed))
})
