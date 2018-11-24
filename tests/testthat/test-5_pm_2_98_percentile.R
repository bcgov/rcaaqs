context("pm 98 percentile")

# Load data and create artificial exceedances
pm1 <- readRDS("pm_daily1.rds")
pm2 <- readRDS("pm_daily2.rds") %>%
  dplyr::mutate(avg_24h = replace(avg_24h,
                                  ems_id == "0310162" &
                                  date > as.Date("2012-04-01") &
                                    date < as.Date("2012-05-01"), 
                                  get_std("pm2.5_24h") + 1),
                avg_24h = replace(avg_24h,
                                  ems_id == "0310162" &
                                    date > as.Date("2011-04-01") &
                                    date < as.Date("2011-05-01"), 
                                  get_std("pm2.5_24h") + 1))

test_that("Runs with silently", {
  expect_silent(r1 <- pm_yearly_98(pm1))
  expect_silent(r2 <- pm_yearly_98(pm2, by = c("ems_id", "site")))
  saveRDS(r1, "pm_yearly98_1.rds")
  saveRDS(r2, "pm_yearly98_2.rds")
})

ret1 <- readRDS("pm_yearly98_1.rds")
ret2 <- readRDS("pm_yearly98_2.rds")

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
    expect_is(r$exceed, "logical")
    expect_is(r$flag_daily_incomplete, "logical")
    expect_is(r$flag_yearly_incomplete, "logical")
  }
})

test_that("has correct dimensions", {
  nrows <- length(unique(format(pm1$date, "%Y")))
  expect_equal(dim(ret1), c(nrows, 12))
  
  nrows <- dplyr::mutate(pm2, year = format(date, "%Y")) %>%
    dplyr::summarize(n = length(unique(year))) %>%
    dplyr::pull(n) %>%
    sum(.)
  expect_equal(dim(ret2), c(nrows, 14))
})

test_that("has correct data", {
  for(r in list(ret1, ret2[ret2$ems_id == "0220205",])){
    expect_equivalent(r$valid_in_year[1:3], c(0.770, 0.934, 0.690), tolerance = 0.001)
    expect_equivalent(r$quarter_1[1:3], c(0.344, 0.978, 0.578), tolerance = 0.001)
    expect_equivalent(r$quarter_2[1:3], c(0.868, 1, 0.176), tolerance = 0.001)
    expect_equivalent(r$quarter_3[1:3], c(0.859, 0.989, 1), tolerance = 0.001)
    expect_equivalent(r$quarter_4[1:3], c(1, 0.772, 1), tolerance = 0.001)
    expect_equivalent(r$ann_98_percentile[1:3], c(NA, 8.80, NA), tolerance = 0.001)
  }
})

test_that("performs data completeness accurately", {
  for(r in list(ret1, ret2)){
    expect_true(all(is.na(r$flag_daily_incomplete)))
    expect_true(all(is.na(r$ann_98_percentile[(r$quarter_1 < 0.6 |
                                                  r$quarter_2 < 0.6|
                                                  r$quarter_3 < 0.6|
                                                  r$quarter_4 < 0.6) & !r$exceed])))
    expect_true(all(r$flag_yearly_incomplete[(r$quarter_1 < 0.6 |
                                                r$quarter_2 < 0.6|
                                                r$quarter_3 < 0.6|
                                                r$quarter_4 < 0.6) & r$exceed]))
  }
  
  expect_true(all(!ret1$exceed))
  expect_true(any(ret2$exceed))
})

test_that("can exclude data rows", {

  high_dates <- dplyr::filter(pm2, avg_24h > get_std("pm2.5_24h")) %>%
    dplyr::select(ems_id, site, date)

  expect_silent(ret3 <- pm_yearly_98(pm2, by = c("ems_id", "site"),
                                     exclude_df = high_dates, 
                                     exclude_df_dt = c("date"), 
                                     quiet = TRUE))

  expect_equivalent(dplyr::select(ret2, "ems_id", "site", "year", 
                                  "valid_in_year", "quarter_1", 
                                  "quarter_2", "quarter_3", "quarter_4", 
                                  "valid_quarters", "valid_year", 
                                  "flag_daily_incomplete"),
                    dplyr::select(ret3, "ems_id", "site", "year", 
                                  "valid_in_year", "quarter_1", 
                                  "quarter_2", "quarter_3", "quarter_4", 
                                  "valid_quarters", "valid_year", 
                                  "flag_daily_incomplete"))
  
  expect_false(all(ret2$ann_98_percentile == ret3$ann_98_percentile, na.rm = TRUE))
  expect_true(all(ret2$ann_98_percentile >= ret3$ann_98_percentile, na.rm = TRUE))
  expect_is(ret3$excluded, "logical")
  expect_equal(ret3$excluded, c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equivalent(ret3$ann_98_percentile, c(NA, 8.80, NA, 10.4, NA, 13.6))
  expect_false(all(ret2$exceed == ret3$exceed))
})
