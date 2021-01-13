context("pm average daily")
library(magrittr)

pm1 <- dplyr::filter(pm25_sample_data, 
                     ems_id == "0220205")
pm2 <- dplyr::filter(pm25_sample_data, 
                     ems_id %in% c("0220205", "0310162"),
                     !(ems_id == "0310162" & 
                         date_time > as.Date("2012-05-01") &
                         date_time < as.Date("2012-06-30")))

saveRDS(pm1, "pm_raw1.rds")
saveRDS(pm2, "pm_raw2.rds")

test_that("Runs with silently", {
  expect_silent(r1 <- pm_daily_avg(pm1))
  expect_silent(r2 <- pm_daily_avg(pm2, by = c("ems_id", "site")))
  saveRDS(r1, "pm_daily1.rds")
  saveRDS(r2, "pm_daily2.rds")
})

test_that("a different name for the date_time col can be used", {
  res <- pm25_sample_data %>% 
    dplyr::rename(DATE_TIME = date_time) %>% 
    pm_annual_caaqs(dt = "DATE_TIME", by = c("ems_id", "site"))
  expect_is(res, "caaqs")
})

ret1 <- readRDS("pm_daily1.rds")
ret2 <- readRDS("pm_daily2.rds")

test_that("has correct classes", {
  for(r in list(ret1, ret2)){
    expect_is(r, "data.frame")
    expect_is(r$date, "Date")
    expect_is(r$n_readings, "integer")
    expect_is(r$avg_24h, "numeric")
    expect_is(r$exceed, "logical")
    expect_is(r$valid, "logical")
    expect_is(r$flag_daily_incomplete, "logical")
  }
})

test_that("has correct dimensions", {
  nrows <- dplyr::group_by(pm1, .data$ems_id) %>% 
    dplyr::mutate(date = lubridate::as_date(date_time)) %>%
    tidyr::complete(date = tidyr::full_seq(date, 1)) %>%
    dplyr::summarize(n = length(unique(date))) %>%
    dplyr::pull(n)
  expect_equal(dim(ret1), c(nrows, 6))
  
  nrows <- dplyr::group_by(pm2, .data$ems_id) %>% 
    dplyr::mutate(date = lubridate::as_date(date_time)) %>%
    tidyr::complete(date = tidyr::full_seq(date, 1)) %>%
    dplyr::summarize(n = length(unique(date))) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(n = sum(n)) %>%
    dplyr::pull(n)
  expect_equal(dim(ret2), c(nrows, 8))
})

test_that("has correct data", {
  for(r in list(ret1, ret2[ret2$ems_id == "0220205",])){
    expect_equal(r$n_readings[1:5], c(24, 22, 24, 24, 24))
    expect_equal(r$avg_24h[1:5], c(1, 2.3, 2, 1, 1.6))
  }
})

test_that("performs data completeness accurately", {
  for(r in list(ret1, ret2)){
    expect_true(all(is.na(r$flag_daily_incomplete)))
    expect_true(all(is.na(r$avg_24h[r$n_readings < 18])))
    expect_true(all(!is.na(r$avg_24h[r$n_readings >= 18])))
  }
})
