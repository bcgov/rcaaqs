context("no2 daily max")

no1 <- dplyr::filter(no2_sample_data, 
                     ems_id == "E231866")
no2 <- dplyr::filter(no2_sample_data, 
                     ems_id %in% c("E231866", "0500886"),
                     !(ems_id == "0500886" & 
                         date_time > as.Date("2014-05-05") &
                         date_time < as.Date("2014-06-30"))) %>%
  dplyr::mutate(value = replace(value, ems_id == "0500886" & 
                                  date_time > as.POSIXct("2014-05-02 00:00:00") &
                                  date_time < as.POSIXct("2014-05-03 23:00:00"),
                                NA),
                value = replace(value, ems_id == "0500886" & 
                                  date_time > as.POSIXct("2014-05-02 08:00:00") &
                                  date_time < as.POSIXct("2014-05-02 13:00:00"),
                                get_std("no2_3yr") + 1))

saveRDS(no1, "no2_raw1.rds")
saveRDS(no2, "no2_raw2.rds")

test_that("Runs with silently", {
  expect_silent(r1 <- no2_daily_max(no1))
  expect_silent(r2 <- no2_daily_max(no2, by = c("ems_id", "site")))
  saveRDS(r1, "no2_daily1.rds")
  saveRDS(r2, "no2_daily2.rds")
})

ret1 <- readRDS("no2_daily1.rds")
ret2 <- readRDS("no2_daily2.rds")

test_that("has correct classes", {
  for(r in list(ret1, ret2)){
    expect_is(r, "data.frame")
    expect_is(r$date, "Date")
    expect_is(r$n_readings, "integer")
    expect_is(r$max_24h, "numeric")
    expect_is(r$exceed, "logical")
    expect_is(r$valid, "logical")
    expect_is(r$flag_daily_incomplete, "logical")
  }
})

test_that("has correct dimensions", {
  nrows <- dplyr::group_by(no1, .data$ems_id) %>% 
    dplyr::mutate(date = lubridate::as_date(date_time)) %>%
    tidyr::complete(date = tidyr::full_seq(date, 1)) %>%
    dplyr::summarize(n = length(unique(date))) %>%
    dplyr::pull(n)
  expect_equal(dim(ret1), c(nrows, 6))
  
  nrows <- dplyr::group_by(no2, .data$ems_id) %>% 
    dplyr::mutate(date = lubridate::as_date(date_time)) %>%
    tidyr::complete(date = tidyr::full_seq(date, 1)) %>%
    dplyr::summarize(n = length(unique(date))) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(n = sum(n)) %>%
    dplyr::pull(n)
  expect_equal(dim(ret2), c(nrows, 8))
})

test_that("has correct data", {
  for(r in list(ret1, ret2[ret2$ems_id == "E231866",])){
    expect_equal(r$n_readings[1:5], c(23, 23, 23, 23, 23))
    expect_equal(r$max_24h[1:5], c(9.9, 24.8, 25.4, 14.9, 16.2))
  }
})

test_that("performs data completeness accurately", {
  for(r in list(ret1, ret2)){
    expect_true(all(!is.na(r$flag_daily_incomplete)))
    expect_true(all(is.na(r$max_24h[r$n_readings < 18])))
    expect_true(all(!is.na(r$max_24h[r$n_readings >= 18])))
    expect_true(all(!r$flag_daily_incomplete))
  }
})
