context("o3 daily 8hr max")
library(magrittr)

o1 <- readRDS("o3_8h_1.rds")
o2 <- readRDS("o3_8h_2.rds") %>%
  dplyr::mutate(rolling8 = replace(rolling8, 
                                   ems_id == "0500886" & 
                                     date_time > as.POSIXct("2014-01-01") &
                                     date_time < as.POSIXct("2014-01-05"),
                                   get_std("o3") + 1),
                rolling8 = replace(rolling8, 
                                   ems_id == "0500886" & 
                                     date_time > as.POSIXct("2014-01-01 06:00:00") &
                                     date_time < as.POSIXct("2014-01-01 23:00:00"),
                                   NA))

test_that("Runs with silently", {
  expect_silent(r1 <- o3_daily_max(o1))
  expect_silent(r2 <- o3_daily_max(o2, by = c("ems_id", "site")))
  saveRDS(r1, "o3_daily1.rds")
  saveRDS(r2, "o3_daily2.rds")
})

ret1 <- readRDS("o3_daily1.rds")
ret2 <- readRDS("o3_daily2.rds")

test_that("has correct classes", {
  for(r in list(ret1, ret2)){
    expect_is(r, "data.frame")
    expect_is(r$date, "Date")
    expect_is(r$n_readings, "integer")
    expect_is(r$max8hr, "numeric")
    expect_is(r$exceed, "logical")
    expect_is(r$valid, "logical")
    expect_is(r$flag_daily_incomplete, "logical")
  }
})

test_that("has correct dimensions", {
  nrows <- dplyr::mutate(o1, date = lubridate::as_date(date_time)) %>%
    tidyr::complete(date = tidyr::full_seq(date, 1)) %>%
    dplyr::summarize(n = length(unique(date))) %>%
    dplyr::pull(n)
  expect_equal(dim(ret1), c(nrows, 6))
  
  nrows <- dplyr::group_by(o2, .data$ems_id) %>% 
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
    expect_equal(r$n_readings[1:5], c(19, 24, 24, 24, 24))
    expect_equal(r$max8hr[1:5], c(17.1, 21.1, 17.1, 25.0, 22.5))
  }
})

test_that("performs data completeness accurately", {
  for(r in list(ret1, ret2)){
    expect_true(all(is.na(r$max8hr[r$n_readings < 18 & !r$exceed])))
    expect_true(all(!is.na(r$max8hr[r$n_readings < 18 & r$exceed])))
    expect_true(all(!is.na(r$max8hr[r$n_readings >= 18])))
    expect_true(all(r$flag_daily_incomplete[r$n_readings < 18 & r$exceed]))
  }
})
