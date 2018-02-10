context("o3 rolling 8hr average")

library(magrittr)

o1 <- dplyr::filter(o3_sample_data, 
                     ems_id == "E231866")
o2 <- dplyr::filter(o3_sample_data, 
                     ems_id %in% c("E231866", "0500886"),
                     !(ems_id == "0500886" & 
                         date_time > as.Date("2014-05-01") &
                         date_time < as.Date("2014-06-30")))

saveRDS(o1, "o3_raw1.rds")
saveRDS(o2, "o3_raw2.rds")

test_that("Runs with silently", {
  expect_silent(r1 <- o3_rolling_8hr_avg(o1))
  expect_silent(r2 <- o3_rolling_8hr_avg(o2, by = c("ems_id", "site")))
  saveRDS(r1, "o3_8h_1.rds")
  saveRDS(r2, "o3_8h_2.rds")
})

ret1 <- readRDS("o3_8h_1.rds")
ret2 <- readRDS("o3_8h_2.rds")

test_that("has correct classes", {
  for(r in list(ret1, ret2)){
    expect_is(r, "data.frame")
    expect_is(r$date_time, "POSIXct")
    expect_is(r$n_val, "numeric")
    expect_is(r$rolling8, "numeric")
    expect_is(r$valid, "logical")
  }
})

test_that("has correct dimensions", {
  nrows <- dplyr::group_by(o1, .data$ems_id) %>% 
    tidyr::complete(date_time = tidyr::full_seq(date_time, 3600)) %>%
    dplyr::summarize(n = length(unique(date_time))) %>%
    dplyr::pull(n)
  expect_equal(dim(ret1), c(nrows, 11))
  
  nrows <- dplyr::group_by(o2, .data$ems_id) %>% 
    tidyr::complete(date_time = tidyr::full_seq(date_time, 3600)) %>%
    dplyr::summarize(n = length(unique(date_time))) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(n = sum(n)) %>%
    dplyr::pull(n)
  expect_equal(dim(ret2), c(nrows, 11))
})

test_that("has correct data", {
  
  expect_equal(unique(na.omit(ret1$ems_id)), "E231866")
  expect_equal(unique(ret2$ems_id), c("0500886", "E231866"))
  
  for(r in list(ret1, ret2[ret2$ems_id == "E231866",])){
    expect_equal(r$n_val[1:10], c(1, 2, 3, 4, 5, 6, 7, 8, 8, 8))
    expect_equal(r$rolling8[1:10], c(NA, NA, NA, NA, NA,
                                     14.3, 14.3, 14.4, 14.3, 13.8))
  }
})

test_that("performs data completeness accurately", {
  for(r in list(ret1, ret2)){
    expect_true(all(is.na(r$rolling8[r$n_val < 6])))
    expect_true(all(!is.na(r$rolling8[r$n_val >= 6])))
  }
})


test_that("errors correctly", {
  expect_error(o3_rolling_8hr_avg(o1, dt = "foo", val = "value"),
               "'foo' is not a column in data", fixed = TRUE)
  expect_error(o3_rolling_8hr_avg(o1, dt = "date_time", val = "foo"),
               "'foo' is not a column in data", fixed = TRUE)
})
