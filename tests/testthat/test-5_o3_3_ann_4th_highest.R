context("o3 annual 4th highest")

o1 <- readRDS("o3_daily1.rds")
o2 <- readRDS("o3_daily2.rds") %>%
  dplyr::mutate(max8hr = replace(max8hr, ems_id == "0500886" & 
                                   date > as.Date("2013-06-01") &
                                   date < as.Date("2013-08-01"),
                                 NA),
                max8hr = replace(max8hr, ems_id == "0500886" & 
                                   date > as.Date("2014-06-01") &
                                   date < as.Date("2014-06-21"),
                                 NA),
                max8hr = replace(max8hr, ems_id == "0500886" & 
                                   date > as.Date("2014-07-16") &
                                   date < as.Date("2014-08-01"),
                                 get_std("o3") + 1))
                                 
test_that("Runs with silently", {
  expect_silent(r1 <- o3_ann_4th_highest(o1))
  expect_silent(r2 <- o3_ann_4th_highest(o2, by = c("ems_id", "site")))
  saveRDS(r1, "o3_4th_1.rds")
  saveRDS(r2, "o3_4th_2.rds")
})

ret1 <- readRDS("o3_4th_1.rds")
ret2 <- readRDS("o3_4th_2.rds")

test_that("has correct classes", {
  for(r in list(ret1, ret2)){
    expect_is(r, "data.frame")
    expect_is(r$year, "numeric")
    expect_is(r$valid_in_year, "integer")
    expect_is(r$quarter_1, "integer")
    expect_is(r$quarter_2, "integer")
    expect_is(r$quarter_3, "integer")
    expect_is(r$quarter_4, "integer")
    expect_is(r$ann_4th_highest, "numeric")
    expect_is(r$excluded, "logical")
    expect_is(r$exceed, "logical")
    expect_is(r$flag_daily_incomplete, "logical")
    expect_is(r$flag_yearly_incomplete, "logical")
  }
})

test_that("has correct dimensions", {
  nrows <- length(unique(format(o1$date, "%Y")))
  expect_equal(dim(ret1), c(nrows, 12))
  
  nrows <- dplyr::summarize(o2, n = length(unique(format(date, "%Y")))) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(n = sum(n)) %>%
    dplyr::pull(n)
  expect_equal(dim(ret2), c(nrows, 14))
})

test_that("has correct data", {
  for(r in list(ret1, ret2[ret2$ems_id == "E231866",])){
    expect_equal(r$valid_in_year, c(341, 357, 360))
    expect_equal(r$quarter_1, c(79, 87, 88))
    expect_equal(r$quarter_2, c(91, 90, 90))
    expect_equal(r$quarter_3, c(86, 91, 91))
    expect_equal(r$quarter_4, c(85, 89, 91))
  }
})

test_that("performs data completeness accurately", {
  for (r in list(ret1, ret2)){
    expect_true(all(r$valid_year[(r$quarter_2 + r$quarter_3)/183 >= 0.75]))
    expect_true(all(!r$valid_year[(r$quarter_2 + r$quarter_3)/183 < 0.75]))
    expect_true(all(is.na(r$ann_4th_highest[!r$valid_year & !r$exceed])))
    expect_true(all(r$flag_yearly_incomplete[(r$quarter_2 + r$quarter_3)/183 < 0.75 & r$exceed]))
  }
})


test_that("can exclude data rows", {
  
  # Following causes errors on travis ci?? Save file to disk to ensure it's the same
  
  # high_dates <- dplyr::group_by(o2, ems_id, site, year = lubridate::year(date)) %>%
  #   dplyr::filter(max8hr == max(max8hr, na.rm = TRUE)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(ems_id, site, date) %>%
  #   dplyr::slice(1:25) %>%
  #   write.csv("o2_ann_4th_exclude.csv", row.names = FALSE)
  
  high_dates <- read.csv("o2_ann_4th_exclude.csv", 
                         colClasses = c("character", "character", "Date"))
                                
  expect_silent(ret3 <- o3_ann_4th_highest(o2, by = c("ems_id", "site"),
                                           exclude_df = high_dates, 
                                           exclude_df_dt = c("date"), 
                                           quiet = TRUE))
  
  expect_equivalent(dplyr::select(ret2, "ems_id", "site", "year", 
                                  "quarter_1", "quarter_2", "quarter_3", 
                                  "quarter_4", "valid_year", 
                                  "flag_daily_incomplete"),
                    dplyr::select(ret3, "ems_id", "site", "year", 
                                  "quarter_1", "quarter_2", "quarter_3", 
                                  "quarter_4", "valid_year", 
                                  "flag_daily_incomplete"))
  
  expect_false(all(ret2$ann_4th_highest == ret3$ann_4th_highest, na.rm = TRUE))
  expect_true(all(ret2$ann_4th_highest >= ret3$ann_4th_highest, na.rm = TRUE))
  expect_equal(ret3$excluded, c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_equivalent(ret3$ann_4th_highest, c(NA, NA, 54.6, 44.1, 46.9, 45.0))
})
