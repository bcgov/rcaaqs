context("pm 24hr three year rolling average")

annual_values <- readRDS("pm_ann_avg.rds")
annual_values_one_id <- annual_values[annual_values$ems_id == "0220205", ]

test_mult <- pm_three_yr_avg(annual_values[, -2], val = "ann_avg", by = "ems_id")
test_one <- pm_three_yr_avg(annual_values_one_id[, c(-1, -2)], val = "ann_avg")

test_that("Is a data frame", {
  expect_is(test_one,  "data.frame")
  expect_is(test_mult, "data.frame")
})

test_that("Has correct dimensions", {
  expect_equal(dim(test_mult), c(24, 13))
  expect_equal(dim(test_one), c(1, 12))
})

test_that("Column classes are correct", {
  classes <- c("numeric", "logical", "numeric", "logical", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "logical", "numeric")
  one_classes <- unname(sapply(test_one, class))
  mult_classes <- unname(sapply(test_mult, class))
  expect_equal(one_classes, classes)
  expect_equal(mult_classes, c("character", classes))
  
})

test_that("n_years is correct", {
  ## removing a row
  ret <- pm_three_yr_avg(annual_values[-c(3:5), -2], val = "ann_avg", by = "ems_id")
  expect_equal(ret$n_years[1:3], c(1, 1, 1))
})
