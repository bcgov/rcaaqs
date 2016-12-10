context("so2 three year rolling average")

annual_values <- readRDS("annual_98_percentiles.rds")
annual_values_one_id <- annual_values[annual_values$id == "a", ]

test_mult <- so2_three_yr_avg(annual_values, val = "ann_98_percentile", by = "id")
test_one <- so2_three_yr_avg(annual_values_one_id, val = "ann_98_percentile")

test_that("Is a data frame", {
  expect_is(test_one,  "data.frame")
  expect_is(test_mult, "data.frame")
})

test_that("Has correct dimensions", {
  expect_equal(dim(test_mult), c(4, 14))
  expect_equal(dim(test_one), c(3, 14))
})

test_that("Column classes are correct", {
  classes <- c("character", "integer", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "logical", "logical", "logical", "numeric", 
               "logical", "logical")
  one_classes <- unname(sapply(test_one, class))
  mult_classes <- unname(sapply(test_one, class))
  expect_equal(one_classes, classes)
  expect_equal(mult_classes, classes)
})

