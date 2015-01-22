context("pm 24hr caaq")

annual_values <- readRDS("annual_98_percentiles.rds")

annual_values_one_id <- annual_values[annual_values$id == "a",]

test_that("Is a data frame", {
  expect_is(pm_24hr_caaq(annual_values, "year", "ann_98_percentile", "id"), "data.frame")
  expect_is(pm_24hr_caaq(annual_values_one_id, "year", "ann_98_percentile"), "data.frame")
})

test_that("Has correct dimensions", {
  expect_equal(dim(pm_24hr_caaq(annual_values_one_id, "year", "ann_98_percentile")), c(1, 3))
  
  # For multiple sites:
  expect_equal(dim(pm_24hr_caaq(annual_values, "year", "ann_98_percentile", "id")), c(2, 4))
})

test_that("Column classes are correct", {
  classes <- c("integer", "integer", "numeric")
  
  ret_classes <- unname(sapply(pm_24hr_caaq(annual_values_one_id, "year", 
                                            "ann_98_percentile"), class))
  expect_equal(ret_classes, classes)
  
  # For multiple sites:
  ret_classes <- unname(sapply(pm_24hr_caaq(annual_values, "year", 
                                            "ann_98_percentile", "id"), class))
  expect_equal(ret_classes, c("character", classes))
})

test_that("average is correct", {
  ret <- pm_24hr_caaq(annual_values, "year", "ann_98_percentile", "id")
  expect_equal(ret$pm_24hr_caaq, 
               c(round(mean(annual_values$ann_98_percentile[annual_values$id == "a"]), 1), 
                 round(mean(annual_values$ann_98_percentile[annual_values$id == "b"]), 1)))
})

test_that("n_years is correct", {
  ## removing a row
  ret <- pm_24hr_caaq(annual_values[2:6,], "year", "ann_98_percentile", "id")
  expect_equal(ret$n_years, c(2,3))
  
  ## Making one value NA
  annual_values$ann_98_percentile[1] <- NA
  ret1 <- pm_24hr_caaq(annual_values, "year", "ann_98_percentile", "id")
  expect_equal(ret$n_years, c(2,3))
})
