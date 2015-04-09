context("pm 24hr caaq")

annual_values <- readRDS("annual_98_percentiles.rds")
annual_values <- annual_values[annual_values$use,]

annual_values_one_id <- annual_values[annual_values$id == "a",]

test_that("Is a data frame", {
  expect_is(pm_24hr_caaq(annual_values, "year", "ann_98_percentile", 
                         "use_but_incomplete", "id"), "data.frame")
  expect_is(pm_24hr_caaq(annual_values_one_id, "year", "ann_98_percentile", 
                         "use_but_incomplete"), "data.frame")
})

test_that("Has correct dimensions", {
  expect_equal(dim(pm_24hr_caaq(annual_values_one_id, "year", "ann_98_percentile", 
                                "use_but_incomplete")), c(1, 6))
  
  # For multiple sites:
  expect_equal(dim(pm_24hr_caaq(annual_values, "year", "ann_98_percentile", 
                                "use_but_incomplete", "id")), c(2, 7))
})

test_that("Column classes are correct", {
  classes <- c(rep("integer", 4), "numeric", "logical")
  
  ret_classes <- unname(sapply(pm_24hr_caaq(annual_values_one_id, "year", 
                                            "ann_98_percentile", 
                                            "use_but_incomplete"), class))
  expect_equal(ret_classes, classes)
  
  # For multiple sites:
  ret_classes <- unname(sapply(pm_24hr_caaq(annual_values, "year", 
                                            "ann_98_percentile", 
                                            "use_but_incomplete", "id"), class))
  expect_equal(ret_classes, c("character", classes))
})

test_that("average is correct", {
  ret <- pm_24hr_caaq(annual_values, "year", 
                      "ann_98_percentile", "use_but_incomplete", "id")
  expect_equal(ret$pm_24hr_metric, 
               c(round(mean(annual_values$ann_98_percentile[annual_values$id == "a"]), 0), 
                 NA))
})

test_that("n_years is correct", {
  ## removing a row
  ret <- pm_24hr_caaq(annual_values[2:4,], "year", "ann_98_percentile", 
                      "use_but_incomplete", "id")
  expect_equal(ret$n_years, c(2,1))
  
  ## Making one value NA
  annual_values$ann_98_percentile[1] <- NA
  ret1 <- pm_24hr_caaq(annual_values, "year", 
                       "ann_98_percentile", "use_but_incomplete", "id")
  expect_equal(ret$n_years, c(2,1))
})

test_that("returns an error when multiple years and no grouping", {
  expect_error(pm_24hr_caaq(annual_values, "year", 
                      "ann_98_percentile", "use_but_incomplete"), 
               "duplicate")
})
