context("pm 24h caaq")

annual_values <- readRDS("annual_98_percentiles.rds")
annual_values <- annual_values[annual_values$use, ]

annual_values_one_id <- annual_values[annual_values$id == "a", ]

test_that("Is a data frame", {
  expect_is(pm_24h_caaq(annual_values, year = "year", val = "ann_98_percentile", 
                        by = "id", cyear = "latest"), "data.frame")
  expect_is(pm_24h_caaq(annual_values_one_id, year = "year", val = "ann_98_percentile", 
                        cyear = "latest"), "data.frame")
})

test_that("Has correct dimensions", {
  expect_equal(dim(pm_24h_caaq(annual_values_one_id, year = "year", 
                               val = "ann_98_percentile")), c(1, 8))
  
  # For multiple sites:
  expect_equal(dim(pm_24h_caaq(annual_values, year = "year", 
                               val = "ann_98_percentile", by = "id")), c(2, 9))
})

test_that("Column classes are correct", {
  classes <- c(as.list(c(rep("integer", 4), "character", "numeric")), 
               list(c("ordered", "factor"), c("ordered", "factor")))
  
  ret_classes <- unname(sapply(pm_24h_caaq(annual_values_one_id, year = "year", 
                                            val = "ann_98_percentile"), class))
  expect_equal(ret_classes, classes)
  
  # For multiple sites:
  ret_classes <- unname(sapply(pm_24h_caaq(annual_values, year = "year", 
                                            val = "ann_98_percentile", 
                                           by = "id"), class))
  expect_equal(ret_classes, c("character", classes))
})

test_that("average is correct", {
  ret <- pm_24h_caaq(annual_values, year = "year", val = "ann_98_percentile", 
                     by = "id")
  expect_equal(ret$metric_value, 
               c(round(mean(annual_values$ann_98_percentile[annual_values$id == "a"]), 0), 
                 NA))
})

test_that("n_years is correct", {
  ## removing a row
  ret <- pm_24h_caaq(annual_values[2:4,], year = "year", val = "ann_98_percentile", 
                     by = "id")
  expect_equal(ret$n_years, c(2,1))
  
  ## Making one value NA
  annual_values$ann_98_percentile[1] <- NA
  ret1 <- pm_24h_caaq(annual_values, year = "year", val = "ann_98_percentile", 
                      by = "id")
  expect_equal(ret$n_years, c(2,1))
})

test_that("returns an error when multiple years and no grouping", {
  expect_error(pm_24h_caaq(annual_values, year = "year", 
                           val = "ann_98_percentile"), "duplicate")
})
