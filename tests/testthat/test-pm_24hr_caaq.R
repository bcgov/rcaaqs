context("pm 24h caaq")

# annual_values <- readRDS("annual_98_percentiles.rds")
# annual_values_one_id <- annual_values[annual_values$id == "a", ]

annual_values <- readRDS("pm_24h_3y.rds")
annual_values_one_id <- annual_values[annual_values$ems_id == "0310162", ]

test_that("pm_24h_caaq runs without errors", {
  expect_silent(pm_24h_caaq(annual_values, by = "ems_id"))
  expect_silent(pm_24h_caaq(annual_values_one_id, by = "ems_id"))
})

test_mult <- pm_24h_caaq(annual_values, by = "ems_id")
test_one <- pm_24h_caaq(annual_values_one_id)

test_that("Is a data frame", {
  expect_is(test_mult, "data.frame")
  expect_is(test_one, "data.frame")
})

test_that("Has correct dimensions", {
  expect_equal(dim(test_one), c(1, 8))
  expect_equal(dim(test_mult), c(10, 9))
})

test_that("Column classes are correct", {
  
  check_classes <- function(df, classes) {
    for (i in seq_along(df)) {
      expect_is(df[[i]], classes[[i]])
    }
  }
  
  classes <- c(rep("numeric", 4), "character", "numeric", rep("ordered", 2))
  
  check_classes(test_one, classes)
  check_classes(test_mult, c("character", classes))
})

test_that("average is correct", {
  expect_equal(test_mult$metric_value[1:5], c(NA, 13.0, 14.6, 12.8, NA))
})

test_that("returns an error when multiple years and no grouping", {
  annual_values$year[2] <- 2013
  expect_error(pm_24h_caaq(annual_values, year = "year", 
                           val = "ann_98_percentile"), "Duplicate")
})
