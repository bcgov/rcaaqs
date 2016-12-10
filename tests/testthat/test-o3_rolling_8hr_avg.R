context("o3 rolling 8hr average")

hourly_data <- readRDS("hourly.rds")
test <- o3_rolling_8hr_avg(hourly_data, dt = "date", val = "val", by = "id")

test_that("Is a data frame", {
  expect_is(test, "data.frame")
})

test_that("Has the right column names and dimensions", {
  expected_names <- c("date", "id", "val", "rolling8", "flag_valid_8hr")
  expect_equal(names(test), expected_names)
  expect_equal(dim(test), c(43961, 5))
})

test_that("Columns are the right class", {
  classes <- list(c("POSIXct", "POSIXt"), "character", "numeric", "numeric", 
                  "logical")
  expect_equal(unname(sapply(test, class)), classes)
})
