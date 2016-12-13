context("so2 hourly by year")

hourly_data <- readRDS("hourly.rds")

test <- so2_avg_hourly_by_year(hourly_data, dt = "date", val = "val", by = "id")

test_that("Is a data frame", {
  expect_is(test, "data.frame")
})

 
test_that("Has the right column names and dimensions", {
  expected_names <-c("id", "year", "valid_in_year", "quarter_1", "quarter_2", "quarter_3", 
                     "quarter_4", "max_yearly", "valid_year", "exceed")
  expect_equal(names(test), expected_names)
  expect_equal(dim(test), c(6, 10))
})
 
test_that("Columns are the right class", {
  classes <- c("character", "integer", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "logical", "logical")
  expect_equal(unname(sapply(test, class)), classes)
})
