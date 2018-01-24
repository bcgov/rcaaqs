context("no2 hourly by year")

hourly_data <- readRDS("hourly.rds")

test <- no2_avg_hourly_by_year(hourly_data, dt = "date", val = "val", by = "id")

test_that("Is a data frame", {
  expect_is(test, "data.frame")
})

 
test_that("Has the right column names and dimensions", {
  expected_names <-c("id", "year", "valid_in_year", "quarter_1", "quarter_2", "quarter_3", 
                     "quarter_4", "avg_yearly", "valid_year")
  expect_equal(names(test), expected_names)
  expect_equal(dim(test), c(6, 9))
})
 
test_that("Columns are the right class", {
  classes <- c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "logical")
  expect_equal(unname(sapply(test, class)), classes)
})

test_that("can exclude data rows", {
  # Take out max value to ensure difference.
  max_val <- max(hourly_data$val[hourly_data$id == "a" & strftime(hourly_data$date,"%Y") == "2011"], na.rm = TRUE)
  max_date <-
    hourly_data[!is.na(hourly_data$val) & hourly_data$val == max_val & hourly_data$id == "a","date"]
  
  excl_df <-
    data.frame(id = "a",
               start = max_date, 
               stop = max_date+(3600*24*10),
               stringsAsFactors = FALSE)
  colnames(excl_df) <- c("id", "start", "stop") # Date as a list weirdness.
  
  ret <-   
    no2_avg_hourly_by_year(hourly_data, dt = "date", val = "val", by = "id",
                           exclude_df = excl_df, exclude_df_dt = c("start", "stop"))
  
  expect_equal(ret$avg_yearly[ret$id == "a" & ret$year == 2011], 4.3)
})

