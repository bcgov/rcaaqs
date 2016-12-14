context("o3 annual 4th highest")

multi_id <- readRDS("daily_averages.rds")
multi_id$valid_max8hr <- TRUE
multi_id$flag_max8hr_incomplete <- FALSE
one_id <- multi_id[multi_id$id == "a", ]

test_one <- o3_ann_4th_highest(one_id, dt = "dates", val = "val")
test_mult <- o3_ann_4th_highest(multi_id, dt = "dates", val = "val", by = "id")

test_that("Only accepts date objects", {
  posix <- seq.POSIXt(as.POSIXct("2012-01-01"), as.POSIXct("2012-01-31"), by = "hour")
  test <- data.frame(posix, val = rnorm(length(posix), 20, 5))

  expect_error(o3_ann_4th_highest(test, "posix", "val"))
})

test_that("Is a data frame", {
  expect_is(test_one, "data.frame")
  expect_is(test_mult, "data.frame")
})

 
test_that("Has the right column names and dimensions", {
  expected_names <-c("year", "valid_in_year", "quarter_1", "quarter_2", "quarter_3", 
                     "quarter_4", "max8hr", "valid_year", "exceed", 
                     "flag_year_based_on_incomplete_data")
  expect_equal(names(test_one), expected_names)
  expect_equal(dim(test_one), c(4, 10))

  # For multiple years:
  expect_equal(names(test_mult), c("id", expected_names))
  expect_equal(dim(test_mult), c(8, 11))
})
 
test_that("Columns are the right class", {
  classes <- c("integer", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "logical", "logical", "logical")
  expect_equal(unname(sapply(test_one, class)), classes)
  expect_equal(unname(sapply(test_mult, class)), c("character", classes))
})
 
test_that("Exceed works", {
  expect_false(any(test_mult$exceed))

  set.seed(42)
  multi_id$val <- rnorm(nrow(multi_id), 350, 1)
  res <- o3_ann_4th_highest(multi_id, dt = "dates", val = "val", by = "id")
  expect_true(all(res$exceed[res$valid_year]))
})
 
test_that("Number of valid days in year correct", {
  expect_equal(test_one$valid_in_year, c(1L, 361L, 358L, 357L))
  expect_equal(test_mult$valid_in_year, c(1L, 361L, 358L, 357L, 1L, 365L, 243L, 170L))
})

test_that("can exclude data rows", {
  # Take out values 9.0 or above to make sure it changes
  high_dates <- 
    multi_id$date[multi_id$id == "a" & strftime(multi_id$dates,"%Y") == 2011 & 
                    !is.na(multi_id$val)  & multi_id$val >= 9.0]
  excl_df <-
    data.frame(id = "a",
               start = high_dates,
               stop = high_dates + 1,
               stringsAsFactors = FALSE)
  
  ret <- o3_ann_4th_highest(multi_id, dt = "dates", val = "val", by = "id", 
                       exclude_df = excl_df, exclude_df_dt = c("start", "stop"))
  
  expect_equal(round(ret$max8hr[ret$id == "a" & ret$year == 2011],1), 8.6)
})
