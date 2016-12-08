context("quantile2")

test_that("only accepts numbers for x", {
  expect_error(quantile2("foo"))
})

test_that("type is either 'caaqs' or 0:10", {
  expect_error(quantile2(1:5, type = "foo"))
  expect_error(quantile2(1:5, type = 0))
  expect_error(quantile2(1:5, type = 10))
})

test_that("percentiles are correct with type = 'caaq'", {
  expect_true(all.equal(quantile2(1:25, probs = 0.98, type = "caaqs"), 25))
  expect_true(all.equal(quantile2(1:50, probs = 0.98, type = "caaqs"), 50))
  expect_true(all.equal(quantile2(1:51, probs = 0.98, type = "caaqs"), 50))
  expect_true(all.equal(quantile2(1:100, probs = 0.98, type = "caaqs"), 99))
  expect_true(all.equal(quantile2(1:101, probs = 0.98, type = "caaqs"), 99))
  expect_true(all.equal(quantile2(1:150, probs = 0.98, type = "caaqs"), 148))
  expect_true(all.equal(quantile2(1:151, probs = 0.98, type = "caaqs"), 148))
  expect_true(all.equal(quantile2(1:200, probs = 0.98, type = "caaqs"), 197))
  expect_true(all.equal(quantile2(1:201, probs = 0.98, type = "caaqs"), 197))
  expect_true(all.equal(quantile2(1:250, probs = 0.98, type = "caaqs"), 246))
  expect_true(all.equal(quantile2(1:251, probs = 0.98, type = "caaqs"), 246))
  expect_true(all.equal(quantile2(1:300, probs = 0.98, type = "caaqs"), 295))
  expect_true(all.equal(quantile2(1:301, probs = 0.98, type = "caaqs"), 295))
  expect_true(all.equal(quantile2(1:350, probs = 0.98, type = "caaqs"), 344))
  expect_true(all.equal(quantile2(1:351, probs = 0.98, type = "caaqs"), 344))
  expect_true(all.equal(quantile2(1:366, probs = 0.98, type = "caaqs"), 359))
})

test_that("has names", {
  expect_true(names(quantile2(1:10, probs = 0.98, names = TRUE, type = "caaqs")) == "98%")
})

test_that("fails with NAs if na.rm = FALSE", {
  x <- 1:10
  x[3] <- NA
  expect_error(quantile2(x, na.rm = FALSE, type = "caaqs"))
})

test_that("works with NA if na.rm = TRUE", {
  x <- 1:10
  x[3] <- NA
  expect_equal(quantile2(x, na.rm = TRUE, type = "caaqs"), 10)
})

test_that("works with length(probs) > 1 and type = 'caaqs'", {
  expect_equal(quantile2(1:100, probs = c(0.1, 0.5, 0.98), type = "caaqs"),
               c(11, 51, 99))
})

test_that("passes correctly to quantile", {
  expect_equal(lapply(1:9, function(x) {
    set.seed(42)
    quantile(rnorm(100), probs = seq(0, 1, 0.25), names = TRUE, type = x)
  }),
  lapply(1:9, function(x) {
    set.seed(42)
    quantile2(rnorm(100), probs = seq(0, 1, 0.25), names = TRUE, type = x)
  }))
})


context("pm 98 percentile")

multi_id <- readRDS("daily_averages.rds")
one_id <- multi_id[multi_id$id == "a", ]

# Make a multi-year df
# dates2 <- seq.Date(from = as.Date("2012-01-01"),
#                             to = as.Date("2012-12-31"), by = "days")
# mult_years <- data.frame(id = c(rep("a", length(dates)), rep("b", length(dates2))),
#                          dates = c(dates, dates2),
#                          val = rnorm(length(dates) + length(dates2), 20, 5),
#                          stringsAsFactors = FALSE)

test_one <- pm_yearly_98(one_id, dt = "dates", val = "val")
test_mult <- pm_yearly_98(multi_id, dt = "dates", val = "val", by = "id")
# saveRDS(test_mult, "~/rcaaqs/tests/testthat/annual_98_percentiles.rds")

test_that("Only accepts date objects", {
  posix <- seq.POSIXt(as.POSIXct("2012-01-01"), as.POSIXct("2012-01-31"), by = "hour")
  test <- data.frame(posix, val = rnorm(length(posix), 20, 5))

  expect_error(pm_yearly_98(test, "posix", "val"))
})

test_that("Is a data frame", {
  expect_is(test_one, "data.frame")
  expect_is(test_mult, "data.frame")
})

 
test_that("Has the right column names and dimensions", {
  expected_names <-c("year", "ann_98_percentile", "valid_in_year", "quarter_1", 
                     "quarter_2", "quarter_3", "quarter_4", "valid_year", 
                     "exceed", "flag_year_based_on_incomplete_data")
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
  multi_id$val <- rnorm(nrow(multi_id), 35, 1)
  res <- pm_yearly_98(multi_id, dt = "dates", val = "val", by = "id")
  expect_true(all(res$exceed))
})
 
test_that("Number of valid days in year correct", {
  expect_equal(c(365,365,366,365) * test_one$valid_in_year, c(1L, 361L, 358L, 357L))
  expect_equal(c(365,365,366,365) * test_mult$valid_in_year, c(1L, 361L, 358L, 357L, 1L, 365L, 243L, 170L))
})
