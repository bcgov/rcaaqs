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

# multi_id <- readRDS("daily_averages.rds")
# one_id <- multi_id[multi_id$id == "a", ]

multi_id <- readRDS("pm_d.rds")
one_id <- multi_id[multi_id$ems_id == "0220205", ]

test_one <- pm_yearly_98(one_id)
test_mult <- pm_yearly_98(multi_id, by = "ems_id")

test_that("can exclude data rows", {
  # Take out values 8.0 or above to make sure it changes
  high_dates <- 
    multi_id$date[multi_id$ems_id == "0220205" & strftime(multi_id$date,"%Y") == 2011 & 
                    !is.na(multi_id$avg_24h)  & multi_id$avg_24h >= 8.0]
  excl_df <-
    data.frame(ems_id = "0220205",
               start = high_dates,
               stop = high_dates + 1,
               stringsAsFactors = FALSE)
  
  # testo <- rcaaqs:::exclude_data(multi_id, "dates", "id", excl_df, c("start", "stop"))
  ret <- pm_yearly_98(multi_id, by = "ems_id", 
                      exclude_df = excl_df, exclude_df_dt = c("start", "stop"))

  expect_equal(ret$ann_98_percentile[ret$ems_id == "0220205" & ret$year == 2011], 6.6)
})

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
  expected_names <-c("year", "valid_in_year", "quarter_1", "quarter_2", "quarter_3", 
                     "quarter_4", "ann_98_percentile", "valid_year", "exceed", 
                     "flag_year_based_on_incomplete_data")
  expect_equal(names(test_one), expected_names)
  expect_equal(dim(test_one), c(3, 10))

  # For multiple years:
  expect_equal(names(test_mult), c("ems_id", expected_names))
  expect_equal(dim(test_mult), c(30, 11))
})
 
test_that("Columns are the right class", {
  classes <- c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "logical", "logical", "logical")
  expect_equal(unname(sapply(test_one, class)), classes)
  expect_equal(unname(sapply(test_mult, class)), c("character", classes))
})
 
test_that("Exceed works", {
  expect_equal(sum(test_mult$exceed), 1)

  set.seed(42)
  multi_id$avg_24h <- rnorm(nrow(multi_id), 35, 1)
  res <- pm_yearly_98(multi_id, by = "ems_id")
  expect_true(all(res$exceed))
})
 
test_that("Number of valid days in year correct", {
  expect_equivalent(c(365,365,366) * test_one$valid_in_year, c(285, 345.0546, 255.6986), tolerance = 0.000001)
  expect_equivalent(c(365,365,366) * test_mult$valid_in_year[1:6], 
                    c(285.0000, 345.0546, 255.6986, 365.0000, 365.0000, 360.9863), tolerance = 0.000001)
})
