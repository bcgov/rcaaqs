context("plotting")

daily_data <- readRDS("daily_averages.rds")
daily_data <- daily_data[daily_data$id == "a", , drop = FALSE]

annual_data <- readRDS("annual_98_percentiles.rds")
annual_data <- annual_data[annual_data$id == "a", , drop = FALSE]

test_that("mid_breaks works", {
  expect_is(mid_breaks(), "function")
  fn <- mid_breaks()
  expect_equal(fn(as.Date(c("2005-01-01", "2010-01-01"))), 
               as.Date(c("2005-07-02", "2006-07-02", "2007-07-02", "2008-07-02", 
                         "2009-07-02", "2010-07-02")))
  expect_error(fn(as.Date(c("2005-01-01", "2010-01-01", "2015-01-01"))))
  fn <- mid_breaks(1)
  expect_error(fn(1:3))
} 
)

test_that("plot_ts fails correctly", {
  names(daily_data)[2:3] <- c("date", "avg_24h")
  # Invalid parameter name
  expect_error(plot_ts(daily_data, caaqs_data = NULL, parameter = "pm2.524h", 
                       rep_yr = 2013, plot_exceedances = FALSE))
  # Wrong name for date column
  names(daily_data)[2:3] <- c("foo", "avg_24h")
  expect_error(plot_ts(daily_data, caaqs_data = NULL, parameter = "pm2.5_24h", 
                       rep_yr = 2013, plot_exceedances = FALSE))
  # Wrong name for parameter column
  names(daily_data)[2:3] <- c("date", "foo")
  expect_error(plot_ts(daily_data, caaqs_data = NULL, parameter = "pm2.5_24h", 
                       rep_yr = 2013, plot_exceedances = FALSE))
  # Wrong data formats
  names(daily_data) <- c("date", "foo", "avg_24h", "n_readings") # date is character
  expect_error(plot_ts(daily_data, caaqs_data = NULL, parameter = "pm2.5_24h", 
                       rep_yr = 2013, plot_exceedances = FALSE))
  names(daily_data) <- c("avg_24h", "date", "foo", "n_readings") # avg_24h is character
  expect_error(plot_ts(daily_data, caaqs_data = NULL, parameter = "pm2.5_24h", 
                       rep_yr = 2013, plot_exceedances = FALSE))
})

test_that("plot_ts works without caaqs_data (ozone)", {
  names(daily_data)[2:3] <- c("date", "max8hr")
  p <- plot_ts(daily_data, caaqs_data = NULL, parameter = "o3", 
               rep_yr = 2013, plot_exceedances = FALSE)
  expect_is(p, "ggplot")
  expect_is(ggplot2::ggplot_build(p), "list")
})

# test_that("works with caaqs_data (ozone)", {
#   names(daily_data) <- c("id", "date", "max8hr", "n_readings")
#   caaqs_data <- pm_24h_caaq(annual_data)
#   p <- plot_ts(daily_data, caaqs_data = caaqs_data, parameter = "o3", 
#                rep_yr = 2013, plot_exceedances = FALSE)
#   expect_is(p, "ggplot")
#   expect_is(ggplot2::ggplot_build(p), "list")
# })

test_that("plot_ts works without caaqs_data (pm_24h)", {
  names(daily_data)[2:3] <- c("date", "avg_24h")
  p <- plot_ts(daily_data, caaqs_data = NULL, parameter = "pm2.5_24h", 
               rep_yr = 2013, plot_exceedances = FALSE)
  expect_is(p, "ggplot")
  expect_is(ggplot2::ggplot_build(p), "list")
})

test_that("works with caaqs_data (pm24h)", {
  names(daily_data) <- c("id", "date", "avg_24h", "n_readings")
  caaqs_data <- pm_24h_caaq(annual_data)
  p <- plot_ts(daily_data, caaqs_data = caaqs_data, parameter = "pm2.5_24h", 
               rep_yr = 2013, plot_exceedances = FALSE)
  expect_is(p, "ggplot")
  expect_is(ggplot2::ggplot_build(p), "list")
})

test_that("plot_ts works without caaqs_data (pm_annual)", {
  names(daily_data)[2:3] <- c("date", "avg_24h")
  p <- plot_ts(daily_data, caaqs_data = NULL, parameter = "pm2.5_annual", 
               rep_yr = 2013, plot_exceedances = FALSE)
  expect_is(p, "ggplot")
  expect_is(ggplot2::ggplot_build(p), "list")
})

test_that("works with caaqs_data (pm_annual)", {
  names(daily_data) <- c("id", "date", "avg_24h", "n_readings")
  names(annual_data)[5] <- "ann_avg"
  caaqs_data <- pm_annual_caaq(annual_data)
  p <- plot_ts(daily_data, caaqs_data = caaqs_data, 
               parameter = "pm2.5_annual", rep_yr = 2013, plot_exceedances = FALSE)
  expect_is(p, "ggplot")
  expect_is(ggplot2::ggplot_build(p), "list")
})
