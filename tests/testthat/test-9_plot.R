context("plotting")

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

o3_caaqs <- readRDS("o3_caaqs1.rds")
pm_24h_caaqs_one <- readRDS("pm_24h_caaqs1.rds")
pm_24h_caaqs_multi <- readRDS("pm_24h_caaqs2.rds")
pm_annual_caaqs <- readRDS("pm_annual_caaqs2.rds")

test_that("plot_ts fails correctly", {
  
  # not a caaqs object
  expect_error(plot_ts(data.frame(), rep_yr = 2013), "x must be an object of class 'caaqs")
  
  # wrong year
  expect_error(plot_ts(o3_caaqs, rep_yr = 2025))
  
  # Can't plot multiple ids
  expect_error(plot_ts(pm_24h_caaqs_multi, rep_yr = 2013), 
               "id and id_col required when more than one monitoring station is present")
  
})

test_that("plot_ts works with and without caaqs_data (ozone)", {
  p <- plot_ts(o3_caaqs, plot_caaqs = TRUE,
               rep_yr = 2015)
  expect_is(p, "ggplot")
  expect_is(ggplot2::ggplot_build(p), "ggplot_built")
  p <- plot_ts(o3_caaqs, plot_caaqs = FALSE,
               rep_yr = 2015)
  expect_is(p, "ggplot")
  expect_is(ggplot2::ggplot_build(p), "ggplot_built")
})


test_that("plot_ts with pm_24h single and multi", {
  p <- plot_ts(pm_24h_caaqs_one, rep_yr = 2013, plot_caaqs = FALSE)
  expect_is(p, "ggplot")
  expect_is(ggplot2::ggplot_build(p), "ggplot_built")
  
  expect_warning(
    plot_ts(pm_24h_caaqs_one, rep_yr = 2013, plot_caaqs = TRUE), 
    "caaqs not added to plot: Insufficient Data"
  )
  
  p <- plot_ts(pm_24h_caaqs_multi, rep_yr = 2013, 
               id = "0310162", id_col = "ems_id")
  expect_is(p, "ggplot")
  expect_is(ggplot2::ggplot_build(p), "ggplot_built")
})

test_that("works with annual", {
  p <- plot_ts(pm_annual_caaqs, rep_yr = 2013, 
               id = "0310162", id_col = "ems_id")
  expect_is(p, "ggplot")
  expect_is(ggplot2::ggplot_build(p), "ggplot_built")
})
