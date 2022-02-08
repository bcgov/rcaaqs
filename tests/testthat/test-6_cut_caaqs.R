context("cut breaks")

o3_vec <- c(0, 40, 50, 50.01, 56, 56.01, 63, 63.01, 80, NA)

test_that("cut_management returns a factor", {
  res <- cut_management(o3_vec, "o3")
  expect_is(res, "factor")
})

test_that("management breaks are correct for o3: category", {
  expected <- c(rep("Actions for Keeping Clean Areas Clean", 3), 
                rep("Actions for Preventing Air Quality Deterioration", 2), 
                rep("Actions for Preventing CAAQS Exceedance", 2), 
                rep("Actions for Achieving Air Zone CAAQS", 2))
  
  res <- cut_management(o3_vec, "o3", drop_na = TRUE)
  expect_equal(as.character(res), c(expected, NA))
  
  res <- cut_management(o3_vec, "o3", drop_na = FALSE)
  expect_equal(as.character(res), c(expected, "Insufficient Data"))
})

test_that("management breaks are correct for o3: value labels (html)", {
  expected <- c(rep("&lteq; 50ppb", 3), 
                rep("&gt; 50ppb &amp; &lteq; 56ppb", 2), 
                rep("&gt; 56ppb &amp; &lteq; 63ppb", 2), 
                rep("&gt; 63ppb", 2))
  
  res <- cut_management(o3_vec, "o3", output = "breaks_h", drop_na = TRUE)
  expect_equal(as.character(res), c(expected, NA))
  
  res <- cut_management(o3_vec, "o3", output = "breaks_h", drop_na = FALSE)
  expect_equal(as.character(res), c(expected, "Insufficient Data"))
})

test_that("management breaks are correct for o3: value labels (unicode)", {
  expected <- c(rep("\u2264 50ppb", 3), 
                rep("\u003E 50ppb \u0026 \u2264 56ppb", 2), 
                rep("\u003E 56ppb \u0026 \u2264 63ppb", 2), 
                rep("\u003E 63ppb", 2))
  
  res <- cut_management(o3_vec, "o3", output = "breaks_u", drop_na = TRUE)
  expect_equal(as.character(res), c(expected, NA))
  
  res <- cut_management(o3_vec, "o3", output = "breaks_u", drop_na = FALSE)
  expect_equal(as.character(res), c(expected, "Insufficient Data"))
})

test_that("management breaks are correct for o3: colour", {
  expected <- c(rep("#A6D96A", 3), 
                rep("#FEE08B", 2), 
                rep("#F46D43", 2), 
                rep("#A50026", 2))
  
  res <- cut_management(o3_vec, "o3", output = "colour", drop_na = TRUE)
  expect_equal(as.character(res), c(expected, NA))
  
  res <- cut_management(o3_vec, "o3", output = "colour", drop_na = FALSE)
  expect_equal(as.character(res), c(expected, "grey80"))
})

test_that("get_std works", {
  expect_error(get_std("foo"))
  expect_is(get_std(), "numeric")
  expect_is(get_std("o3"), "numeric")
  expect_is(get_std("pm2.5_annual"), "numeric")
  expect_is(get_std("pm2.5_24h"), "numeric")
  expect_equal(get_std("o3"), c(o3 = 62))
  expect_equal(get_std("pm2.5_annual"), c(pm2.5_annual = 8.8))
  expect_equal(get_std("pm2.5_24h"), c(pm2.5_24h = 27))
  expect_equal(length(get_std()), 7)
  expect_equal(length(get_std("o3")), 1)
  expect_equal(length(get_std("pm2.5_annual")), 1)
  expect_equal(length(get_std("pm2.5_24h")), 1)
})

test_that("get_units works", {
  expect_error(get_units("foo"))
  expect_is(get_units(), "character")
  expect_is(get_units("o3"), "character")
  expect_is(get_units("pm2.5_annual"), "character")
  expect_is(get_units("pm2.5_24h"), "character")
  expect_equal(get_units("o3"), c(o3 = "ppb"))
  expect_equal(get_units("pm2.5_annual"), c(pm2.5_annual = "\u03BCg/m\u00B3"))
  expect_equal(get_units("pm2.5_24h"), c(pm2.5_24h = "\u03BCg/m\u00B3"))
  expect_equal(length(get_units()), 7)
  expect_equal(length(get_units("o3")), 1)
  expect_equal(length(get_units("pm2.5_annual")), 1)
  expect_equal(length(get_units("pm2.5_24h")), 1)
  expect_equal(length(get_units("no2_1yr")), 1)
  expect_equal(length(get_units("no2_3yr")), 1)
  expect_equal(length(get_units("so2_1yr")), 1)
  expect_equal(length(get_units("so2_3yr")), 1)
})
