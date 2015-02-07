context("cut_management")

o3_vec <- c(0, 40, 50, 50.01, 56, 56.01, 63, 63.01, 80, NA)

test_that("returns a factor", {
  res <- cut_management(o3_vec, "o3")
  expect_is(res, "factor")
})

test_that("breaks are correct for o3: category", {
  expected <- c(rep("Actions for Keeping Clean Areas Clean", 3), 
                rep("Actions for Preventing Air Quality Deterioration", 2), 
                rep("Actions for Preventing CAAQS Exceedance", 2), 
                rep("Actions for Achieving Air Zone CAAQS", 2))
  
  res <- cut_management(o3_vec, "o3", drop_na = TRUE)
  expect_equal(as.character(res), c(expected, NA))
  
  res <- cut_management(o3_vec, "o3", drop_na = FALSE)
  expect_equal(as.character(res), c(expected, "Insufficient Data"))
})

test_that("breaks are correct for o3: value labels (html)", {
  expected <- c(rep("&leq; 50ppb", 3), 
                rep("&gt; 50ppb &amp; &leq; 56ppb", 2), 
                rep("&gt; 56ppb &amp; &leq; 63ppb", 2), 
                rep("&gt; 63ppb", 2))
  
  res <- cut_management(o3_vec, "o3", output = "breaks_h", drop_na = TRUE)
  expect_equal(as.character(res), c(expected, NA))
  
  res <- cut_management(o3_vec, "o3", output = "breaks_h", drop_na = FALSE)
  expect_equal(as.character(res), c(expected, "Insufficient Data"))
})

test_that("breaks are correct for o3: value labels (unicode)", {
  expected <- c(rep("\u2264 50ppb", 3), 
                rep("\u003E 50ppb \u0026 \u2264 56ppb", 2), 
                rep("\u003E 56ppb \u0026 \u2264 63ppb", 2), 
                rep("\u003E 63ppb", 2))
  
  res <- cut_management(o3_vec, "o3", output = "breaks_u", drop_na = TRUE)
  expect_equal(as.character(res), c(expected, NA))
  
  res <- cut_management(o3_vec, "o3", output = "breaks_u", drop_na = FALSE)
  expect_equal(as.character(res), c(expected, "Insufficient Data"))
})

test_that("breaks are correct for o3: colour", {
  expected <- c(rep("#A6D96A", 3), 
                rep("#FEE08B", 2), 
                rep("#F46D43", 2), 
                rep("#A50026", 2))
  
  res <- cut_management(o3_vec, "o3", output = "colour", drop_na = TRUE)
  expect_equal(as.character(res), c(expected, NA))
  
  res <- cut_management(o3_vec, "o3", output = "colour", drop_na = FALSE)
  expect_equal(as.character(res), c(expected, "#CCCCCC"))
})
