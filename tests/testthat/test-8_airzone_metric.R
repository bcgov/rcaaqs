context("airzone_metric")

test_that("parsing valid years works with some n_years being 2", {
  test_s2 <- data.frame(n_years = c(3,3,2,2,3), val = rnorm(5), foo = letters[1:5])
  res <- parse_incomplete(test_s2, "n_years", "val")
  expect_true(all(is.na(res$val[3:4])))
})

test_that("parsing valid years works with all n_years being 2", {
  test_a2 <- data.frame(n_years = rep(2,5), val = rnorm(5), foo = letters[1:5])
  res <- parse_incomplete(test_a2, "n_years", "val")
  expect_true(all(!is.na(res$val)))
})

test_that("airzone_metric works", {
  df <- data.frame(Airzone = sort(rep(LETTERS[1:4],4)), 
                   nyears = c(rep(2, 4), rep(3, 4), c(2,3,2,3,2,3,2,3)), 
                   value = c(1:4, 5:8, 12:9, 13:16), stringsAsFactors = FALSE)
  res <- airzone_metric(df, n_years = "nyears", az = "Airzone", caaq = "value")
  expect_equal(dim(res), c(4,3))
  expect_equal(res$Airzone, LETTERS[1:4])
  expect_equal(res$nyears, c(2,3,3,3))
  expect_equal(res$value, c(4,8,11,16))
})

test_that("keep arg works", {
  df <- data.frame(site_id = letters[1:16], Airzone = sort(rep(LETTERS[1:4],4)), 
                   nyears = c(rep(2, 4), rep(3, 4), c(2,3,2,3,2,3,2,3)), 
                   value = c(1:4, 5:8, 12:9, 13:16), 
                   otherdata = rep(c("foo", "bar"), 8), stringsAsFactors = FALSE)
  res <- airzone_metric(df, n_years = "nyears", az = "Airzone", caaq = "value",
                        keep = c("site_id", "otherdata"))
  expect_equal(dim(res), c(4,5))
  expect_equal(res$Airzone, LETTERS[1:4])
  expect_equal(res$nyears, c(2,3,3,3))
  expect_equal(res$value, c(4,8,11,16))
  expect_equal(res$site_id, c("d", "h", "j", "p"))
})

test_that("renaming keep cols works", {
  df <- data.frame(site_id = letters[1:16], Airzone = sort(rep(LETTERS[1:4],4)), 
                   nyears = c(rep(2, 4), rep(3, 4), c(2,3,2,3,2,3,2,3)), 
                   value = c(1:4, 5:8, 12:9, 13:16), 
                   otherdata = rep(c("foo", "bar"), 8), stringsAsFactors = FALSE)
  res <- airzone_metric(df, n_years = "nyears", az = "Airzone", caaq = "value",
                        keep = c(rep_site = "site_id", foobar = "otherdata"))
  expect_equal(names(res), c("Airzone", "nyears", "value", "rep_site", "foobar"))
  res <- airzone_metric(df, n_years = "nyears", az = "Airzone", caaq = "value",
                        keep = c(rep_site = "site_id", "otherdata", yearssss = "nyears"))
  expect_equal(names(res), c("Airzone", "yearssss", "value", "rep_site", "otherdata"))
})

test_that("assign_airzone catches incorrect input", {
  p <- readRDS("pm_24h_caaq2.rds") %>%
    dplyr::mutate(latitude = c(rep(49.9, 3), rep(48.4, 3)),
                  longitude = c(rep(-119, 3), rep(-123, 3)))
  
  expect_error(assign_airzone(""), "'data' is not a data frame")
  expect_error(assign_airzone(p), "'lat' is not a column in data")
  expect_error(assign_airzone(p, coords = c("latitude", "lon")), "'lon' is not a column in data")
  
  expect_error(assign_airzone(p, coords = c("ems_id", "site")), "Column 'ems_id' is not numeric")
  expect_error(assign_airzone(p, coords = c("latitude", "site")), "Column 'site' is not numeric")
  
  expect_error(assign_airzone(p, coords = c("latitude", "longitude")), "argument \"airzones\" is missing, with no default")
  
  skip_on_appveyor()
  skip_on_cran()
  skip_on_travis()
  expect_error(assign_airzone(p, coords = c("latitude", "longitude"),
                              airzones = bcmaps::airzones("sp"), az = "az"), 
               "'az' is not a column in the airzones Spatial Polygons Data Frame")
  
  expect_error(assign_airzone(p, coords = c("longitude", "latitude"),
                              airzones = bcmaps::airzones("sp")),
               "latitude can only range from -90 to \\+90")
})

test_that("assign_airzone returns expected input", {
  p <- readRDS("pm_24h_caaq2.rds") %>%
    dplyr::mutate(latitude = c(rep(49.9, 3), rep(48.4, 3)),
                  longitude = c(rep(-119, 3), rep(-123, 3)))
  
  expect_silent(a <- assign_airzone(p, coords = c("latitude", "longitude"),
                                    airzones = bcmaps::airzones("sp")))
  
  expect_is(a, "data.frame")
  expect_length(a, 17)
  expect_equal(nrow(a), 6)
  expect_true("airzone" %in% names(a))
  expect_is(a$airzone, "character")
  expect_equal(a$airzone, c(rep("Southern Interior", 3), rep(NA, 3)))
  
})

