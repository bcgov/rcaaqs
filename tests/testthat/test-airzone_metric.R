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

