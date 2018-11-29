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
  # Airzone C has a station (I) with only two years that has the highest level (12), 
  # but with only two years - that shouldn't be the AZ level, the station
  # with the highest level with 3 years should be (J; 11)
  # Airzone A has all stations with two years, so the station with the highest 
  # level should be the rep station
  df <- data.frame(Airzone = sort(rep(LETTERS[1:4],4)), 
                   nyears = c(rep(2, 4), rep(3, 4), c(2,3,2,3,2,3,2,3)),
                   stationid = sort((LETTERS[1:16])),
                   value = c(1:4, 5:8, 12:9, 13:16), 
                   caaqs = achievement_levels$labels[1],
                   excluded = (rep(FALSE, 16)),
                   mgmt_value = c(1:4, 5:8, 12:9, 13:16),
                   mgmt = management_levels$labels[1])
                   
  res <- airzone_metric(df, n_years = "nyears", az = "Airzone",
                        ambient_metric_val = "value", ambient_caaqs = "caaqs",
                        station_id = "stationid", mgmt = "mgmt",
                        mgmt_metric_val = "mgmt_value", excluded = "excluded")
  expect_equal(dim(res), c(4, 10))
  expect_equal(as.character(res$Airzone), LETTERS[1:4])
  expect_equal(res$nyears_ambient, c(2,3,3,3))
  expect_equal(res$nyears_mgmt, c(2,3,3,3))
  expect_equal(res$value, c(4,8,11,16))
  expect_equal(as.character(res$rep_stn_id_ambient), c("D", "H", "J", "P"))
})

test_that("keep arg works", {
  df <- data.frame(city = letters[1:16],
                   stationid = sort((LETTERS[1:16])),
                   Airzone = sort(rep(LETTERS[1:4],4)), 
                   nyears = c(rep(2, 4), rep(3, 4), c(2,3,2,3,2,3,2,3)), 
                   value = c(1:4, 5:8, 12:9, 13:16),
                   caaqs = achievement_levels$labels[1],
                   excluded = (rep(FALSE, 16)),
                   mgmt_value = c(1:4, 5:8, 12:9, 13:16),
                   mgmt = management_levels$labels[1],
                   otherdata = rep(c("foo", "bar"), 8), stringsAsFactors = FALSE)
  res <- airzone_metric(df,  station_id = "stationid", n_years = "nyears",
                        az = "Airzone", ambient_metric_val = "value",
                        ambient_caaqs = "caaqs",
                        mgmt = "mgmt", mgmt_metric_val = "mgmt_value",
                        excluded = "excluded", keep = c("city", "otherdata"))
  expect_equal(dim(res), c(4, 14))
  expect_equal(res$Airzone, LETTERS[1:4])
  expect_equal(res$nyears_ambient, c(2,3,3,3))
  expect_equal(res$nyears_mgmt, c(2,3,3,3))
  expect_equal(res$value, c(4,8,11,16))
  expect_equal(res$city_ambient, c("d", "h", "j", "p"))
  expect_true(all(c("city_ambient", "otherdata_ambient", "city_mgmt",
                    "otherdata_mgmt") %in% names(res)))
})

test_that("assign_airzone catches incorrect input", {
  p <- readRDS("pm_24h_caaqs2.rds") %>%
    get_caaqs() %>% 
    dplyr::mutate(latitude = c(rep(49.9, 3), rep(48.4, 3)),
                  longitude = c(rep(-119, 3), rep(-123, 3)))
  
  expect_error(assign_airzone(""), "'data' is not a data frame")
  expect_error(assign_airzone(p), "'lat' is not a column in 'data'")
  expect_error(assign_airzone(p, coords = c("longitude", "lat")), "'lat' is not a column in 'data'")
  
  expect_error(assign_airzone(p, coords = c("ems_id", "site")), "Column 'ems_id' is not numeric")
  expect_error(assign_airzone(p, coords = c("longitude", "site")), "Column 'site' is not numeric")
  
  expect_error(assign_airzone(p, coords = c("longitude", "latitude")), "argument \"airzones\" is missing, with no default")
  
  skip_on_appveyor()
  skip_on_cran()
  skip_on_travis()
  expect_error(assign_airzone(p, coords = c("longitude", "latitude"),
                              airzones = bcmaps::airzones(), az = "az"), 
               "'az' is not a column in 'airzones'")
  
  expect_error(assign_airzone(p, coords = c("latitude", "longitude"),
                              airzones = bcmaps::airzones()),
               "latitude can only range from -90 to \\+90")
})

test_that("assign_airzone returns expected input", {
  skip_on_appveyor()
  skip_on_cran()
  skip_on_travis()
  
  p <- readRDS("pm_24h_caaqs2.rds") %>%
    get_caaqs() %>% 
    dplyr::mutate(latitude = c(rep(49.9, 3), rep(48.4, 3)),
                  longitude = c(rep(-119, 3), rep(-123, 3)))
  
  expect_silent(a <- assign_airzone(p, coords = c("longitude", "latitude"),
                                    airzones = bcmaps::airzones()))
  
  expect_is(a, "data.frame")
  expect_length(a, 15)
  expect_equal(nrow(a), 6)
  expect_true("airzone" %in% names(a))
  expect_is(a$airzone, "character")
  expect_equal(a$airzone, c(rep("Southern Interior", 3), rep(NA, 3)))
  
})

