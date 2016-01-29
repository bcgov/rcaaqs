context("eetf")

daily_avg <- readRDS("daily_averages.rds")


set.seed(42)
samp <- sample(1:nrow(daily_avg), 50)
eetf_data <- daily_avg[samp, c("id", "dates")]
names(daily_avg)[1] <- "ems_id"
names(eetf_data)[2] <- "date"

ans <- cbind(daily_avg, eetf = FALSE)
ans$eetf[samp] <- TRUE

test_that("match_date_string_works", {
  expect_true(match_date_string("1826-12-31"))
  expect_true(match_date_string("1826-01-01"))
  expect_false(match_date_string("1826-00-31"))
  expect_false(match_date_string("1826-12-32"))
  expect_false(match_date_string("182-01-15"))
  expect_false(match_date_string("18265-01-15"))
})

test_that("eetf works", {
  expect_equal(eetf(daily_avg, "ems_id", "dates", eetf_data), ans)
})

test_that("eetf fails correctly", {
  expect_error(eetf(list(a = 1, b = 2), "a", "b", data.frame()), "daily_df must be a data frame.")
  expect_error(eetf(data.frame(a = 1, b = 2), "a", "c", data.frame()), "id_col and date_col must both be columns")
  expect_error(eetf(data.frame(a = 1, b = 2), "a", "b", list(id = "a", date = "b")), "eetf must be a data frame")
  expect_error(eetf(data.frame(a = 1, b = 2), "a", "b", data.frame(id = "a", foo = "b")), 
               "eetf must be a data frame containing 'id' and 'date'")
  expect_error(eetf(data.frame(a = 1, b = 2), "a", "b", data.frame(id = "a", date = "b", stringsAsFactors = FALSE)), 
               "'date' column in eetf contains malformed dates")
  expect_error(eetf(data.frame(a = 1, b = 2), "a", "b", data.frame(id = "a", date = 1)), 
               "'date' column in eetf must be")
})
