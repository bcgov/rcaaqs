context("eetf")

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
