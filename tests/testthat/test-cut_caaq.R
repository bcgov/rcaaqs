context("cut_caaq")

test_that("returns a factor", {
  res <- cut_caaq(seq(45,65,by = 2), "o3")
  expect_is(res, "factor")
})
