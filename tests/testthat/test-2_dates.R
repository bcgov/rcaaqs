context("dates")
library(lubridate)

from <- "2017-06-09 00:00:00"
to <- "2017-06-09 05:00:00"

dt_seq <- seq(as.POSIXct(from), as.POSIXct(to), by = "1 hour")

tz(dt_seq) <- "UTC"

test_that("format_caaqs_dt works with POSIXt", {
  res <- format_caaqs_dt(dt_seq)
  expect_is(res, "POSIXct")
  
  expect_equal(res, structure(c(1496995199, 1496998799, 1497002399, 1497005999, 1497009599, 
                                1497013199), tzone = "Etc/GMT+8", class = c("POSIXct", "POSIXt")))
  
  expect_equal(as.POSIXlt(res), format_caaqs_dt(as.POSIXlt(dt_seq)))
  
  ## No converstion should happen if no prev_hour and tz = same as input
  expect_equal(format_caaqs_dt(dt_seq, prev_hour = FALSE, tz = tz(dt_seq)), 
               dt_seq)
})

test_that("format_caaqs_dt works with character", {
  expect_equal(format_caaqs_dt(as.character(dt_seq)), 
               format_caaqs_dt(dt_seq))
})
