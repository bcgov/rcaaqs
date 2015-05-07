# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#'Calculates the percentage of days with readings annually, as well as for each 
#'quarter for each year in a dataset.
#'
#'Designed to be used with the output from \code{\link{pm_daily_avg}}
#'@import dplyr
#'@import lazyeval
#'@import openair
#'@param  data data frame (likely the result of running
#'  \code{\link{pm_daily_avg}})
#'@param  dt the name of the date/time column (as a character string). Default
#'  \code{"date_time"}
#'@param  val the name of the column with daily average PM2.5 values. Default
#'  \code{"avg_24hr"}
#'@param  by character vector of  grouping variables, probably an id if using
#'  multiple sites. Even if not using multiple sites, you shoud specfify the id
#'  column so that it is retained.
#'@param  daily_valid  The minimum number of hourly readings in a day for the
#'   daily average to be considered valid (default \code{18}).
#'@param  year_valid  The percentage of valid days required in a year (default
#'  75).
#'@param  q_valid  The percentage of valid days required in each quarter
#'  (default 60).
#'@export
#'@seealso \code{\link{pm_daily_avg}}
#'@return A data frame with percentage of days with readings annually, as well 
#'  as one for each quarter. Also includes whether or not the annual and 
#'  quarterly requirements are met

pm_data_complete <- function(data, dt = "date_time", val = "value", by = NULL, 
                             daily_valid = 18, year_valid = 75, q_valid = 60) {
  if (!inherits(data[[dt]], "POSIXct")) {
    stop("dt must be POSIXct with time intervals of one hour")
  }
  
  if (test_time_interval(data[[dt]]) != 3600) {
    stop("dt must have a time intervals of one hour")
  }
  
  data <- data[!is.na(data[[val]]), ]
  
  ## Summarise by date
  data$date <- as.Date(data[[dt]])
  data <- group_by_(data, .dots = c(by, "date"))
  data <- summarise_(data, n_hrs = ~n())
  data <- filter_(data, ~n_hrs >= daily_valid)
  
  ## Add year column
  data$year <- get_year_from_date(data[["date"]])
  
  res <- group_by_(data, .dots = c(by, "year"))
  res <- summarise_(res, 
                    n_days = ~n(),
                    percent_valid_annual = ~percent_valid_days(date, q = "year"), 
                    percent_valid_q1 = ~percent_valid_days(date, q = "Q1"), 
                    percent_valid_q2 = ~percent_valid_days(date, q = "Q2"), 
                    percent_valid_q3 = ~percent_valid_days(date, q = "Q3"), 
                    percent_valid_q4 = ~percent_valid_days(date, q = "Q4"))
  res <- rowwise(res)
  res <- mutate_(res, 
                 annual_valid = ~percent_valid_annual >= year_valid,
                 quarters_valid = ~all(c(percent_valid_q1, percent_valid_q2, 
                                         percent_valid_q3, percent_valid_q4) >= q_valid), 
                 use_annual = ~quarters_valid & annual_valid)
  ret <- ungroup(res)
  ret
}

#' Given a vector of dates (in a single year), calculate the percentage of days in a quarter
#' 
#' @param  dates a vector of dates
#' @param  q the time period of interest, one of: "year","Q1","Q2","Q3","Q4"
#' @param  tz the timezone the dates are in. Default Etc/GMT-8 with no Daylight savings
#' @export
#' @return  A percentage of days in the specified quarter that are in the supplied vector
#' 
percent_valid_days <- function(dates, q = c("year","Q1","Q2","Q3","Q4"), 
                               tz = "Etc/GMT-8") {
  if (!tz %in% OlsonNames()) stop(tz, " is not a valid timezone.")
  if (!inherits(dates, "Date")) {
    if (test_time_interval(dates) != 86400) stop("Time interval of date column must be one day")
  }
  
  q = match.arg(q)
  
  dates <- dates[!is.na(dates)]
  
  year <- get_year_from_date(dates[1])
  is_leap_year <- lubridate::leap_year(year)
  
  if (is_leap_year) Q1 <- 91 else Q1 <- 90
  q_lengths <- c(Q1 = Q1, Q2 = 91, Q3 = 92, Q4 = 92)
  q_lengths <- c(year = sum(q_lengths), q_lengths)
  
  q_length = q_lengths[q]
  
  if (q == "year") {
    q_dates <- dates
  } else {
    q_dates <- dates[quarters(dates) %in% q]
  }
  
  ret <- (length(q_dates) / q_length) * 100
  
  ret
}

test_time_interval <- function(x) {
  len <- min(100, length(x))
  test_x <- sort(x[1:len])
  m_diff <- Mode(diff(as.numeric(test_x)))
  if (inherits(x, "Date")) m_diff <- m_diff * 3600 * 24
  m_diff
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# get_valid_days <- function(, daily_valid = 18, tz = "Etc/GMT-8") {
#   if (!inherits(dt, "POSIXt")) stop("dt is not a valid date-time class (POSIXct or POSIXlt)")
#   if (!tz %in% OlsonNames()) stop(tz, " is not a valid timezone.")
#   time_interval <- openair:::find.time.interval(dt)
#   if (!grepl("3600", time_interval)) stop("Time interval of date column must be one hour")
#   
#   dt <- unique(na.omit(dt))
#   
#   df <- data.frame(date = as.Date(dt, tz = tz))
#   
#   df <- group_by_(df, "date")
#   df <- summarise_(df, n_hrs = ~n())
#   df <- filter_(df, ~n_hrs >= daily_valid)
#   df$date
# }
