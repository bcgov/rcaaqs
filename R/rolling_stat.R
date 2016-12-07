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

# Rolling statistics.
rolling_value <- function(data, dt, val, interval, by, window, valid_thresh, 
                          flag_num = NULL) {
  if (!is.null(by)) data <- group_by_(data, .dots = by)
  dt <- as.name(dt)
  val <- as.name(val)
  data <- 
    mutate_(data, rolled_value = 
            ~filled_rolling_mean(dt, val, interval, window, valid_thresh))
  data <- 
    mutate_(data, n_within = 
            ~n_within_window(dt, interval, window))
  data <- ungroup(data)
  n_within <- data$n_within
  data$n_within <- NULL

  data$valid <- n_within >= valid_thresh
  if (!is.null(flag_num)) data$flag <- n_within %in% flag_num
  data
}
#'Find the daily maximum of a value by some factor
#'
#'Given a dataframe with one value column, find the maximum value by some date.
#' @importFrom  stats na.omit
#' @importFrom  lubridate is.POSIXt
#' @param  data Dataframe
#' @param  dt character the column containing date-times
#' @param  val character the value column
#' @param  by  character the columns to group by
# '@return dataframe with rolling 8-hour averages.
o3_rolling_8hr_avg <- function(data, dt = "date_time", val = "value", by = NULL){
  if (!is.POSIXt(data[[dt]])) stop(paste0('"', dt, '" is not a date-time'))
  if (!is.null(by)) data <- data %>% group_by_(.dots = by)
  rolling_value(data, 
                dt = dt, 
                val = val, 
                by = by, 
                interval = 3600,
                window = 8, 
                valid_thresh = 6) %>% 
    rename_(rolling8 = "rolled_value", flag_valid_8hr = "valid")
}

so2_three_yr_avg <- function(data, dt = "year", val = "ann_99_percentile", by = NULL) {
  if (!is.null(by)) data <- group_by_(data, .dots = by)
  data <- filter(data, valid_year | exceed)
  data <- rolling_value(data,
                        dt = dt,
                        val = val,
                        interval = 1,
                        by = by,
                        window = 3,
                        valid_thresh = 2,
                        flag_num = 2)
  rename_(data, so2_metric = "rolled_value")
}

no2_three_yr_avg <- function(data, dt = "year", val = "ann_98_percentile", by = NULL) {
  if (!is.null(by)) data <- group_by_(data, .dots = by)
  data <- filter(data, valid_year)
  data <- rolling_value(data,
                        dt = dt,
                        val = val,
                        interval = 1,
                        by = by,
                        window = 3,
                        valid_thresh = 2,
                        flag_num = 2)
  rename_(data, no2_metric = "rolled_value")
}

pm_three_yr_avg <- function(data, dt = "year", val = "ann_98_percentile", by = NULL) {
  if (!is.null(by)) data <- group_by_(data, .dots = by)
  data <- filter(data, valid_year)
  data <- rolling_value(data,
                        dt = dt,
                        val = val,
                        interval = 1,
                        by = by,
                        window = 3,
                        valid_thresh = 2,
                        flag_num = 2)
  rename_(data,
          pm_metric = "rolled_value",
          flag_two_of_three_years = "flag")
}

#'Calculate the 3-year rolling average of the ozone reading, following CAAQS data completeness 
#'rules
#'
#'@param data data frame with date and value
#'@param dt the name (as a character string) of the date-time column. Default
#'  \code{"date_time"}
#'@param val the name (as a character string) of the ozone value column. Default
#'  \code{"value"}
#'@param by character vector of grouping variables in data, probably an id if
#'  using multiple sites. Even if not using multiple sites, you shoud specify
#'  the id column so that it is retained in the output.
#'@import dplyr
#'@import lazyeval
#'@export
#'@return data frame with the 3-year rolling averages

# The method here is correct, but it won't provide measurements for 
# invalid and not flagged years, even though those measurements might be valid.
o3_three_yr_avg <- function(data, dt = "year", val = "max8hr", by = NULL) {
  if (!is.null(by)) data <- group_by_(data, .dots = by)
  data <- filter(data, valid_year | flag_year_based_on_incomplete_data)
  data <- rolling_value(data,
                        dt = dt,
                        val = val,
                        interval = 1,
                        by = by,
                        window = 3,
                        valid_thresh = 2,
                        flag_num = 2) 
  rename_(data,
          ozone_metric = "rolled_value",
          flag_two_of_three_years = "flag")
}
globalVariables(c("valid_year", "flag_year_based_on_incomplete_data", "rolled.value", "flag"))
