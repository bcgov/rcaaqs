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

#'Find the daily maximum of a value by some factor
#'
#'Given a dataframe with one value column, find the maximum value by some date.
#' @importFrom  stats na.omit
#' @param  data Dataframe
#' @param  dt character the column containing dates
#' @param  val character the value column
#' @param  by  character the columns to group by
#' @param  thresh numeric. 
#' @param  digits numeric. 
#' @param  n_readings_min numeric. 
#' @param  stat numeric. 
# '@return dataframe with filled in dates
daily_stat <- function(data, dt = "date", val = "value", 
                       by = c("ems_id", "site"), digits = 1,
                       thresh, n_readings_min,
                       stat) {
  data <- group_by_(data, .dots = c(by, dt))
  data <- 
    summarise_(data,
               n_readings = interp(~length(na.omit(value)), 
                                   value = as.name(val)), 
               stat       = interp(~fun(value), 
                                   value = as.name(val), fun = stat), 
               exceed     = interp(~stat > thresh, thresh = thresh), 
               valid      = interp(~n_readings >= n_readings_min, 
                                   n_readings_min = n_readings_min))  
  data <- mutate(data, flag = exceed & !valid) # Flag for data incomplete, but used
  mutate(data, stat = round(ifelse(valid | flag, stat, NA_real_), digits))
  
}

# Clobbers any date column if there was one.
pollutant_daily_stat <- function(data, dt, val, by = NULL, pollutant_standard, stat = max_na) {
  if (!is.null(by)) data <- group_by_(data, .dots = by)
  date_time <- as.name(dt)
  data <- mutate(data, date = time_to_date(date_time)) 
  daily_stat(data,
             dt = "date", 
             val = val, 
             by = by, 
             thresh = pollutant_standard, 
             n_readings_min = 18,
             stat = stat)
}

o3_daily_max <- function(data, dt = "date_time", val = "rolling8", by = NULL) {
  data <- pollutant_daily_stat(data, dt, val, by, pollutant_standard = get_std("o3"))
  rename_(data, max8hr = "stat", valid_max8hr = "valid", flag_max8hr_incomplete = "flag")
}

no2_daily_max <- function(data, dt = "date_time", val = "value", by = NULL) {
  data <- pollutant_daily_stat(data, dt, val, by, pollutant_standard = Inf)
  rename_(data, avg_24h = "stat", valid_avg_24h = "valid", flag_avg_24hr_incomplete = "flag")
}

so2_daily_max <- function(data, dt = "date_time", val = "value", by = NULL) {
  data <- pollutant_daily_stat(data, dt, val, by, pollutant_standard = 70)
  rename_(data, avg_24h = "stat", valid_avg_24h = "valid", flag_avg_24hr_incomplete = "flag")
}

#'Find the daily average for PM2.5
#'
#'@param data data frame with date and value
#'@param dt the name (as a character string) of the date-time column. Default
#'  \code{"date_time"}
#'@param val the name (as a character string) of the PM2.5 value column. Default
#'  \code{"value"}
#'@param by character vector of grouping variables in data, probably an id if
#'  using multiple sites. Even if not using multiple sites, you shoud specfify
#'  the id column so that it is retained in the output.
#'@export
#'@return data frame with the daily averages, can be input into 
pm_daily_avg <- function(data, dt = "date_time", val = "value", by = NULL) {
  data <- pollutant_daily_stat(data, dt, val, by, pollutant_standard = Inf, stat = mean_na)
  rename_(data, avg_24h = "stat", valid_avg_24h = "valid", flag_avg_24hr_incomplete = "flag")
}
