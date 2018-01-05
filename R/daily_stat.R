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

#' Find the daily statistic of a value by some factor
#'
#' Given a dataframe with one value column, find the maximum value by some date.
#' 
#' @param  data Dataframe
#' @param  dt character the column containing dates
#' @param  val character the value column
#' @param  by  character the columns to group by
#' @param  thresh numeric. 
#' @param  stat numeric. 
#' 
#' @return dataframe with summarised statistic
#' 
#' @noRd

daily_stat <- function(data, dt = "date", val = "value", 
                       by = c("ems_id", "site"),
                       thresh, stat) {
  # Stat calculation
  data <- dplyr::group_by_(data, .dots = c(by, dt))
  dplyr::summarise_(data,
             stat   = lazyeval::interp(~fun(value), value = as.name(val), fun = stat),
             exceed = lazyeval::interp(~stat > thresh, thresh = thresh))
}

#' Find the daily statistic of a value by some factor
#'
#' Given a dataframe with one value column, find the maximum value by some date.
#' 
#' @param  data Dataframe
#' @param  dt character the column containing dates
#' @param  val character the value column
#' @param  by  character the columns to group by
#' @param  pollutant_standard If this value is exceeded, then exceed column will
#'   be true.
#' @param  digits The number of digits that the value should be rounded to. 
#' @param  n_readings_min The minimum number of readings in a day for the
#'   summary statistic to be valid.
#' @param  stat function to summarise by. 
#' @param exclude_df a data.frame specifying which date ranges and sites to
#'   exclude. The data.frame must have to one date column if specific dates or
#'   date-times are to be excluded, and exactly two date or date-time columns if
#'   a date range is needed. must have all the columns of by provided,
#' @param exclude_df_dt specifies the one (or two) date columns to be use in the
#'   exclude_df data.frame.
#'   
#' @return dataframe with summarised statistic
#' 
#' @noRd

pollutant_daily_stat <- function(data, dt, val, by = NULL, pollutant_standard, 
                                 stat = max_na, exclude_df = NULL, exclude_df_dt = NULL, 
                                 digits = 1, n_readings_min = 18) {
  # Validity checks (before data exclusion)
  validity <- dplyr::mutate_(data, date = lazyeval::interp(~time_to_date(date_time), 
                                                           date_time = as.name(dt))) 
  validity <- dplyr::group_by_(validity, .dots = c(by, "date"))
  validity <- 
    dplyr::summarise_(validity,
               n_readings = lazyeval::interp(~length(stats::na.omit(value)), 
                                   value = as.name(val)), 
               valid      = lazyeval::interp(~n_readings >= n_readings_min, 
                                   n_readings_min = n_readings_min))
  validity <- validity[c(by, "date", "n_readings", "valid")]
  # Exclude data before stat calculation
  if(!is.null(exclude_df)) data <- exclude_data(data, dt, by, exclude_df, exclude_df_dt)
  # Calculate statistic.
  data <- dplyr::mutate_(data, date = lazyeval::interp(~time_to_date(date_time), date_time = as.name(dt))) 
  data <- 
    daily_stat(data, dt = "date", val = val, by = by, 
               thresh = pollutant_standard, stat = stat)
  # Join together.
  data <- dplyr::left_join(validity, data, by = c(by, "date"))
  data$exceed <- ifelse(is.na(data$exceed), FALSE, data$exceed)
  data$flag <- data$exceed & !data$valid # Flag for data incomplete, but used
  data$stat <- round_caaqs(ifelse(data$valid | data$flag, data$stat, NA_real_), digits)
  data
}

#' Cacluate daily maxima or averages for different sensor readings.
#'
#' @param data data frame with date and value
#' @param dt the name (as a character string) of the date-time column. Default
#'  \code{"date_time"}
#' @param val the name (as a character string) of the PM2.5 value column. Default
#'  \code{"value"}
#' @param by character vector of grouping variables in data, probably an id if
#'  using multiple sites. Even if not using multiple sites, you shoud specfify
#'  the id column so that it is retained in the output.
#' @param exclude_df a data.frame specifying which date ranges and sites to
#'   exclude. The data.frame must have to one date column if specific dates or
#'   date-times are to be excluded, and exactly two date or date-time columns if
#'   a date range is needed. must have all the columns of by provided
#' @param exclude_df_dt specifies the one (or two) date columns to be use in the
#'   exclude_df data.frame.
#' @return data frame with the daily averages, can be input into 
#' @name daily_stat_page
NULL
#> NULL

#' @rdname daily_stat_page
#' @export

o3_daily_max <- function(data, dt = "date_time", val = "rolling8", by = NULL, exclude_df = NULL, exclude_df_dt = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            dt %in% names(data),
            val %in% names(data),
            lubridate::is.POSIXt(data[[dt]]), 
            is.numeric(data[[val]]))
  data <- pollutant_daily_stat(data, dt, val, by, pollutant_standard = get_std("o3"), exclude_df = exclude_df, exclude_df_dt = exclude_df_dt)
  dplyr::rename_(data, max8hr = "stat", valid_max8hr = "valid", flag_max8hr_incomplete = "flag")
}

#' @rdname daily_stat_page
#' @export

no2_daily_max <- function(data, dt = "date_time", val = "value", by = NULL, exclude_df = NULL, exclude_df_dt = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            dt %in% names(data),
            val %in% names(data),
            lubridate::is.POSIXt(data[[dt]]), 
            is.numeric(data[[val]]))
  data <- pollutant_daily_stat(data, dt, val, by, pollutant_standard = Inf, exclude_df = exclude_df, exclude_df_dt = exclude_df_dt)
  dplyr::rename_(data, max_24h = "stat", valid_max_24h = "valid", flag_max_24hr_incomplete = "flag")
}

#' @rdname daily_stat_page
#' @export

so2_daily_max <- function(data, dt = "date_time", val = "value", by = NULL, exclude_df = NULL, exclude_df_dt = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            dt %in% names(data),
            val %in% names(data),
            lubridate::is.POSIXt(data[[dt]]), 
            is.numeric(data[[val]]))
  data <- pollutant_daily_stat(data, dt, val, by, pollutant_standard = 70, exclude_df = exclude_df, exclude_df_dt = exclude_df_dt)
  dplyr::rename_(data, max_24h = "stat", valid_max_24h = "valid", flag_max_24hr_incomplete = "flag")
}

#' @rdname daily_stat_page
#' @export

pm_daily_avg <- function(data, dt = "date_time", val = "value", by = NULL, exclude_df = NULL, exclude_df_dt = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            dt %in% names(data),
            val %in% names(data),
            lubridate::is.POSIXt(data[[dt]]), 
            is.numeric(data[[val]]))
  data <- pollutant_daily_stat(data, dt, val, by, pollutant_standard = Inf, stat = mean_na, exclude_df = exclude_df, exclude_df_dt = exclude_df_dt)
  dplyr::rename_(data, avg_24h = "stat", valid_avg_24h = "valid", flag_avg_24hr_incomplete = "flag")
}
