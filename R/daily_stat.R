# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.


# Initial data check - When user first passes raw data to rcaaqs, check for
# obvious problems
initial_check <- function(data, dt, val, by) {

  # Check inputs
  if(!is.data.frame(data)) stop("'data' must be a data frame", call. = FALSE)
  check_vars(list(by, dt, val), data)
  check_class(dt, data, "POSIXct")
  check_class(val, data, "numeric")
  check_one(dt, val)
  
  data <- dplyr::arrange(data, !!!rlang::syms(c(by, dt)))

  # Make sure grouping vars are character (prevents warnings when merging)
  if(!is.null(by)) data <- dplyr::mutate_at(data, by, as.character)
  
  # Confirm that data is hourly sequential with no gaps
  data <- dplyr::group_by(data, !!!rlang::syms(by))
  check_groups(data, dt)
  data <- dplyr::mutate(data, diff = difftime(!!!rlang::syms(dt), 
                                      dplyr::lag(!!!rlang::syms(dt)), 
                                      units = "hours"))

  # Fill out gaps with NA
  if(any(data$diff < 1, na.rm = TRUE)) {
    stop("Data resolution is less than hourly, summarize to hourly first", 
         call. = FALSE)
  } else if (any(data$diff > 1, na.rm = TRUE)) {
    data <- tidyr::complete(data,
                            !!rlang::sym(dt) := tidyr::full_seq(!!rlang::sym(dt), 3600),
                            if (is.null(by)) NULL else tidyr::nesting(!!!rlang::syms(by)))
  }
  
  dplyr::select(data, -dplyr::matches("diff"))
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
#'   
#' @return dataframe with summarised statistic
#' 
#' @noRd

pollutant_daily_stat <- function(data, dt, val, by = NULL, pollutant_standard, 
                                 stat = max_na, digits = 1, 
                                 n_readings_min = 18) {
  
  # Start with ungrouped data
  data <- dplyr::ungroup(data)
  
  # Add dates
  data <- dplyr::mutate(data, date = time_to_date(!!rlang::sym(dt)))
  
  # Add grouping
  data <- dplyr::group_by(data, !!!rlang::syms(c(by, "date")))
  check_groups(data, dt)
  
  # Validity checks
  validity <- dplyr::summarize(data,
                               n_readings = length(stats::na.omit(!!rlang::sym(val))),
                               valid = .data$n_readings >= n_readings_min)
  
  # Exclude data before stat calculation - THIS SHOULD HAPPEN IN NEXT STEP, NOT HERE
  # if(!is.null(exclude_df)) data <- exclude_data(data, dt, by, 
  #                                               exclude_df, exclude_df_dt, val)
  
  # Calculate statistic and round
  data <- dplyr::summarize(data, 
                           stat = round_caaqs(stat(.data[[val]]), digits),
                           exceed = .data$stat > pollutant_standard)
  data$exceed[is.na(data$exceed)] <- FALSE
  
  # Join together
  data <- dplyr::left_join(validity, data, by = c(by, "date"))
  
  # Calculate data completeness
  data$flag <- data$exceed & !data$valid # Flag for data incomplete, but used
  data
}

#' Cacluate daily maxima or averages for different sensor readings.
#'
#' @param data data frame with date and value
#' @param dt the name (as a character string) of the date-time column. Default
#'  `"date_time"`
#' @param val the name (as a character string) of the PM2.5 value column. Default
#'  `"value"`
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
#' 
#' @noRd
NULL
#> NULL

o3_daily_max <- function(data, dt = "date_time", val = "rolling8", by = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            dt %in% names(data),
            val %in% names(data),
            lubridate::is.POSIXt(data[[dt]]), 
            is.numeric(data[[val]]))
  
  # Daily stat
  data <- pollutant_daily_stat(data, dt, val, by, pollutant_standard = get_std("o3"))
  
  # Remove invalid data (< 75% of hourly measures) BUT if exceeds standard, keep and flag
  data$stat[!data$valid & !data$exceed] <- NA
  
  dplyr::rename(data, 
                "max8hr" = "stat", 
                "flag_daily_incomplete" = "flag")
}

no2_daily_max <- function(data, dt = "date_time", val = "value", by = NULL) {

  # Initial data checks for first time raw data is passed to rcaaqs
  data <- initial_check(data, dt = dt, val = val, by = by)
  
  # Daily stat
  data <- pollutant_daily_stat(data, dt, val, by, pollutant_standard = Inf)
  
  # Remove invalid data (< 75% of hourly measures) 
  data$stat[!data$valid] <- NA_real_
  
  # Exceedance of daily measures not relevant
  data$exceed <- NA
  
  dplyr::rename(data, 
                "max_24h" = "stat", 
                "flag_daily_incomplete" = "flag")
}

so2_daily_max <- function(data, dt = "date_time", val = "value", by = NULL) {

  # Initial data checks for first time raw data is passed to rcaaqs
  data <- initial_check(data, dt = dt, val = val, by = by)
  
  # Daily stat
  data <- pollutant_daily_stat(data, dt, val, by, pollutant_standard = 70)
  
  # Remove invalid data (< 75% of hourly measures) BUT if exceeds standard, keep and flag
  data$stat[!data$valid & !data$exceed] <- NA_real_
  
  dplyr::rename(data, 
                "max_24h" = "stat", 
                "flag_daily_incomplete" = "flag")
}

pm_daily_avg <- function(data, dt = "date_time", val = "value", by = NULL) {

  # Initial data checks for first time raw data is passed to rcaaqs
  data <- initial_check(data, dt = dt, val = val, by = by)
  
  data <- pollutant_daily_stat(data, dt, val, by, pollutant_standard = Inf, 
                               stat = mean_na)
  
  # Remove invalid data (< 75% of hourly measures)
  data$stat[!data$valid] <- NA_real_
  
  # Remove flag (not applicable to pm)
  data$flag <- NA
  
  # Exceedance of daily measures not relevant
  data$exceed <- NA
  
  dplyr::rename(data, 
                "avg_24h" = "stat", 
                "flag_daily_incomplete" = "flag")
}
