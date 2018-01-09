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


#' Rolling statistics
#' 
#' @noRd

rolling_value <- function(data, dt, val, interval, by, window, valid_thresh, 
                          flag_num = NULL, exclude_df = NULL, exclude_df_dt = NULL) {
  
  if (!is.null(by)) data <- dplyr::group_by(data, !!!rlang::syms(by))

  # Pad in missing years with NA
  fill <- list(NA)
  names(fill) <- val
  data <- tidyr::complete(data,
                          !!!rlang::syms(dt) := tidyr::full_seq(!!!rlang::syms(dt), interval),
                          tidyr::nesting(!!!rlang::syms(by)), fill = fill)
  
  # Determine validity
  validity <- dplyr::mutate(data, 
                            n_years = n_within_window(!!!rlang::syms(dt), interval, window))
  validity$valid <- validity$n_years >= valid_thresh
  valid_cols <- c(by, dt, "valid")

  if (!is.null(flag_num)) {
    validity$flag <- validity$n_years %in% flag_num
    valid_cols <- c(valid_cols, "n_years", "flag")
  }
  validity <- validity[valid_cols]
  
  # Exclude data
  if(!is.null(exclude_df)) data <- 
    exclude_data(data, dt, by, exclude_df, exclude_df_dt)  
  
  # Add in NAs for excluded data
  data <- tidyr::complete(data,
                          !!!rlang::syms(dt) := tidyr::full_seq(!!!rlang::syms(dt), interval),
                          tidyr::nesting(!!!rlang::syms(by)), fill = fill)
  
  # Cacluate statistic
  data <- dplyr::mutate(data,
                        rolled_value = round_caaqs(rolling_mean(!!!rlang::syms(val), 
                                                                width = window, 
                                                                valid_thresh), 
                                                   digits = 1))

  # All missing data gets NA, even if filled by rolling mean
  data <- dplyr::mutate(data,
                        rolled_value = replace(.data$rolled_value, 
                                               is.na(!!!rlang::syms(val)), 
                                               as.numeric(NA)))
  # data <- 
  #   mutate_(data, rolled_value = 
  #           interp(~filled_rolling_mean(dt, val, interval, window, valid_thresh),
  #                  dt = rlang::syms(dt), val = rlang::syms(val)))
  data <- dplyr::ungroup(data)
  # Join validity with statistic.
  dplyr::left_join(validity, data, by = c(by, dt))
}


#' Compute a rolling statistic (typically over years, but also hourly).
#'
#' @param data data frame with date and value
#' @param dt the name (as a character string) of the date-time column.
#' @param val the name (as a character string) of the value column.
#' @param by character vector of grouping variables in data, probably an id if 
#'   using multiple sites. Even if not using multiple sites, you shoud specify 
#'   the id column so that it is retained in the output.
#' @param  exclude_df he data.frame that has all the columns in the by
#'   parameter, in addition exactly one or two date columns.
#' @param  exclude_df_dt a character vector with exactly one or two date
#'   columns.
#'   
#' @return dataframe with rolling 8-hour averages.
#' 
#' @name rolling_stat_page
NULL
#> NULL


#' @rdname rolling_stat_page
#' @export

o3_rolling_8hr_avg <- function(data, dt = "date_time", val = "value", 
                               by = NULL, exclude_df = NULL, exclude_df_dt = NULL){
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            dt %in% names(data),
            val %in% names(data),
            lubridate::is.POSIXt(data[[dt]]), 
            is.numeric(data[[val]]))

  data <- rolling_value(data, 
                       dt = dt, 
                       val = val, 
                       by = by, 
                       interval = 3600,
                       window = 8, 
                       valid_thresh = 6,
                       exclude_df = exclude_df,
                       exclude_df_dt = exclude_df_dt)
  dplyr::rename_(data, rolling8 = "rolled_value", flag_valid_8hr = "valid")
}

#' @rdname rolling_stat_page
#' @export

so2_three_yr_avg <- function(data, dt = "year", val = "ann_99_percentile", by = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            dt %in% names(data),
            val %in% names(data),
            is.numeric(data[[dt]]), 
            is.numeric(data[[val]]))

  data <- data[data$valid_year | data$exceed,]
  data <- rolling_value(data,
                        dt = dt,
                        val = val,
                        interval = 1,
                        by = by,
                        window = 3,
                        valid_thresh = 2,
                        flag_num = 2)
  dplyr::rename_(data, so2_metric = "rolled_value")
}

#' @rdname rolling_stat_page
#' @export

no2_three_yr_avg <- function(data, dt = "year", val = "ann_98_percentile", by = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            dt %in% names(data),
            val %in% names(data),
            is.numeric(data[[dt]]), 
            is.numeric(data[[val]]))

  data <- data[data$valid_year,]
  data <- rolling_value(data,
                        dt = dt,
                        val = val,
                        interval = 1,
                        by = by,
                        window = 3,
                        valid_thresh = 2,
                        flag_num = 2)
  dplyr::rename_(data, no2_metric = "rolled_value")
}

#' @rdname rolling_stat_page
#' @export

pm_three_yr_avg <- function(data, dt = "year", val = "ann_98_percentile", by = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            dt %in% names(data),
            val %in% names(data),
            is.numeric(data[[dt]]), 
            is.numeric(data[[val]]))

  data <- data[data$valid_year,]
  data <- rolling_value(data,
                        dt = dt,
                        val = val,
                        interval = 1,
                        by = by,
                        window = 3,
                        valid_thresh = 2,
                        flag_num = 2)
  dplyr::rename_(data,
          pm_metric = "rolled_value",
          flag_two_of_three_years = "flag")
}

#' @rdname rolling_stat_page
#' @export

o3_three_yr_avg <- function(data, dt = "year", val = "max8hr", by = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            dt %in% names(data),
            val %in% names(data),
            is.numeric(data[[dt]]), 
            is.numeric(data[[val]]))

  data <- data[data$valid_year | data$flag_year_based_on_incomplete_data,]
  data <- rolling_value(data,
                        dt = dt,
                        val = val,
                        interval = 1,
                        by = by,
                        window = 3,
                        valid_thresh = 2,
                        flag_num = 2) 
  dplyr::rename_(data,
          ozone_metric = "rolled_value",
          flag_two_of_three_years = "flag")
}

