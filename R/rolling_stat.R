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

rolling_value <- function(data, dt, val, interval, by, window, 
                          valid_thresh, digits) {
  
  # Check inputs
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            dt %in% names(data),
            val %in% names(data),
            is.numeric(data[[val]]))
  
  # Apply grouping
  if (!is.null(by)) {
    data <- dplyr::group_by(data, !!!rlang::syms(by))
    check_groups(data, dt)
  }

  # Determine validity
  validity <- dplyr::mutate(data, 
                            n_val = n_within_window(.data[[val]], window))
  validity$valid <- validity$n_val >= valid_thresh
  validity$flag <- validity$valid & validity$n_val != window
  validity <- validity[c(by, dt, "n_val", "valid", "flag")]
  validity <- dplyr::ungroup(validity)
  
  # Calcuate statistic
  data <- dplyr::mutate(data,
                        rolled_value = round_caaqs(rolling_mean(.data[[val]], 
                                                                width = window, 
                                                                valid_thresh), 
                                                   digits = digits))
  if("excluded" %in% names(data)) {
    data <- dplyr::mutate(data, 
                          excluded = rolling_sum(.data$excluded, 
                                                 width = window),
                          excluded = .data$excluded > 0)
  }
  
  # Calculate extra details for three_yr_avgs, not for o3_rolling_8hr_avg
  if(dt == "year") {
    data <- dplyr::mutate(data,
                          year_na = dplyr::if_else(!is.na(.data[[val]]), 
                                                   .data$year, as.numeric(NA)),
                          min_year = pmin(.data$year_na,
                                          dplyr::lag(.data$year_na, 1),
                                          dplyr::lag(.data$year_na, 2), 
                                          na.rm = TRUE),
                          min_year = dplyr::if_else(is.na(.data$min_year), 
                                                    .data$year, .data$min_year), 
                          max_year = .data$year)
  }
  

  data <- dplyr::ungroup(data)
  
  # Join validity with statistic
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
#' @noRd
#' 
#' @name rolling_stat_page
NULL
#> NULL


o3_rolling_8hr_avg <- function(data, dt = "date_time", val = "value", 
                               by = NULL){
  
  # Initial data checks for first time raw data is passed to rcaaqs
  data <- initial_check(data, dt = dt, val = val, by = by)

  data <- rolling_value(data, 
                       dt = dt, 
                       val = val, 
                       by = by, 
                       interval = 3600,
                       window = 8, 
                       valid_thresh = 6,
                       digits = 1)
  
  # Data completeness performed inside 'rolling_value' (assign NA if < 6 hours)
  
  # Don't need a flag
  data$flag <- NULL
  
  dplyr::rename(data, 
                "rolling8" = "rolled_value")
}

so2_three_yr_avg <- function(data, dt = "year", val = "ann_99_percentile", by = NULL) {
  data <- rolling_value(data,
                        dt = dt,
                        val = val,
                        interval = 1,
                        by = by,
                        window = 3,
                        valid_thresh = 2,
                        digits = 1)
  
  # Data completeness performed inside 'rolling_value' (assign NA if < 2 years)
  
  dplyr::rename(data, 
                "so2_metric" = "rolled_value",
                "flag_two_of_three_years" = "flag")
}

no2_three_yr_avg <- function(data, dt = "year", val = "ann_98_percentile", by = NULL) {
  data <- rolling_value(data,
                        dt = dt,
                        val = val,
                        interval = 1,
                        by = by,
                        window = 3,
                        valid_thresh = 2,
                        digits = 1)
  
  # Data completeness performed inside 'rolling_value' (assign NA if < 2 years)
  
  dplyr::rename(data, 
                "no2_metric" = "rolled_value",
                "flag_two_of_three_years" = "flag")
}

pm_three_yr_avg <- function(data, dt = "year", val = "ann_98_percentile", by = NULL) {
  data <- rolling_value(data,
                        dt = dt,
                        val = val,
                        interval = 1,
                        by = by,
                        window = 3,
                        valid_thresh = 2,
                        digits = ifelse(val == "ann_98_percentile", 0, 1))
  
  # Data completeness performed inside 'rolling_value' (assign NA if < 2 years)
  
  dplyr::rename(data, 
                "pm_metric" = "rolled_value",
                "flag_two_of_three_years" = "flag")
}

o3_three_yr_avg <- function(data, dt = "year", val = "max8hr", by = NULL) {
  data <- rolling_value(data,
                        dt = dt,
                        val = val,
                        interval = 1,
                        by = by,
                        window = 3,
                        valid_thresh = 2,
                        digits = 0) 
  
  # Data completeness performed inside 'rolling_value' (assign NA if < 2 years)
  
  dplyr::rename(data, 
                "ozone_metric" = "rolled_value",
                "flag_two_of_three_years" = "flag")
}

