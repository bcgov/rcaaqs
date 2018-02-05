# Copyright 2018 Province of British Columbia
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

#' @importFrom rlang .data
caaq <- function(data, year, val, by, metric, n) {

  # Check inputs
  check_vars(list(year, val, by), data)
  check_class(year, data, "numeric")
  check_class(val, data, "numeric")
  
  # Rename to standard
  names(data)[names(data) == year] <- "year"
  names(data)[names(data) == val] <- "metric_value"
  
  # Start with ungrouped data
  data <- dplyr::ungroup(data)
  
  # Group data if supplied
  if(!is.null(by)) {
    if(n == 1) message("Grouping doesn't apply to single year caaqs. 'by' ignored.")
    if(n == 3) {
      data <- dplyr::group_by_if(data, names(data) %in% by)
      check_groups(data, year)
    }
  }
  
  # Order by date (within grouping if applied)
  data <- dplyr::arrange(data, !!! rlang::syms(c(by, "year")))
  
  # Added details
  if(n == 3) {
    data <- dplyr::mutate(data, 
                          n_years = n_val,
                          min_year = pmin(.data$year, 
                                          dplyr::lag(.data$year, 1),
                                          dplyr::lag(.data$year, 2), na.rm = TRUE),
                          max_year = .data$year,
                          n_max = as.integer(max(.data$n_years, na.rm = TRUE)))
    
    data <- dplyr::ungroup(data)
  }

  # Determine station achievements
  data$caaqs <- cut_achievement(data$metric_value, metric, output = "labels")
  data$mgmt <- cut_management(data$metric_value, metric)
  data$metric <- metric
  
  # Clean up
  data <- dplyr::rename(data, "caaq_year" = "year")
  cols_keep <- c(by, "caaq_year", "min_year", "max_year", "n_years", "metric", 
                 "metric_value", "caaqs", "mgmt",
                 "flag_daily_incomplete", "flag_yearly_incomplete",
                 "flag_two_of_three_years")
  cols_keep <- cols_keep[cols_keep %in% names(data)]
  
  data <- dplyr::select(data, cols_keep)
  
  data
}

#' Calculate the PM2.5 24 hour CAAQ metric
#' 
#' Calculates and returns the PM2.5 24 hour CAAQ metric based on a rolling
#' three-year average.
#'
#' @param data Data frame. Contains three year PM2.5 24hr average data output from
#'   \code{pm_three_yr_avg}
#' @param year Character. The name of the data column containing years. Defaults
#'   to 'year'.
#' @param val Character. The name of the data column containing the pm metric.
#'   Defaults to 'pm_metric'.
#' @param by Character. The name(s) of the data column(s) specifying the data
#'   grouping variables (i.e. site_id, etc.). Even if not using multiple sites,
#'   you shoud specfify the id column so that it is retained in the output.
#'
#' @return A data frame arranged by grouping variables (if present) with caaq
#'   metrics, achievement levels and management levels.
#'   
#' @export
pm_24h_caaq <- function(data, year = "year", val = "pm_metric",
                        by = NULL) {
  caaq(data, year, val, by, metric = "pm2.5_24h", n = 3)
}

#' Calculate the PM2.5 annaul CAAQ metric
#' 
#' Calculates and returns the PM2.5 annual CAAQ metric based on a rolling
#' three-year average.
#'
#' @param data Data frame. Contains PM2.5 annual three year average data output
#'   from \code{pm_three_yr_avg}
#' @param year Character. The name of the data column containing years. Defaults
#'   to 'year'.
#' @param val Character. The name of the data column containing the pm metric.
#'   Defaults to 'pm_metric'.
#' @param by Character. The name(s) of the data column(s) specifying the data
#'   grouping variables (i.e. site_id, etc.). Even if not using multiple sites,
#'   you shoud specfify the id column so that it is retained in the output.
#'
#' @return A data frame arranged by grouping variables (if present) with caaq
#'   metrics, achievement levels and management levels.
#' @export
pm_annual_caaq <- function(data, year = "year", val = "pm_metric",
                         by = NULL) {
  caaq(data, year, val, by, metric = "pm2.5_annual", n = 3)
}

#' Calculate the Ozone CAAQ metric
#' 
#' Calculates and returns the Ozone CAAQ metric based on a rolling three-year 
#' average.
#'
#' @param data Data frame. Contains three year average data output from
#'   \code{o3_three_yr_avg}
#' @param year Character. The name of the data column containing years. Defaults
#'   to 'year'.
#' @param val Character. The name of the data column containing the ozone metric.
#'   Defaults to 'ozone_metric'.
#' @param by Character. The name(s) of the data column(s) specifying the data
#'   grouping variables (i.e. site_id, etc.). Even if not using multiple sites,
#'   you shoud specfify the id column so that it is retained in the output.
#'
#' @return A data frame arranged by grouping variables (if present) with caaq
#'   metrics, achievement levels and management levels.
#'
#' @examples
#' 
#' data(o3_sample_data)
#' o3_rolling <- o3_rolling_8hr_avg(o3_sample_data, by = c("ems_id", "site"))
#' o3_daily <- o3_daily_max(o3_rolling, by = c("ems_id", "site"))
#' o3_4th_highest <- o3_ann_4th_highest(o3_daily, by = c("ems_id", "site"))
#' o3_avg <- o3_three_yr_avg(o3_4th_highest, by = c("ems_id", "site"))
#' 
#' o3_final_caaq <- o3_caaq(o3_avg, by = c("ems_id", "site"))
#' 
#' @export
o3_caaq <- function(data, year = "year", val = "ozone_metric", by = NULL) {
  caaq(data, year, val, by, metric = "o3", n = 3)
}

#' @export
so2_1yr_caaq <- function(data, year = "year", val = "avg_yearly", by = NULL) {
  caaq(data, year, val, by, metric = "so2_1yr", n = 1)
}

#' @export
so2_3yr_caaq <- function(data, year = "year", val = "so2_metric", by = NULL) {
  caaq(data, year, val, by, metric = "so2_3yr", n = 3)
}

#' @export
no2_1yr_caaq <- function(data, year = "year", val = "avg_yearly", by = NULL) {
  caaq(data, year, val, by, metric = "no2_1yr", n = 1)
}

#' @export
no2_3yr_caaq <- function(data, year = "year", val = "no2_metric", by = NULL) {
  caaq(data, year, val, by, metric = "no2_3yr", n = 3)
}
