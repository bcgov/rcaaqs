# Copyright 2018 Province of British Columbia
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

#' Calculate CAAQS management levels excluding days with Exceptional Events 
#' or Transboundary Flows
#'
#' @param x an object of class `caaqs`
#' @param exclude_df Data frame. The dates over which data should be excluded 
#'   (see details). Data should be arranged either with one column of dates to 
#'   omit, or two columns specifying a series of start and end date ranges to 
#'   omit.
#' @param exclude_df_dt Character vector. The names of the date columns in 
#'   \code{exclude_df}. Must specify either one (a series of dates), or two (the
#'   start and end columns specifying dates ranges).
#' @param quiet Logical. Suppress progress messages (default FALSE)
#' 
#' @details To omit days which are suspected to be influenced by Transboundary
#'   Flows or Exceptional Events create a data frame that either a) contains a
#'   column listing all the days which are to be omitted, or b) contains two
#'   columns listing the start and end dates of all the date periods which are
#'   to be omitted. This is supplied as \code{exclude_df}. Use
#'   \code{exlcude_df_dt} to specify the name of the column containing the
#'   dates, or the names of the columns containing the start and end of the date
#'   ranges (see examples and vignette for more details).
#'
#' @return object of class `caaqs`
#' @export
#'
#' @examples
caaqs_management <- function(x, exclude_df = NULL, exclude_df_dt = NULL, quiet = FALSE) {
  if (!is.null(exclude_df)) {
    check_exclude(extract_daily(x), dt = "date",
                  by = attr(x, "vars")[["by"]],
                  exclude_df, exclude_df_dt)
  }
  UseMethod("caaqs_management")
}

caaqs_management.default <- function(x, exclude_df = NULL, exclude_df_dt = NULL, 
                                     quiet = FALSE) {
  stop("No method defined for object of type ", 
       paste(class(x), collapse = ", "), call. = FALSE)
}

caaqs_management.pm2.5_24h <- function(x, exclude_df = NULL, exclude_df_dt = NULL, 
                                       quiet = FALSE) {
  
  daily <- extract_daily(x)
  if (!quiet) message("Calculating PM 2.5 annual 98th percentile")
  
  by <- attr(x, "vars")[["by"]]
  
  yearly_mgmt <- pm_yearly_98(daily, 
                              by = by, 
                              exclude_df = exclude_df, 
                              exclude_df_dt = exclude_df_dt, 
                              quiet = quiet)
  
  if (!quiet) message("Calculating PM 2.5 24h CAAQS metric")
  
  yearly_roll_mgmt <- pm_three_yr_avg(yearly_mgmt, val = "ann_98_percentile", by = by)
  caaqs_mgmt <- caaqs(yearly_roll_mgmt, val = "pm_metric", by = by, metric = "pm2.5_24h", 
                      n = 3, management = TRUE)
  # Add new columns or objects to caaqs object, update class, return modified 
  # caaqs object
  list(x, 
       list(yearly_mgmt = yearly_mgmt, 
            yearly_roll_mgmt = yearly_roll_mgmt, 
            caaqs_mgmt = caaqs_mgmt))
}

join_management_caaqs <- function(caaqs, mgmt_caaqs, by) {
  mgmt_caaqs <- dplyr::select(mgmt_caaqs, by, "caaqs_year", "metric", 
                        "mgmt_metric_value" = "metric_value", 
                        "mgmt", "excluded")
  ret <- dplyr::left_join(caaqs, mgmt_caaqs, by = c(by, "caaqs_year", "metric"))
  dplyr::select(ret, by, "caaqs_year", "metric", 
                "ambient_metric_value" = "metric_value", 
                "ambient_caaqs" = "caaqs", 
                "excluded", "mgmt_metric_value", "mgmt_level" = "mgmt", 
                dplyr::everything())
}

join_management_yearly <- function(yearly, mgmt_yearly, by) {
  mgmt_yearly <- dplyr::select(mgmt_yearly, by, "year", "excluded", 
                               "ann_98_percentile_mgmt" = "ann_98_percentile", 
                               "exceed_mgmt" = "exceed")
  
  ret <- dplyr::left_join(yearly, mgmt_yearly, by = c(by, "year"))
  dplyr::select(ret, by, "year", "valid_in_year", dplyr::starts_with("quarter"), 
                "ann_98_percentile", "exceed", "excluded", 
                "ann_98_percentile_mgmt", "exceed_mgmt", dplyr::everything())
}