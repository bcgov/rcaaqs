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

as.caaqs_mgmt <- function(x, eetf = NULL) {
  class(x) <- unique(c("caaqs_mgmt", class(x)))
  attr(x, "eetf") <- eetf
  x
}

get_eetf <- function(x) {
  attr(x, "eetf")
}

#' Calculate CAAQS management levels excluding days with Exceptional Events 
#' or Transboundary Flows
#'
#' @param x an object of class `caaqs`
#' @param exclude_df Data frame. The dates over which data should be excluded 
#'   (see details). Data should be arranged either with one column of dates to 
#'   omit, or two columns specifying a series of start and end date ranges to 
#'   omit.
#' @param exclude_df_dt Character vector. The names of the date columns in 
#'   `exclude_df`. Must specify either one (a series of dates), or two (the
#'   start and end columns specifying dates ranges).
#' @param quiet Logical. Suppress progress messages (default FALSE)
#' 
#' @details To omit days which are suspected to be influenced by Transboundary
#'   Flows or Exceptional Events create a data frame that either a) contains a
#'   column listing all the days which are to be omitted, or b) contains two
#'   columns listing the start and end dates of all the date periods which are
#'   to be omitted. This is supplied as `exclude_df`. Use
#'   `exlcude_df_dt` to specify the name of the column containing the
#'   dates, or the names of the columns containing the start and end of the date
#'   ranges (see examples and vignette for more details).
#'
#' @return object of class `caaqs`
#'
#' @examples
#' 
#' pm <- pm_24h_caaqs(pm25_sample_data, by = c("ems_id", "site"))
#' 
#' pm
#' 
#' get_caaqs(pm)
#' 
#' # Exclude dates
#' high_dates <- data.frame(ems_id = "0310162",
#'                          site = "Port Moody Rocky Point Park", 
#'                          date = seq(as.Date("2012-06-11"),
#'                                     as.Date("2012-06-30"), by = "1 day"))
#'                                     
#' pm_ex <- caaqs_management(pm, exclude_df = high_dates, exclude_df_dt = "date")
#' 
#' pm_ex
#' 
#' get_caaqs(pm_ex)
#' 
#' @export
caaqs_management <- function(x, exclude_df = NULL, exclude_df_dt = NULL, quiet = FALSE) {
  UseMethod("caaqs_management")
}

#' @export
caaqs_management.default <- function(x, exclude_df = NULL, exclude_df_dt = NULL, 
                                     quiet = FALSE) {
  stop("No method defined for object of type ", 
       paste(class(x), collapse = ", "), call. = FALSE)
}

#' @export
caaqs_management.pm2.5_24h <- function(x, exclude_df = NULL, exclude_df_dt = NULL, 
                                       quiet = FALSE) {
  caaqs_management_pm_no2_so2_3yr(x, exclude_df, exclude_df_dt, quiet)
}

#' @export
caaqs_management.pm2.5_annual <- function(x, exclude_df = NULL, exclude_df_dt = NULL, 
                                       quiet = FALSE) {
  caaqs_management_pm_no2_so2_3yr(x, exclude_df, exclude_df_dt, quiet)
}

#' @export
caaqs_management.so2_3yr <- function(x, exclude_df = NULL, exclude_df_dt = NULL, 
                                     quiet = FALSE) {
  caaqs_management_pm_no2_so2_3yr(x, exclude_df, exclude_df_dt, quiet)
}

#' @export
caaqs_management.no2_3yr <- function(x, exclude_df = NULL, exclude_df_dt = NULL, 
                                     quiet = FALSE) {
  caaqs_management_pm_no2_so2_3yr(x, exclude_df, exclude_df_dt, quiet)
}

caaqs_management_pm_no2_so2_3yr <- function(x, exclude_df, exclude_df_dt, quiet) {
  daily <- get_daily(x)
  by <- get_by(x)
  
  if (!is.null(exclude_df)) {
    check_exclude(daily, dt = "date",
                  by = by,
                  exclude_df, exclude_df_dt)
  }
  
  parameter <- get_param(x)
  
  yearly_fun <- switch(parameter, 
                       "pm2.5_annual" = pm_yearly_avg, 
                       "pm2.5_24h" = pm_yearly_98, 
                       "so2_3yr" = so2_yearly_99, 
                       "no2_3yr" = no2_yearly_98)
  
  yearly_roll_fun <- switch(parameter, 
                            "pm2.5_annual" = pm_three_yr_avg, 
                            "pm2.5_24h" = pm_three_yr_avg, 
                            "so2_3yr" = so2_three_yr_avg,
                            "no2_3yr" = no2_three_yr_avg)
  
  caaqs_val <- switch(parameter, 
                      "pm2.5_annual" = "pm_metric", 
                      "pm2.5_24h" = "pm_metric", 
                      "so2_3yr" = "so2_metric",
                      "no2_3yr" = "no2_metric")
  
  yearly_obj <- switch(parameter, 
                       "pm2.5_annual" = "yearly_avg", 
                       "pm2.5_24h" = "yearly_98", 
                       "so2_3yr" = "yearly_99",
                       "no2_3yr" = "yearly_98")
  
  if (!quiet) message("Calculating ", get_annual_stat(parameter, "long"))
  yearly_mgmt <- yearly_fun(daily, 
                            by = by, 
                            exclude_df = exclude_df, 
                            exclude_df_dt = exclude_df_dt, 
                            management = TRUE,
                            quiet = quiet)
  
  if (!quiet) message("Calculating, " , params()[parameter], " CAAQS metric")
  
  yearly_roll_mgmt <- yearly_roll_fun(yearly_mgmt, 
                                      val = get_annual_stat(parameter), 
                                      by = by)
  
  caaqs_mgmt <- caaqs(yearly_roll_mgmt, val = caaqs_val, by = by, 
                      metric = parameter, n = 3, management = TRUE)
  # Add new columns or objects to caaqs object, update class, return modified 
  # caaqs object
  
  x[[yearly_obj]] <- join_management_yearly(get_yearly(x), yearly_mgmt, 
                                             parameter = parameter, by, 
                                            eetf = !is.null(exclude_df))
  
  x[["three_yr_rolling"]] = join_management_yearly(get_three_yr_rolling(x), 
                                                   yearly_roll_mgmt, 
                                                   parameter = parameter, 
                                                   by = by, eetf = !is.null(exclude_df))
  
  x[["caaqs"]] <- join_management_caaqs(get_caaqs(x), caaqs_mgmt, by = by, 
                                        eetf = !is.null(exclude_df))
  
  as.caaqs_mgmt(x, eetf = exclude_df)
}

#' @export
caaqs_management.o3 <- function(x, exclude_df = NULL, exclude_df_dt = NULL, 
                                quiet = FALSE) {
  daily <- get_daily(x)
  by <- get_by(x)
  
  if (!is.null(exclude_df)) {
    check_exclude(daily, dt = "date",
                  by = by,
                  exclude_df, exclude_df_dt)
  }
  
  if (!quiet) message("Calculating O3 annual 4th highest")
  yearly_mgmt <- o3_ann_4th_highest(daily, by = by, 
                                    exclude_df = exclude_df, 
                                    exclude_df_dt = exclude_df_dt, 
                                    management = TRUE,
                                    quiet = quiet)
  
  if (!quiet) message("Calculating O3 CAAQS metric")
  yearly_roll_mgmt <- o3_three_yr_avg(yearly_mgmt, by = by)
  
  caaqs_mgmt <- caaqs(yearly_roll_mgmt, val = "ozone_metric", by = by, metric = "o3", 
                      n = 3, management = TRUE)
  
  x[["ann_4th_highest"]] <- join_management_yearly(get_yearly(x), yearly_mgmt, 
                                                   parameter = "o3", by, 
                                                   eetf = !is.null(exclude_df))
  
  x[["three_yr_rolling"]] = join_management_yearly(get_three_yr_rolling(x), 
                                                   yearly_roll_mgmt, 
                                                   parameter = "o3", 
                                                   by = by, 
                                                   eetf = !is.null(exclude_df))
  
  x[["caaqs"]] <- join_management_caaqs(get_caaqs(x), caaqs_mgmt, by = by, 
                                        eetf = !is.null(exclude_df))
  
  as.caaqs_mgmt(x, eetf = exclude_df)
}

#' @export
caaqs_management.so2_1yr <- function(x, exclude_df = NULL, exclude_df_dt = NULL, 
                                     quiet = FALSE) {
  caaqs_management_so2_no2_1yr(x, exclude_df, exclude_df_dt, quiet)
}

#' @export
caaqs_management.no2_1yr <- function(x, exclude_df = NULL, exclude_df_dt = NULL, 
                                     quiet = FALSE) {
  caaqs_management_so2_no2_1yr(x, exclude_df, exclude_df_dt, quiet)
}

caaqs_management_so2_no2_1yr <- function(x, exclude_df, exclude_df_dt, 
                                quiet) {
  
  hourly <- get_hourly(x)
  by <- get_by(x)
  
  parameter <- get_param(x)
  
  yearly_fun <- switch(parameter, 
                       "so2_1yr" = so2_avg_hourly_by_year, 
                       "no2_1yr" = no2_avg_hourly_by_year)
  
  if (!quiet) message("Calculating, " , params()[parameter], " CAAQS metric")
  yearly_mgmt <- yearly_fun(hourly, dt = get_dt(x), 
                            val = get_val(x), by = by, 
                            exclude_df = exclude_df, 
                            exclude_df_dt = exclude_df_dt, 
                            management = TRUE,
                            quiet = quiet)
  
  caaqs_mgmt <- caaqs(yearly_mgmt, val = "avg_yearly", by = by, 
                      metric = parameter, n = 1, management = TRUE)
  
  x[["yearly_hr"]] <- join_management_yearly(get_yearly(x), yearly_mgmt, 
                                             parameter = parameter, by = by, 
                                             eetf = !is.null(exclude_df))
  
  x[["caaqs"]] <- join_management_caaqs(get_caaqs(x), caaqs_mgmt, by = by, 
                                        eetf = !is.null(exclude_df))
  
  as.caaqs_mgmt(x, eetf = exclude_df)
}

join_management_caaqs <- function(caaqs, mgmt_caaqs, by, eetf) {
  mgmt_caaqs <- dplyr::select(mgmt_caaqs, by, "caaqs_year", "metric", 
                        "metric_value_mgmt" = "metric_value", 
                        "mgmt", "excluded")
  ret <- dplyr::left_join(caaqs, mgmt_caaqs, by = c(by, "caaqs_year", "metric"))
  dplyr::select(ret, by, "caaqs_year", "metric", 
                "metric_value_ambient" = "metric_value", 
                "caaqs_ambient" = "caaqs", 
                "excluded", "metric_value_mgmt", "mgmt_level" = "mgmt", 
                dplyr::everything())
}

join_management_yearly <- function(yearly, mgmt_yearly, parameter, by, eetf) {
  annual_stat <- get_annual_stat(parameter)
  mgmt_yearly <- dplyr::select(mgmt_yearly, by, "year", "excluded", 
                               annual_stat, "exceed_mgmt" = "exceed")
  names(mgmt_yearly)[names(mgmt_yearly) == annual_stat] <- paste0(annual_stat, "_mgmt")
  
  ret <- dplyr::left_join(yearly, mgmt_yearly, by = c(by, "year"))
  dplyr::select(ret, by, "year", "valid_in_year", dplyr::starts_with("quarter"), 
                annual_stat, "exceed", "excluded", 
                paste0(annual_stat, "_mgmt"), "exceed_mgmt", dplyr::everything())
}

get_annual_stat <- function(parameter, which = "short") {
  annual_stat <- switch(parameter, 
         "pm2.5_24h" = c("ann_98_percentile" = "PM 2.5 annual 98th percentile"), 
         "pm2.5_annual" = c("ann_avg" = "PM 2.5 annual average"), 
         "o3" = c("ann_4th_highest" = "O3 annual 4th highest"), 
         "so2_1yr" = c("avg_yearly" = "SO2 annual average"), 
         "so2_3yr" = c("ann_99_percentile" = "SO2 annual 99th percentile"),
         "no2_1yr" = c("avg_yearly" = "NO2 annual average"), 
         "no2_3yr" = c("ann_98_percentile" = "NO2 annual 98th percentile"))
  switch(which, 
         "short" = names(annual_stat), 
         "long" = unname(annual_stat))
}
