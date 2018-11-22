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

#' Compute specific CAAQS metrics
#' 
#' Compute specific CAAQS metrics for different forms of air pollution.
#'
#' @param data Data frame. Hourly raw pollution data with at least date-time and
#'   value columns
#' @param dt Character. The name of the date-time column. Default \code{"date"}
#' @param val Character. The name of the value column. Default \code{"value"}
#' @param by Character vector. Grouping variables in data, probably an id if 
#'   using multiple sites. Even if not using multiple sites, you should specify 
#'   the id column so that it is retained in the output.
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
#' @return `caaqs` object with results of the caaqs analysis, including results 
#' from intermediate steps. The final `caaqs` results can be extracted with the 
#' [extract_caaqs()] function and contains the following columns:
#'   \itemize{
#'     \item caaqs_year The year corresponding to the CAAQS metric
#'     \item metric The type of CAAQS metric calculated
#'     \item metric_value The CAAQS metric value, rounded to appropriate digits
#'     \item caaqs The CAAQS status, Achieved, Not Achieved, or Insufficient Data
#'     \item mgmt The management status actions
#'     \item excluded Logical value indicating whether any of the underlying
#'     data was excluded due to transboundary flows or exceptional events
#'     \item flag_daily_incomplete Logical value indicating whether any of the
#'     daily data was flagged as incomplete (see CAAQS guidelines for more
#'     details). If NA, indicates that this particular metric is never flagged.
#'     \item flag_yearly_incomplete Logical value indicating whether any of the
#'     yearly data was flagged as incomplete (see CAAQS guidelines for more
#'     details). If NA, indicates that this particular metric is never flagged.}
#'  
#'  To obtain any of the intermediate results data frames, use the 
#'  `caaqs_extractor` family of functions. See `?caaqs_extractors`
#' 
#' @references CCME Guidance document on achievement determination Canadian
#'   ambient air quality standards for fine particulate matter and ozone
#'   \url{https://www.ccme.ca/files/Resources/air/aqms/pn_1483_gdad_eng.pdf}.
#' 
#' @examples 
#' 
#' # Normal run
#' pm <- pm_24h_caaqs(pm25_sample_data, by = c("ems_id", "site"))
#' 
#' pm
#' 
#' extract_caaqs(pm)
#' 
#' # Exclude dates
#' high_dates <- data.frame(ems_id = "0310162",
#'                          site = "Port Moody Rocky Point Park", 
#'                          date = seq(as.Date("2012-06-11"),
#'                                     as.Date("2012-06-30"), by = "1 day"))
#'                                     
#' pm_ex <- pm_24h_caaqs(pm25_sample_data, 
#'                      by = c("ems_id", "site"),
#'                      exclude_df = high_dates,
#'                      exclude_df_dt = "date")
#'                      
#' extract_caaqs(pm_ex)
#' 
#' # Extract intermediate objects:
#' 
#' extract_daily(pm_ex)
#' extract_yearly(pm_ex)
#' extract_three_yr_rolling(pm_ex)
#' 
#' @name caaqs_metric
#' 
NULL
#> NULL


#' @importFrom rlang .data
caaqs <- function(data, year = "year", val, by, metric, n, management = FALSE) {

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
  if (!is.null(by)) {
    if (n == 3) {
      data <- dplyr::group_by_if(data, names(data) %in% by)
      check_groups(data, year)
    }
  }
  
  # Order by date (within grouping if applied)
  data <- dplyr::arrange(data, !!! rlang::syms(c(by, "year")))

  # Added details
  if (n == 3) {
    data <- dplyr::mutate(data, 
                          n_years = .data$n_val,
                          n_max = as.integer(max(.data$n_years, na.rm = TRUE)))
    
    data <- dplyr::ungroup(data)
  }

  # Determine station achievements
  data$caaqs <- cut_achievement(data$metric_value, metric, output = "labels")

  data$mgmt <- cut_management(data$metric_value, metric)
  data$metric <- metric
  
  # Clean up
  data <- dplyr::rename(data, "caaqs_year" = "year")
  cols_keep <- c(by, "caaqs_year", "min_year", "max_year", "n_years", "metric", 
                 "metric_value", "caaqs", 
                 # Only include management columns if management
                 if (management) c("mgmt", "excluded"),
                 "flag_daily_incomplete", "flag_yearly_incomplete",
                 "flag_two_of_three_years")
  cols_keep <- intersect(cols_keep, names(data))
  
  data <- dplyr::select(data, cols_keep)
  
  data
}

#' @rdname caaqs_metric
#' @export
pm_24h_caaqs <- function(data, dt = "date_time", val = "value",
                        by = NULL, quiet = FALSE) {

  if (!quiet) message("Calculating PM 2.5 daily average")
  daily <- pm_daily_avg(data, dt = dt, val = val, by = by)
  
  if (!quiet) message("Calculating PM 2.5 annual 98th percentile")
  yearly <- pm_yearly_98(daily, by = by, quiet = quiet)
  
  if (!quiet) message("Calculating PM 2.5 24h CAAQS metric")

  yearly_roll <- pm_three_yr_avg(yearly, val = "ann_98_percentile", by = by)

  caaqs <- caaqs(yearly_roll, val = "pm_metric", by = by, metric = "pm2.5_24h", 
                 n = 3)
  
  as.caaqs(
    list(daily_avg = daily, 
         yearly_98 = yearly, 
         three_yr_rolling = yearly_roll, 
         caaqs = caaqs
         ), 
    param = "pm2.5_24h", 
    dt = dt, val = val, by = by
  )
}

#' @rdname caaqs_metric
#' @export
pm_annual_caaqs <- function(data, dt = "date_time", val = "value",
                           by = NULL, exclude_df = NULL, exclude_df_dt = NULL,
                           quiet = FALSE) {
  
  if (!is.null(exclude_df)) check_exclude(data, dt, by,
                                         exclude_df, exclude_df_dt)
  
  if (!quiet) message("Calculating PM 2.5 daily average")
  daily <- pm_daily_avg(data, dt = dt, val = val, by = by)
  
  if (!quiet) message("Calculating PM 2.5 annual average ")
  yearly <- pm_yearly_avg(daily, by = by, 
                          exclude_df = exclude_df, 
                          exclude_df_dt = exclude_df_dt, 
                          quiet = quiet)
  
  if (!quiet) message("Calculating PM 2.5 annual CAAQS metric")
  
  yearly_roll <- pm_three_yr_avg(yearly, val = "ann_avg", by = by)
  caaqs <- caaqs(yearly_roll, val = "pm_metric", by = by, metric = "pm2.5_annual", n = 3)
  
  as.caaqs(
    list(daily_avg = daily,
         yearly_avg = yearly,
         three_yr_rolling = yearly_roll,
         caaqs = caaqs
         ),
    param = "pm2.5_annual", 
    dt = dt, val = val, by = by
  )
}

#' @rdname caaqs_metric
#' @export
o3_caaqs <- function(data, dt = "date_time", val = "value",
                    by = NULL, exclude_df = NULL, exclude_df_dt = NULL,
                    quiet = FALSE) {
  
  if (!is.null(exclude_df)) check_exclude(data, dt, by,
                                         exclude_df, exclude_df_dt)
  
  if (!quiet) message("Calculating O3 daily maximum of 8h average")
  daily8 <- o3_rolling_8hr_avg(data, dt = dt, val = val, by = by)
  daily <- o3_daily_max(daily8, by = by)
  
  if (!quiet) message("Calculating O3 annual 4th highest")
  yearly <- o3_ann_4th_highest(daily, by = by, 
                               exclude_df = exclude_df, 
                               exclude_df_dt = exclude_df_dt, 
                               quiet = quiet)
  
  if (!quiet) message("Calculating O3 CAAQS metric")
  yearly_roll <- o3_three_yr_avg(yearly, by = by)
  # TODO - when caaqs_management is finished, remove management = TRUE
  caaqs <- caaqs(yearly_roll, val = "ozone_metric", by = by, metric = "o3", 
                 n = 3, management = TRUE)
  
  as.caaqs(
    list(
      daily_max = daily,
      ann_4th_highest = yearly,
      three_yr_rolling = yearly_roll,
      caaqs = caaqs
    ),
    param = "o3", 
    dt = dt, val = val, by = by
  )
}

#' @rdname caaqs_metric
#' @export
so2_1yr_caaqs <- function(data, dt = "date_time", val = "value",
                         by = NULL, exclude_df = NULL, exclude_df_dt = NULL,
                         quiet = FALSE) {
  
  if (!is.null(exclude_df)) check_exclude(data, dt, by,
                                         exclude_df, exclude_df_dt)
  
  if (!quiet) message("Calculating SO2 annual average CAAQS metric")
  yearly <- so2_avg_hourly_by_year(data, dt = dt, val = val, by = by, 
                                   exclude_df = exclude_df, 
                                   exclude_df_dt = exclude_df_dt, 
                                   quiet = quiet)
  
  caaqs <- caaqs(yearly, val = "avg_yearly", by = by, metric = "so2_1yr", n = 1)
  
  as.caaqs(
    list(
      yearly_hr = yearly,
      caaqs = caaqs
    ),
    param = "so2_1yr", 
    dt = dt, val = val, by = by
  )
}

#' @rdname caaqs_metric
#' @export
so2_3yr_caaqs <- function(data, dt = "date_time", val = "value",
                         by = NULL, exclude_df = NULL, exclude_df_dt = NULL,
                         quiet = FALSE) {
  
  if (!is.null(exclude_df)) check_exclude(data, dt, by,
                                         exclude_df, exclude_df_dt)
  
  if (!quiet) message("Calculating SO2 daily maximum")
  daily <- so2_daily_max(data, dt = dt, val = val, by = by)
  
  if (!quiet) message("Calculating SO2 annual 99th percentile")
  yearly <- so2_yearly_99(daily, by = by, 
                          exclude_df = exclude_df, 
                          exclude_df_dt = exclude_df_dt, 
                          quiet = quiet)
  
  if (!quiet) message("Calculating SO2 1h CAAQS metric")
  yearly_roll <- so2_three_yr_avg(yearly, by = by)
  caaqs <- caaqs(yearly_roll, val = "so2_metric", by = by, metric = "so2_3yr", n = 3)
  
  as.caaqs(
    list(
      daily_max = daily,
      yearly_99 = yearly,
      three_yr_rolling = yearly_roll,
      caaqs = caaqs
      ), 
    param = "so2_3yr", 
    dt = dt, val = val, by = by
  )
}

#' @rdname caaqs_metric
#' @export
no2_1yr_caaqs <- function(data, dt = "date_time", val = "value",
                         by = NULL, exclude_df = NULL, exclude_df_dt = NULL,
                         quiet = FALSE) {
  
  if (!is.null(exclude_df)) check_exclude(data, dt, by,
                                         exclude_df, exclude_df_dt)
  
  if (!quiet) message("Calculating NO2 annual average CAAQS metric")
  yearly <- no2_avg_hourly_by_year(data, dt = dt, val = val, by = by, 
                                   exclude_df = exclude_df, 
                                   exclude_df_dt = exclude_df_dt, 
                                   quiet = quiet)
  caaqs <- caaqs(yearly, val = "avg_yearly", by = by, metric = "no2_1yr", n = 1)
  
  as.caaqs(
    list(
      yearly_hr = yearly,
      caaqs = caaqs
    ),
    param = "no2_1yr", 
    dt = dt, val = val, by = by
  )

}

#' @rdname caaqs_metric
#' @export
no2_3yr_caaqs <- function(data, dt = "date_time", val = "value",
                         by = NULL, exclude_df = NULL, exclude_df_dt = NULL,
                         quiet = FALSE) {
  
  if (!is.null(exclude_df)) check_exclude(data, dt, by,
                                          exclude_df, exclude_df_dt)
  
  if (!quiet) message("Calculating NO2 daily maximum")
  daily <- no2_daily_max(data, dt = dt, val = val, by = by)
  
  if (!quiet) message("Calculating NO2 annual 98th percentile")
  yearly <- no2_yearly_98(daily, by = by, 
                          exclude_df = exclude_df, 
                          exclude_df_dt = exclude_df_dt, 
                          quiet = quiet)
  
  if (!quiet) message("Calculating NO2 1h CAAQS metric")
  yearly_roll <- no2_three_yr_avg(yearly, by = by)
  caaqs <- caaqs(yearly_roll, val = "no2_metric", by = by, metric = "no2_3yr", n = 3)
  
  as.caaqs(
    list(
      daily_max = daily,
      yearly_98 = yearly,
      three_yr_rolling = yearly_roll,
      caaqs = caaqs
    ), 
    param = "no2_3yr", 
    dt = dt, val = val, by = by
  )

}
