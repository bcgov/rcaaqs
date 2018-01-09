#' @importFrom rlang .data
caaq <- function(df, year, val, by, metric, n) {

  # Check inputs
  check_vars(list(year, val, by), df)
  check_class(year, df, "numeric")
  check_class(val, df, "numeric")
  
  # Rename to standard
  names(df)[names(df) == year] <- "year"
  names(df)[names(df) == val] <- "metric_value"
  
  # Start with ungrouped df
  df <- dplyr::ungroup(df)
  
  # Group df if supplied
  if(!is.null(by)) {
    if(n == 1) message("Grouping doesn't apply to single year caaqs. 'by' ignored.")
    if(n == 3) df <- dplyr::group_by_if(df, names(df) %in% by)
  }
  
  # Order by date (within grouping if applied)
  df <- dplyr::arrange(df, !!! rlang::syms(c(by, "year")))
  
  if(n == 1) {
    # Round caaqs metric (already completed for 3yr, but necessary for 1yr)
    df$metric_value <- round_caaqs(df$metric_value)
  }
  if(n == 3) {
    
    # Check for duplicates (considering grouping)
    dup <- dplyr::group_by(df, .data$year, add = TRUE)
    dup <- dplyr::summarize(dup, n = length(.data$year))
    if(any(dup$n > 1)) {
      msg <- paste0("Duplicate values in '", year, "'.")
      if(is.null(by)) msg <- paste0(msg, " Consider using 'by' to specify grouping argument(s).")
      if(!is.null(by)) msg <- paste0(msg, " Consider adding additional grouping variables to 'by'.")
      stop(msg, call. = FALSE)
    }

    # Calculate min/max and total number of years in rolling averages
    df <- dplyr::mutate(df, 
                        year_lag1 = dplyr::lag(.data$year),
                        year_lag2 = dplyr::lag(.data$year, 2),
                        min_year = dplyr::case_when(!is.na(.data$year_lag2) ~ .data$year_lag2,
                                                    !is.na(.data$year_lag1) ~ .data$year_lag1,
                                                    TRUE ~ .data$year),
                        max_year = .data$year,
                        n_max = as.integer(max(.data$n_years, na.rm = TRUE)))
    
    # Extract 3-year average if 3 or more years in the data, otherwise, use 2-year average
    df <- dplyr::filter(df, .data$n_years == .data$n_max)
    
    df <- dplyr::ungroup(df)
  }

  # Consider flagging data based on incomplete?
  
  ## Determine station achievements
  df$caaqs <- cut_achievement(df$metric_value, metric, output = "labels")
  df$mgmt <- cut_management(df$metric_value, metric)
  df$metric = metric
  df$caaq_year <- df$year
  
  # Clean up
  df <- dplyr::select(df, dplyr::one_of(by, "caaq_year", "min_year", 
                                        "max_year", "n_years", "metric", 
                                        "metric_value", "caaqs", "mgmt"))
  
  df
}

#' Calculate the PM2.5 24 hour CAAQ metric
#' 
#' Calculates and returns the PM2.5 24 hour CAAQ metric based on a rolling
#' three-year average.
#'
#' @param df Data frame. Contains three year PM2.5 24hr average data output from
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
pm_24h_caaq <- function(df, year = "year", val = "pm_metric",
                        by = NULL) {
  caaq(df, year, val, by, metric = "pm2.5_24h", n = 3)
}

#' Calculate the PM2.5 annaul CAAQ metric
#' 
#' Calculates and returns the PM2.5 annual CAAQ metric based on a rolling
#' three-year average.
#'
#' @param df Data frame. Contains PM2.5 annual three year average data output
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
pm_annual_caaq <- function(df, year = "year", val = "pm_metric",
                         by = NULL) {
  caaq(df, year, val, by, metric = "pm2.5_annual", n = 3)
}

#' Calculate the Ozone CAAQ metric
#' 
#' Calculates and returns the Ozone CAAQ metric based on a rolling three-year 
#' average.
#'
#' @param df Data frame. Contains three year average data output from
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
o3_caaq <- function(df, year = "year", val = "ozone_metric", by = NULL) {
  caaq(df, year, val, by, metric = "o3", n = 3)
}

#' @export
so2_1yr_caaq <- function(df, year = "year", val = "ann_99_percentile", by = NULL) {
  
  # CHECK VALIDITY
  caaq(df, year, val, by, metric = "so2_1yr", n = 1)
}

#' @export
so2_3yr_caaq <- function(df, year = "year", val = "so2_metric", by = NULL) {
  caaq(df, year, val, by, metric = "so2_3yr", n = 3)
}

#' @export
no2_1yr_caaq <- function(df, year = "year", val = "avg_yearly", by = NULL) {
  caaq(df, year, val, by, metric = "no2_1yr", n = 1)
}

#' @export
no2_3yr_caaq <- function(df, year = "year", val = "no2_metric", by = NULL) {
  caaq(df, year, val, by, metric = "no2_3yr", n = 3)
}
