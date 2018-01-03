
#' Calculate the three-year average for the Ozone CAAQ metric
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

  check_vars(list(year, val, by), df)
  
  # Rename to standard
  names(df)[names(df) == year] <- "year"
  names(df)[names(df) == val] <- "ozone_metric"
  
  # Start with ungrouped df
  df <- dplyr::ungroup(df)
  
  # Group df if supplied
  if(!is.null(by)) df <- dplyr::group_by_if(df, names(df) %in% by)
  
  # Check for duplicates (considering grouping)
  dup <- dplyr::group_by(df, .data$year, add = TRUE)
  dup <- dplyr::summarize(dup, n = length(.data$year))
  if(any(dup$n > 1)) {
    msg <- paste0("Duplicate values in '", year, "'.")
    if(is.null(by)) msg <- paste0(msg, " Consider using 'by' to specify grouping argument(s).")
    if(!is.null(by)) msg <- paste0(msg, " Consider adding additional grouping variables to 'by'.")
    stop(msg)
  }
  
  # Order by date (within grouping if applied)
  df <- dplyr::arrange(df, !!!lapply(c(by, "year"), as.name))
  
  # Calculate number of years in rolling averages
  df <- dplyr::mutate(df, 
                      year_lag1 = dplyr::lag(.data$year),
                      year_lag2 = dplyr::lag(.data$year, 2),
                      n_total = length(.data$year),
                      min_year = dplyr::case_when(!is.na(.data$year_lag2) ~ .data$year_lag2,
                                                  !is.na(.data$year_lag1) ~ .data$year_lag1,
                                                  TRUE ~ .data$year),
                      max_year = .data$year,
                      n_years = .data$max_year - .data$min_year + 1L)
  
  # Extract 3-year average if 3 or more years in the data, otherwise, use 2-year average
  df <- dplyr::filter(df, (.data$n_years == 3 & .data$n_total >= 3) | 
                          (.data$n_years == 2 & .data$n_total == 2))
  
  df <- dplyr::ungroup(df)
  
  # Round ozone caaqs metric
  df$metric_value <- round_caaqs(df$ozone_metric)
  
  # Consider flagging data based on incomplete?
  
  ## Determine station achievements
  df$caaqs <- cut_achievement(df$metric_value, "o3", output = "labels")
  df$mgmt <- cut_management(df$metric_value, "o3")
  df$metric = "ozone"
  df$caaq_year <- df$year
  
  # Clean up
  df <- dplyr::select(df, dplyr::one_of(by, "caaq_year", "min_year", 
                                            "max_year", "n_years", "metric", 
                                            "metric_value", "caaqs", "mgmt"))
  
  df
}


so2_1yr_caaq <- function() {
  
  
  
}

so2_3yr_caaq <- function() {
  
  
  
}

no2_1yr_caaq <- function() {
  
  
  
}

no2_3yr_caaq <- function() {
  
  
  
}
