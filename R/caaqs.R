
#' Calculate the three-year average for the Ozone metric
#' 
#' Calculates and returns the Ozone metric based on a rolling three-year
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
#'   metrics, achievement levels and management levels.#' 
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

  check_vars(list(year, val, by), data)
  
  # Rename to standard
  names(data)[names(data) == year] <- "year"
  names(data)[names(data) == val] <- "ozone_metric"
  
  # Start with ungrouped data
  data <- dplyr::ungroup(data)
  
  # Group Data
  if(!is.null(by)) data <- dplyr::group_by_if(data, names(data) %in% by)
  
  # Check for duplicates (considering grouping)
  dup <- dplyr::group_by(data, .data$year, add = TRUE)
  dup <- dplyr::summarize(dup, n = length(.data$year))
  if(any(dup$n > 1)) {
    msg <- paste0("Duplicate values in '", year, "'.")
    if(is.null(by)) msg <- paste0(msg, " Consider using 'by' to specify grouping argument(s).")
    if(!is.null(by)) msg <- paste0(msg, " Consider adding additional grouping variables to 'by'.")
    stop(msg)
  }
  
  # Calculate number of years in rolling averages
  data <- dplyr::mutate(data, 
                        n_years = dplyr::case_when(.data$valid == "FALSE" & .data$flag_two_of_three_years == "FALSE" ~ 1,
                                                   .data$valid == "TRUE" & .data$flag_two_of_three_years == "FALSE" ~ 3,
                                                   TRUE ~ 2),
                        n_total = length(.data$year),
                        min_year = min(.data$year), max_year = max(.data$year))
  
  # Extract 3-year average if 3 or more years in the data, otherwise, use 2-year average
  data <- dplyr::filter(data, (.data$n_years == 3 & .data$n_total >= 3) | 
                          (.data$n_years == 2 & .data$n_total == 2))
  
  data <- dplyr::ungroup(data)
  
  # Round ozone caaqs metric
  data$metric_value <- round_caaqs(data$ozone_metric)
  
  # Consider flagging data based on incomplete?
  
  ## Determine station achievements
  data$caaqs <- cut_achievement(data$metric_value, "o3", output = "labels")
  data$mgmt <- cut_management(data$metric_value, "o3")
  data$metric = "ozone"
  data$caaq_year <- data$year
  
  # Clean up
  data <- dplyr::select(data, dplyr::one_of(by, "caaq_year", "min_year", 
                                            "max_year", "n_years", "metric", 
                                            "metric_value", "caaqs", "mgmt"))
  
  data
}


so2_1yr_caaq <- function() {
  
  
  
}

so2_3yr_caaq <- function() {
  
  
  
}

no2_1yr_caaq <- function() {
  
  
  
}

no2_3yr_caaq <- function() {
  
  
  
}
