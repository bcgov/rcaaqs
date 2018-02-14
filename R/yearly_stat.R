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


# Yearly statistics
yearly_stat <- function(data, dt = "date", val = "value", 
                        by = c("ems_id", "site"), 
                        stat, stat.opts = NULL, quarter_units = "prop",
                        pollutant_standard,
                        exclude_df, exclude_df_dt, quiet = FALSE) {
  
  # Check inputs
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            dt %in% names(data),
            val %in% names(data),
            is.numeric(data[[val]]))
  
  data <- dplyr::mutate(data, year = lubridate::year(!!!rlang::syms(dt)))
  
  # Apply grouping
  data <- dplyr::group_by(data, !!!rlang::syms(c(by, "year")))
  check_groups(data, dt)

  # Get quarter validity
  quarter_valid <- valid_by_quarter(data, dt, by, val, quarter_units)

  # Exclude data
  if(!is.null(exclude_df)) {
    data <- exclude_data(data, dt, by, 
                         exclude_df, exclude_df_dt,
                         val = val, quiet = quiet)
  } else {
    data$excluded <- FALSE
  }

  # Calculate yearly statistic
  if(!is.null(stat.opts)) {
    data <- dplyr::summarize(data, 
                             stat = stat(!!!rlang::syms(val), !!unlist(stat.opts)),
                             excluded = any(excluded),
                             flag_daily_incomplete = any(flag_daily_incomplete))
  } else {
    data <- dplyr::summarize(data, 
                             stat = stat(!!!rlang::syms(val)),
                             excluded = any(excluded),
                             flag_daily_incomplete = any(flag_daily_incomplete))
  }
  
  # Round data / Exceeds threshold
  data <- dplyr::mutate(data, 
                        stat = round_caaqs(.data$stat, 1),
                        exceed = .data$stat > pollutant_standard)
  
  data <- dplyr::ungroup(data)
  dplyr::left_join(quarter_valid, data, by = c(by, "year"))
}


#' Compute a yearly statistic (typically 98th percentile).
#'
#' @param data data frame with date and value
#' @param dt the name (as a character string) of the date-time column. Default 
#'   \code{"date"}
#' @param val the name (as a character string) of the value column. Default 
#'   \code{"value"}
#' @param by character vector of grouping variables in data, probably an id if 
#'   using multiple sites. Even if not using multiple sites, you shoud specify 
#'   the id column so that it is retained in the output.
#' @param  exclude_df he data.frame that has all the columns in the by
#'   parameter, in addition exactly one or two date columns.
#' @param  exclude_df_dt a character vector with exactly one or two date
#'   columns.
#'   
#' @return data frame with the yearly summary (typically the 98th percentile)
#' 
#' @name yearly_stat_page
#' 
NULL
#> NULL


#' @rdname yearly_stat_page
#' @export

pm_yearly_98 <- function(data, dt = "date", val = "avg_24h", by = NULL, 
                         exclude_df = NULL, exclude_df_dt = NULL, 
                         quiet = FALSE) {

  data <- yearly_stat(data, dt, val, by, quantile2_na, 
                      list(probs = 0.98, na.rm = TRUE), 
                      pollutant_standard = get_std("pm2.5_24h"),
                      exclude_df = exclude_df, 
                      exclude_df_dt = exclude_df_dt, 
                      quiet = quiet)
  
  data <- dplyr::rename(data, "ann_98_percentile" = "stat")
  
  # Determine data completeness
  data$valid_year <- data$valid_in_year >= 0.75
  data$valid_quarters <- data$quarter_1 >= 0.6 &
                         data$quarter_2 >= 0.6 &
                         data$quarter_3 >= 0.6 &
                         data$quarter_4 >= 0.6
  
  # Remove data which invalid - Flag invalid data which exceeds (thus kept)
  data$ann_98_percentile[!data$valid_year] <- NA
  data$ann_98_percentile[data$valid_year & !data$valid_quarters & !data$exceed] <- NA
  data$flag_yearly_incomplete <- data$valid_year & !data$valid_quarters & data$exceed
  
  data
}

#' @rdname yearly_stat_page
#' @export

so2_yearly_99 <- function(data, dt = "date", val = "max_24h", by = NULL, 
                          exclude_df = NULL, exclude_df_dt = NULL, 
                          quiet = FALSE) {
  
  data <- yearly_stat(data, dt, val, by, quantile2_na, 
                      list(probs = 0.99, na.rm = TRUE), 
                      pollutant_standard = get_std("so2_3yr"),
                      exclude_df = exclude_df, 
                      exclude_df_dt = exclude_df_dt, 
                      quiet = quiet)
  
  data <- dplyr::rename(data, "ann_99_percentile" = "stat")
  
  # Determine data completeness
  data$valid_year <- data$valid_in_year >= 0.75 &
                       data$quarter_1 >= 0.6 &
                       data$quarter_2 >= 0.6 &
                       data$quarter_3 >= 0.6 &
                       data$quarter_4 >= 0.6
  
  data$ann_99_percentile[!data$valid_year & !data$exceed] <- NA
  data$flag_yearly_incomplete <- !data$valid_year & data$exceed
  
  data
}

#' @rdname yearly_stat_page
#' @export

no2_yearly_98 <- function(data, dt = "date", val = "max_24h", by = NULL, 
                          exclude_df = NULL, exclude_df_dt = NULL, 
                          quiet = FALSE) {

  data <- yearly_stat(data, dt, val, by, quantile2_na, 
                      list(probs = 0.98, na.rm = TRUE), 
                      pollutant_standard = get_std("no2_3yr"),
                      exclude_df = exclude_df, 
                      exclude_df_dt = exclude_df_dt, 
                      quiet = quiet)
  
  data <- dplyr::rename(data, "ann_98_percentile" = "stat")
  
  data$valid_year <- data$valid_in_year >= 0.75 &
                     data$quarter_1 >= 0.6 &
                     data$quarter_2 >= 0.6 &
                     data$quarter_3 >= 0.6 &
                     data$quarter_4 >= 0.6
  
  data$ann_98_percentile[!data$valid_year] <- NA
  
  # No yearly incomplete flags for NO2
  data$flag_yearly_incomplete <- NA
  
  data
}


#' @rdname yearly_stat_page
#' @export

pm_yearly_avg <- function(data, dt = "date", val = "avg_24h", by = NULL, 
                          exclude_df = NULL, exclude_df_dt = NULL, 
                          quiet = FALSE) {

  data <- yearly_stat(data, dt, val, by, mean_na, 
                      pollutant_standard = get_std("pm2.5_annual"),
                      exclude_df = exclude_df, 
                      exclude_df_dt = exclude_df_dt, 
                      quiet = quiet)
  
  data <- dplyr::rename(data, "ann_avg" = "stat")
  
  # Determine data completeness
  data$valid_year <- data$valid_in_year >= 0.75
  data$valid_quarters <- data$quarter_1 >= 0.6 &
                         data$quarter_2 >= 0.6 &
                         data$quarter_3 >= 0.6 &
                         data$quarter_4 >= 0.6
  
  # Remove data which invalid
  data$ann_avg[!(data$valid_year & data$valid_quarters)] <- NA
  
  # No yearly incomplete flags for annual PM2.5
  
  data
}

#' @rdname yearly_stat_page
#' @export

o3_ann_4th_highest <- function(data, dt = "date", val = "max8hr", by = NULL, 
                               exclude_df = NULL, exclude_df_dt = NULL, 
                               quiet = FALSE) {

  data <- yearly_stat(data, dt, val, by, 
                      nth_highest, stat.opts = list(n = 4), 
                      quarter_units = "days", 
                      pollutant_standard = get_std("o3"),
                      exclude_df = exclude_df, 
                      exclude_df_dt = exclude_df_dt, 
                      quiet = quiet)
  
  data <- dplyr::rename(data, "max8hr" = "stat")
  
  # Determine data completeness
  days_in_quarter_2_and_3 <- 183
  data$valid_year <- (data$quarter_2 + data$quarter_3) / days_in_quarter_2_and_3 >= 0.75
  
  # Remove data which invalid - Flag invalid data which exceeds (thus kept)
  data$max8hr[!data$valid_year & !data$exceed] <- NA
  data$flag_yearly_incomplete <- !data$valid_year & data$exceed
  
  data
}

#' @rdname yearly_stat_page
#' @export

so2_avg_hourly_by_year <- function(data, dt = "date_time", val = "value", 
                                   by = NULL, exclude_df = NULL, 
                                   exclude_df_dt = NULL, 
                                   quiet = FALSE) {

  # Initial data checks for first time raw data is passed to rcaaqs
  data <- initial_check(data, dt = dt, val = val, by = by)
  
  # Add flag placeholder
  data$flag_daily_incomplete <- NA
  
  data <- yearly_stat(data, dt, val, by, mean_na, 
                      quarter_units = "days", 
                      pollutant_standard = get_std("so2_1yr"),
                      exclude_df = exclude_df, 
                      exclude_df_dt = exclude_df_dt, 
                      quiet = quiet) 
  
  data <- dplyr::rename(data, "avg_yearly" = "stat")
  
  # Determine data completeness
  data$valid_in_year <- data$valid_in_year / (days_in_year(data$year) * 24)
                          
  for (q in 1:4) {
    data[[paste0("quarter_",q)]] <- data[[paste0("quarter_",q)]] / (days_in_quarter(q, data$year) * 24)
  }
  
  data$valid_year <- data$valid_in_year >= 0.75 &
                       data$quarter_1 >= 0.6 &
                       data$quarter_2 >= 0.6 &
                       data$quarter_3 >= 0.6 &
                       data$quarter_4 >= 0.6
  
  data$avg_yearly[!data$valid_year & !data$exceed] <- NA
  data$flag_yearly_incomplete <- !data$valid_year & data$exceed
  
  data
}

#' @rdname yearly_stat_page
#' @export

no2_avg_hourly_by_year <- function(data, dt = "date_time", val = "value", 
                                   by = NULL, exclude_df = NULL, 
                                   exclude_df_dt = NULL, 
                                   quiet = FALSE) {

  # Initial data checks for first time raw data is passed to rcaaqs
  data <- initial_check(data, dt = dt, val = val, by = by)
 
  # Add flag placeholder
  data$flag_daily_incomplete <- NA
   
  data <- yearly_stat(data, dt, val, by, mean_na, 
                      quarter_units = "days", 
                      pollutant_standard = get_std("no2_1yr"),
                      exclude_df = exclude_df, 
                      exclude_df_dt = exclude_df_dt, 
                      quiet = quiet) 
  
  data <- dplyr::rename(data, "avg_yearly" = "stat")
  
  # Determine data completeness
  data$valid_in_year <- data$valid_in_year / (days_in_year(data$year) * 24)
  
  for (q in 1:4) {
    data[[paste0("quarter_",q)]] <- 
      data[[paste0("quarter_",q)]] / (days_in_quarter(q, data$year) * 24)
  }
  
  data$valid_year <- data$valid_in_year >= 0.75 &
                       data$quarter_1 >= 0.6 &
                       data$quarter_2 >= 0.6 &
                       data$quarter_3 >= 0.6 &
                       data$quarter_4 >= 0.6
  
  data$avg_yearly[!data$valid_year] <- NA
  data$flag_yearly_incomplete <- NA
  
  # No yearly incomplete flags for hourly NO2
  
  data
}

#' Return the rank that should be used to determine the 98th percentile given a 
#' number of valid days
#' 
#' Wraps \code{\link[stats]{quantile}} but with different defaults, adds another
#' \code{type}, "caaqs", where the percentile (default 0.98) is calculated 
#' according to the caaqs methods.
#' 
#' @param x numeric vector whose sample quantiles are wanted.
#' @param probs numeric vector of probablities with values in \eqn{[0,1]}. 
#'   Default \code{0.98}
#' @param na.rm logical; if true, any \code{NA} and \code{NaN}'s are removed 
#'   from \code{x} before the quantiles are computed. Default \code{FALSE}
#' @param names logical; if true, the result has a names attribute. Set to FALSE
#'   for speedup with many probs. Default \code{FALSE}
#' @param type \code{"caaqs"} (default) or an integer between 1 and 9 selecting
#'   one of the nine base quantile algorithms be used. See
#'   \code{\link[stats]{quantile}} for details
#'   
#' @return A vector of \code{length(probs)}; if \code{names = TRUE}, it has a
#'   \code{names} attribute
#'   
#' @seealso \code{\link[stats]{quantile}}
#' 
#' @noRd

quantile2 <- function(x, probs = 0.98, na.rm = FALSE, names = FALSE, type = "caaqs") {
  if (missing(x) || length(x) == 0) {
    warning("No non-missing arguments to quantile2; returning -Inf")
    return(-Inf)
  }
  if (!inherits(x, c("integer", "numeric"))) stop("x is not numeric")
  if (!all(probs <= 1 & probs >= 0)) stop("probs should be between 0 and 1")
  if (!(type == "caaqs" || type %in% 1:9)) stop("type needs to be either 'caaqs' or 1:9")
  
  if (type == "caaqs") {
    if (na.rm) {
      x <- x[!is.na(x)]
    } else if (anyNA(x)) {
      stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
    }
    x <- sort(x, decreasing = TRUE)
    n <- length(x)
    i.d <- probs * n
    i <- floor(i.d)
    ret <- x[n - i]
    if (names) names(ret) <- paste0(probs * 100, "%")
  } else {
    ret <- stats::quantile(x = x, probs = probs, na.rm = na.rm, names = names, type = type)
  }
  ret
}


nth_highest <- function(x, n) {
  x[order(x, decreasing = TRUE)[n]]
}

