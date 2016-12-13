# Copyright 2015 Province of British Columbia
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

# Yearly statistics.
yearly_stat <- function(data, dt = "date", val = "value", 
                        by = c("ems_id", "site"), 
                        stat, stat.opts = NULL, quarter_units = "prop",
                        exclude_df, exclude_df_dt) {
  # Get quarter validity
  quarter_valid <- valid_by_quarter(data, dt, by, quarter_units)
  # Exclude data.
  if(!is.null(exclude_df)) data <- exclude_df(data, dt, by, exclude_df, exclude_df_dt)
  
  # Calculate yearly statistic
  data <- mutate_(data, year = interp(~get_year_from_date(dt), 
                                      dt = as.name(dt)))
  data <- group_by_(data, .dots = c(by, "year"))
  fun2 <- function(x) do.call(stat, c(list(x), stat.opts))
  data <- summarise_(data,
                     stat = interp(~fun2(value), 
                                   value = as.name(val), 
                                   fun = stat))
  data <- ungroup(data)
  left_join(data, quarter_valid, by = c(by, "year"))
}


#'Compute a yearly statistic (typically 98th percentile).
#'
#'@param data data frame with date and value
#'@param dt the name (as a character string) of the date-time column. Default
#'  \code{"date"}
#'@param val the name (as a character string) of the value column. Default
#'  \code{"value"}
#'@param by character vector of grouping variables in data, probably an id if
#'  using multiple sites. Even if not using multiple sites, you shoud specify
#'  the id column so that it is retained in the output.
#' @param  exclude_df he data.frame that has all the columns in the 
#' by parameter, in addition exactly one or two date columns.
#' @param  exclude_df_dt a character vector with exactly one or two date columns.
#' @return data frame with the yearly summary (typically the 98th percentile)
#' @name yearly_stat_page
NULL
#> NULL


#' @rdname yearly_stat_page
#' @importFrom  lubridate is.Date
#' @export
pm_yearly_98 <- function(data, dt = "date", val = "avg_24h", by = NULL, exclude_df = NULL, exclude_df_dt = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            is.Date(data[[dt]]), 
            is.numeric(data[[val]]))
  data <- yearly_stat(data, dt, val, by, quantile2_na, list(probs = 0.98, na.rm = TRUE), exclude_df = exclude_df, exclude_df_dt = exclude_df_dt)
  data <- rename_(data, ann_98_percentile = "stat")
  data$ann_98_percentile <- round(data$ann_98_percentile, 1)
  data$valid_year <-
    data$valid_in_year > 0.75 &
    data$quarter_1 > 0.6 &
    data$quarter_2 > 0.6 &
    data$quarter_3 > 0.6 &
    data$quarter_4 > 0.6
  standard <- get_std("pm2.5_24h")
  data$exceed <- data$ann_98_percentile > standard
  data$exceed <- ifelse(is.na(data$exceed), FALSE, data$exceed)
  data$flag_year_based_on_incomplete_data <- data$exceed & !data$valid_year
  data
}

#' @rdname yearly_stat_page
#' @importFrom  lubridate is.Date
#' @export
so2_yearly_99 <- function(data, dt = "date", val = "max_24h", by = NULL, exclude_df = NULL, exclude_df_dt = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            is.Date(data[[dt]]), 
            is.numeric(data[[val]]))
  data <- yearly_stat(data, dt, val, by, quantile2_na, list(probs = 0.99, na.rm = TRUE), exclude_df = exclude_df, exclude_df_dt = exclude_df_dt)
  data <- rename_(data, ann_99_percentile = "stat")
  data$ann_99_percentile <- round(data$ann_99_percentile, 1)
  data$valid_year <-
    data$valid_in_year > 0.75 &
    data$quarter_1 > 0.6 &
    data$quarter_2 > 0.6 &
    data$quarter_3 > 0.6 &
    data$quarter_4 > 0.6
  standard <- 70
  data$exceed <- data$ann_99_percentile > standard
  data$exceed <- ifelse(is.na(data$exceed), FALSE, data$exceed)
  data$flag_year_based_on_incomplete_data <- data$exceed & !data$valid_year
  data
}

#' @rdname yearly_stat_page
#' @importFrom  lubridate is.Date
#' @export
no2_yearly_98 <- function(data, dt = "date", val = "max_24h", by = NULL, exclude_df = NULL, exclude_df_dt = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            is.Date(data[[dt]]), 
            is.numeric(data[[val]]))
  data <- yearly_stat(data, dt, val, by, quantile2_na, list(probs = 0.98, na.rm = TRUE), exclude_df = exclude_df, exclude_df_dt = exclude_df_dt)
  data <- rename_(data, ann_98_percentile = "stat")
  data$ann_98_percentile <- round(data$ann_98_percentile, 1)
  data$valid_year <-
    data$valid_in_year > 0.75 &
    data$quarter_1 > 0.6 &
    data$quarter_2 > 0.6 &
    data$quarter_3 > 0.6 &
    data$quarter_4 > 0.6
  standard <- 70
  data
}


#' @rdname yearly_stat_page
#' @importFrom  lubridate is.Date
#' @export
pm_yearly_avg <- function(data, dt = "date", val = "avg_24h", by = NULL, exclude_df = NULL, exclude_df_dt = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            is.Date(data[[dt]]), 
            is.numeric(data[[val]]))
  data <- yearly_stat(data, dt, val, by, mean_na, exclude_df = exclude_df, exclude_df_dt = exclude_df_dt)
  data <- rename_(data, ann_avg = "stat")
  data$ann_avg <- round(data$ann_avg, 1)
  data$valid_year <-
    data$valid_in_year > 0.75 &
    data$quarter_1 > 0.6 &
    data$quarter_2 > 0.6 &
    data$quarter_3 > 0.6 &
    data$quarter_4 > 0.6
  data
}

#' @rdname yearly_stat_page
#' @importFrom  lubridate is.Date
#' @export
o3_ann_4th_highest <- function(data, dt = "date", val = "max8hr", by = NULL, exclude_df = NULL, exclude_df_dt = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            is.Date(data[[dt]]), 
            is.numeric(data[[val]]))
  data <- data[data$valid_max8hr | data$flag_max8hr_incomplete,]
  data <- yearly_stat(data, dt, val, by, 
                      nth_highest, stat.opts = list(n = 4), 
                      quarter_units = "days", exclude_df = exclude_df, exclude_df_dt = exclude_df_dt)
  data <- rename_(data, max8hr = "stat")
  days_in_quarter_2_and_3 <- 183 
  data$valid_year <-
    (data$quarter_2 + data$quarter_3) / days_in_quarter_2_and_3 > 0.75
  o3_standard <- get_std("o3")
  data$exceed <- data$max8hr > o3_standard
  data$exceed <- ifelse(is.na(data$exceed), FALSE, data$exceed)
  data$flag_year_based_on_incomplete_data <- data$exceed & !data$valid_year
  data
}

#' @rdname yearly_stat_page
#' @importFrom  lubridate is.POSIXt
#' @export
so2_avg_hourly_by_year <- function(data, dt = "date_time", val = "value", by = NULL, exclude_df = NULL, exclude_df_dt = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            is.POSIXt(data[[dt]]), 
            is.numeric(data[[val]]))
  data$date <- time_to_date(data[[dt]])
  data <- yearly_stat(data, "date", val, by, mean_na, quarter_units = "days", exclude_df = exclude_df, exclude_df_dt = exclude_df_dt) 
  data <- rename_(data, max_yearly = "stat")
  data$max_yearly <- round(data$max_yearly, 1)
  data$valid_in_year <- data$valid_in_year / (days_in_year(data$year)*24)
  for (q in 1:4)
    data[[paste0("quarter_",q)]] <- 
      data[[paste0("quarter_",q)]] / (days_in_quarter(q, data$year)*24)
  data$valid_year <-
    data$valid_in_year > 0.75 &
    data$quarter_1 > 0.6 &
    data$quarter_2 > 0.6 &
    data$quarter_3 > 0.6 &
    data$quarter_4 > 0.6
  so2.std <- 5
  data$exceed <- data$max_yearly > so2.std 
  data$exceed <- ifelse(is.na(data$exceed), FALSE, data$exceed)
  data
}

#' @rdname yearly_stat_page
#' @importFrom  lubridate is.POSIXt
#' @export
no2_avg_hourly_by_year <- function(data, dt = "date_time", val = "value", by = NULL, exclude_df = NULL, exclude_df_dt = NULL) {
  stopifnot(is.data.frame(data), 
            is.character(dt),
            is.character(val),
            is.POSIXt(data[[dt]]), 
            is.numeric(data[[val]]))
  data$date <- time_to_date(data[[dt]])
  data <- yearly_stat(data, "date", val, by, mean_na, quarter_units = "days", exclude_df = exclude_df, exclude_df_dt = exclude_df_dt) 
  data <- rename_(data, max_yearly = "stat")
  data$max_yearly <- round(data$max_yearly, 1)
  data$valid_in_year <- data$valid_in_year / (days_in_year(data$year)*24)
  for (q in 1:4)
    data[[paste0("quarter_",q)]] <- 
    data[[paste0("quarter_",q)]] / (days_in_quarter(q, data$year)*24)
  data$valid_year <-
    data$valid_in_year > 0.75 &
    data$quarter_1 > 0.6 &
    data$quarter_2 > 0.6 &
    data$quarter_3 > 0.6 &
    data$quarter_4 > 0.6
  data
}

#' Return the rank that should be used to determine the 98th percentile given a 
#' number of valid days
#' 
#' Wraps \code{\link[stats]{quantile}} but with different defaults, adds another
#' \code{type}, "caaqs", where the percentile (default 0.98) is calculated 
#' according to the caaqs methods.
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
#' @seealso \code{\link[stats]{quantile}}
#' @export
#' 
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
    ret <- quantile(x = x, probs = probs, na.rm = na.rm, names = names, type = type)
  }
  ret
}

nth_highest <- function(x,n)
  x[order(x, decreasing = TRUE)[n]]
