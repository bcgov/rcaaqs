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

# Window functions.
rolling_sum <- function(x, width) {
  x <- ifelse(is.na(x), 0, x)
  cumsum(x) - lag(cumsum(x), width, default = 0)
}

rolling_mean <- function(x, width = 8, thresh = 0){
  available <- rolling_sum(!is.na(x), width = width)
  sum <- rolling_sum(x, width = width)
  ifelse(available >= thresh, sum/available, NA)
}

#'number within a rolling window and step size (for dates or numeric)
#'
#' @importFrom tidyr full_seq
#' @param x the date, date_time, or numeric value
#' @param interval some numeric step size. for date-times, in seconds.
#' @param window some numeric look-back window.
#' @return vector with all input dates and values, interpolated with NAs.
n_within_window <- function(x, interval, window){
  full_x <- full_seq(x, interval)
  rolling_sum(full_x %in% x, width = window)[match(x,full_x)]
}

# Check for bad intervals? (Duplicated dates. etc.)
filled_rolling_mean <- function(dat, val, interval = 3600, 
                                width = 8, valid_thresh = 6, digits = 1) {
  dat_val_padded <- pad_date_time(dat, val, interval)
  roll_mean <- round(rolling_mean(
    dat_val_padded$val, width, valid_thresh), digits)
  # Ensure order and length is same as input.
  roll_mean[match(dat, dat_val_padded$dat)] 
}

# rolling_value <- function(input, dat, val, interval, by, window, valid_thresh, 
#                           flag.num = NULL) {
#   if (!is.null(by)) input <- input %>% group_by_(.dots = by)
#   ret.val <- 
#     input %>%
#     mutate_(rolled.value =
#               interp(~filled.rolling_mean(dat, val, interval, 
#                                           window, valid.thresh),
#                      dat = as.name(dat),
#                      val = as.name(val),
#                      interval = interval, 
#                      window = window, 
#                      valid.thresh = valid.thresh),
#             n.within =
#               interp(~n.within.window(dat, interval, window),
#                      dat = input[[dat]],
#                      interval = interval,
#                      window = window)) %>%
#     mutate(valid = n.within >= valid.thresh) %>% 
#     ungroup
#   if (!is.null(flag.num)) 
#     ret.val <- ret.val %>% mutate(flag = n.within %in% flag.num)
#   ret.val %>% select(-n.within)
# }
# globalVariables("n.within")

#'Find the daily maximum of a value by some factor
#'
#'Given a dataframe with one value column, find the maximum value by some date.
#' @importFrom  stats na.omit
#' @param  input Dataframe
#' @param  dat character the column containing dates
#' @param  val character the value column
#' @param  by  character the columns to group by
#' @param  thresh numeric. 
#' @param  n.readings.min numeric. 
# '@return dataframe with filled in dates
daily_max <- function(input, dat = "date", val = "value", 
                      by = c("ems_id", "site"), 
                      thresh = 63, n.readings.min = 18) {
  input %>%
    group_by_(.dots = c(by, dat)) %>%
    summarise_(nReadings = interp(~length(na.omit(value)), 
                                  value = as.name(val)), 
               max       = interp(~max(value, na.rm = TRUE), 
                                  value = as.name(val)), 
               exceed    = interp(~max > thresh, thresh = thresh), 
               valid     = interp(~nReadings >= n.readings.min, 
                                  n.readings.min = n.readings.min)) %>%  
    mutate(flag = exceed & !valid) # Flag for data incomplete, but used
}
globalVariables(c("exceed", "valid"))

#'Find the maximum, return NA if no non-NA values.
#'
#'max typically returns -Inf (and a warning) if no non-NA values; this wrapper avoids that.
#' @importFrom  stats na.omit
#' @param  x numeric
# '@return The maximum value of x, with NAs removed. If no non-NA values, return NA.
max_na <- function(x) 
  if (length(na.omit(x)) == 0) NA else max(x, na.rm = TRUE)

#'Find the mean, return NA if no non-NA values.
#'
#'mean typically returns a warning if no non-NA values; this wrapper avoids that.
#' @importFrom  stats na.omit
#' @param  x numeric
# '@return The maximum value of x, with NAs removed. If no non-NA values, return NA.
mean_na <- function(x) 
  if (length(na.omit(x)) == 0) NA else mean(x, na.rm = TRUE)
