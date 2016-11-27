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
rolling.sum <- function(x, width) {
  x <- ifelse(is.na(x), 0, x)
  cumsum(x) - lag(cumsum(x), width, default = 0)
}

rolling.mean <- function(x, width = 8, thresh = 0){
  available <- rolling.sum(!is.na(x), width = width)
  sum <- rolling.sum(x, width = width)
  ifelse(available >= thresh, sum/available, NA)
}

#'number within a rolling window and step size (for dates or numeric)
#'
#' @import tidyr
#' @param x the date, date_time, or numeric value
#' @param interval some numeric step size. for date-times, in seconds.
#' @param window some numeric look-back window.
#' @return vector with all input dates and values, interpolated with NAs.
n.within.window <- function(x, interval, window){
  full.x <- full_seq(x, interval)
  rolling.sum(full.x %in% x, width = window)[match(x,full.x)]
}

# Check for bad intervals? (Duplicated dates. etc.)
filled.rolling.mean <- function(dat, val, interval = 3600, 
                                width = 8, thresh = 6, digits = 1) {
  dat.val.padded <- pad.date.time(dat, val, interval)
  roll.mean <- rolling.mean(dat.val.padded$val, width, thresh) %>% round(digits)
  # Ensure order and length is same as input.
  roll.mean[match(dat, dat.val.padded$dat)] 
}

rolling.value <- function(input, dat, val, interval, by, window, valid.thresh, 
                          flag.num = NULL) {
  if (!is.null(by)) input <- input %>% group_by_(.dots = by)
  ret.val <- 
    input %>%
    mutate_(rolled.value =
              interp(~filled.rolling.mean(dat, val, interval, 
                                          window, valid.thresh),
                     dat = as.name(dat),
                     val = as.name(val),
                     interval = interval, 
                     window = window, 
                     valid.thresh = valid.thresh),
            n.within =
              interp(~n.within.window(dat, interval, window),
                     dat = as.name(dat),
                     interval = interval,
                     window = window)) %>%
    mutate(valid = n.within >= valid.thresh) %>% 
    ungroup
  if (!is.null(flag.num)) 
    ret.val <- ret.val %>% mutate(flag = n.within %in% flag.num)
  ret.val %>% select(-n.within)
}
globalVariables("n.within")

#'Find the daily maximum of a value by some factor
#'
#'Given a dataframe with one value column, find the maximum value by some date.
#' @param  input Dataframe
#' @param  dat character the column containing dates
#' @param  val character the value column
#' @param  by  character the columns to group by
#' @param  thresh numeric. 
#' @param  n.readings.min numeric. 
# '@return dataframe with filled in dates
daily.max <- function(input, dat = "date", val = "value", 
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
