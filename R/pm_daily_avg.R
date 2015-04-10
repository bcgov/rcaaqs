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

#'Calcualte the daily average PM2.5 reading, following CAAQS data completeness 
#'rules
#'
#'@param data data frame with date and value
#'@param dt the name (as a character string) of the date-time column. Default
#'  \code{"date_time"}
#'@param val the name (as a character string) of the PM2.5 value column. Default
#'  \code{"value"}
#'@param by character vector of grouping variables in data, probably an id if
#'  using multiple sites. Even if not using multiple sites, you shoud specfify
#'  the id column so that it is retained in the output.
#'@import dplyr
#'@import lazyeval
#'@export
#'@return data frame with the daily averages, can be input into 
#'  \code{\link{pm_98_percentile}}
#'@seealso \code{\link{pm_98_percentile}}
#'  
pm_daily_avg <- function(data, dt = "date_time", val = "value", by = NULL) {
  ## if dt is a datetime column, convert to date
  if (inherits(data[[dt]], "POSIXt")) {
    data[[dt]] <- as.Date(data[[dt]], tz = "Etc/GMT+8")
  }
  
  # Capture grouping variables
  by <- c(by, dt)
  
  ## Capture the formulas for the summaries
  readings_formula <- interp(~length(na.omit(x)), x = as.name(val))
  avg_formula <- interp(~ifelse(n_readings >= 18, 
                                round(mean(x, na.rm = TRUE), 1),
                                NA_real_),
                        x = as.name(val))
  
  year_formula <- interp(~get_year_from_date(x), x = as.name(dt))
  
  res <- group_by_(data, .dots = by)
  res <- summarise_(res, 
                    n_readings = readings_formula,
                    avg_24hr = avg_formula)
  res <- mutate_(res, year = year_formula)
  ret <- ungroup(res)
  names(ret)[names(ret) == dt] <- "date"
  ret
}

## This should work but lazy(date) does weird stuff. See 
## https://github.com/hadley/lazyeval/issues/18
# PM_avg_daily <- function(data, datecol, val) {
#   datecol <- lazy(datecol)
#   group_formula <- interp(~as.Date(x), x = datecol)
#   
#   val <- lazy(val)
#   readings_formula <- interp(~length(na.omit(x)), x = val)
#   avg_formula <- interp(~ifelse(n_readings >= 18, mean(x, na.rm = TRUE), 
#                                 NA_real_), 
#                         x = val)
#   
#   res <- group_by_(data, group_formula)
#     ret <- summarise_(res, n_readings = readings_formula,  
#                avg_24hr = avg_formula)
# }
# PM_avg_daily(data, date, value)
