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

#'Calculate the rolling ozone reading, following CAAQS data completeness 
#'rules
#'
#'@param data data frame with date and value
#'@param dt the name (as a character string) of the date-time column. Default
#'  \code{"date_time"}
#'@param val the name (as a character string) of the ozone value column. Default
#'  \code{"value"}
#'@param by character vector of grouping variables in data, probably an id if
#'  using multiple sites. Even if not using multiple sites, you shoud specify
#'  the id column so that it is retained in the output.
#'@import dplyr
#'@import lazyeval
#'@export
#'@return data frame with the daily maximum, can be input into 
#'  \code{\link{o3_ann_4th_highest}}
#'@seealso \code{\link{o3_ann_4th_highest}}
#'  
# 
# # Clobbers any date column if there was one.
# o3_daily_max <- function(data, dt = "date_time", val = "rolling8", by = NULL) {
#   if (!is.null(by)) data <- data %>% group_by_(.dots = by)
#   o3_standard <- 63
#   data %>% 
#     mutate_(date = interp(~as.Date(date_time), date_time = as.name(dt))) %>% 
#     daily.max(dat = "date", 
#               val = val, 
#               by = by, 
#               thresh = o3_standard, 
#               n.readings.min = 18) %>% 
#     rename(max8hr = max,
#            valid_max8hr = valid,
#            flag_max8hr_incomplete = flag)
# }
# globalVariables("flag")
