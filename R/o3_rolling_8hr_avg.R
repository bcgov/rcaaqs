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

#'Calculate the rolling 8 hour ozone reading, following CAAQS data completeness 
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
#'@return data frame with the hourly rolling averages, can be input into 
#'  \code{\link{o3_daily_max}}
#'@seealso \code{\link{o3_daily_max}}
#'  

o3_rolling_8hr_avg <- function(data, dt = "date_time", val = "value", by = NULL){
  if (!is.null(by)) data <- data %>% group_by_(.dots = by)
  rolling.value(data, 
                dat = dt, 
                val = val, 
                by = by, 
                interval = 3600,
                window = 8, 
                valid.thresh = 6) %>% 
    rename(rolling8 = rolled.value, flag_valid_8hr = valid)
}
globalVariables("rolled.value")
