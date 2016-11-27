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

#'Calculate the 3-year rolling average of the ozone reading, following CAAQS data completeness 
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
#'@return data frame with the 3-year rolling averages

# The method here is correct, but it won't provide measurements for 
# invalid and not flagged years, even though those measurements might be valid.
o3_three_yr_avg <- function(data, dt = "date", val = "max8hr", by = NULL) {
  if (!is.null(by)) data <- data %>% group_by_(.dots = by)
  data %>% 
    filter(valid_year | flag_year_based_on_incomplete_data) %>% 
    rolling.value(dat = dt,
                  val = val,
                  interval = 1,
                  by = by,
                  window = 3,
                  valid.thresh = 2,
                  flag.num = 2) %>% 
    rename(ozone_metric = rolled.value,
           flag_two_of_three_years = flag)
}
globalVariables(c("valid_year", "flag_year_based_on_incomplete_data", "rolled.value", "flag"))
