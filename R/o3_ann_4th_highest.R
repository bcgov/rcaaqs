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

#'Calculate the 4th highest annual ozone reading, following CAAQS data completeness 
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
#'@return data frame with the fourth highest maximum daily reading, can be input into 
#'  \code{\link{o3_three_yr_avg}}
#'@seealso \code{\link{o3_three_yr_avg}}
#'  

o3_ann_4th_highest <- function(data, dt = "date", val = "max8hr", by = NULL) {
  if (!is.null(by)) data <- data %>% group_by_(.dots = by)
  o3_standard <- 63
  quarter_validity <-
    data %>% 
    filter(valid_max8hr | flag_max8hr_incomplete) %>% 
    valid_by_quarter(dt, by)
  
  year_validity <- 
    quarter_validity %>% 
    filter(quarter %in% 2:3) %>% 
    mutate(valid_quarter = valid_in_quarter >= 0.75) %>% 
    group_by_(.dots = c(by, "year")) %>% 
    summarise(valid_year = all(valid_quarter)) %>% 
    ungroup
  
  data %>%
    filter(valid_max8hr | flag_max8hr_incomplete) %>%
    mutate_(year = interp(~get_year_from_date(date), date = as.name(dt))) %>% 
    group_by_(.dots = c(by, "year")) %>% 
    filter_(interp(~row_number(desc(max8hr)) == 4, max8hr = as.name(val))) %>% 
    left_join(year_validity, by = c(by, "year")) %>% 
    mutate(valid_year = ifelse(is.na(valid_year), FALSE, valid_year)) %>% 
    mutate_(exceed = interp(~max8hr > o3_standard, max8hr = as.name(val))) %>% 
    mutate(flag_year_based_on_incomplete_data = exceed & !valid_year) %>% 
    ungroup  
}
globalVariables(c("valid_max8hr", "flag_max8hr_incomplete", "quarter", "valid_in_quarter", "valid_quarter", "valid_year"))
