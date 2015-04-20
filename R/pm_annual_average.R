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

#' Calculate the annual average pm2.5 concentration, optionally with
#' completeness criteria
#' 
#' @import dplyr
#' @import lazyeval
#' @param  data data frame
#' @param  date the name of the "date" column (as a character string)
#' @param  val the name of the column with daily average PM2.5 values
#' @param  by character vector of grouping variables in data, probably an id if
#'   using multiple sites. Even if not using multiple sites, you shoud specfify
#'   the id column so that it is retained in the output.
#' @param completeness Should the completeness criteria be calculated (default 
#'   TRUE)
#' @param  year_valid  The percentage of valid days required in a year (default 
#'   75). Only required if calculating the completeness criteria.
#' @param  q_valid  The percentage of valid days required in each quarter 
#'   (default 60). Only required if calculating the completeness criteria.
#'   
#' @return a data frame with annual average values per year
#' @seealso \code{\link{pm_daily_avg}}
#' @export

pm_annual_average <- function(data, date = "date", val = "avg_24hr", nr = "n_readings", by = NULL, 
                              completeness = TRUE, year_valid = 75, q_valid = 60) {
  data <- data[!is.na(data[[val]]) & data[[nr]] >= 18, ]
  
  if (!inherits(data[[date]], "Date")) {
    time_interval <- openair:::find.time.interval(data[[date]])
    if (!grepl("86400", time_interval)) stop("Time interval of date column can't be less than one day")
  }
  
  data$year <- get_year_from_date(data[[date]])
  
  by <- c(by, "year")
  
  avg_formula <- interp(~round(mean(x), 1), x = as.name(val))
  
  ans <- group_by_(data, .dots = by)
  ans <- summarise_(ans, 
                    n_days = ~ n(),
                    ann_avg = avg_formula)
  
  if (completeness) {
    comp <- pm_data_complete(data = data, date = date, val = val, 
                             by = by, year_valid = year_valid, q_valid = q_valid)
    comp <- ungroup(comp)
    comp <- select_(comp, ~ -n_days)
    
    ans <- merge(ans, comp, by = by)
    ans <- mutate_(ans, 
                   use = ~ (annual_valid & quarters_valid))
  }
  ans

}
