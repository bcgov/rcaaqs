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
#' @param nr the column containing the number of readings per day (default
#'   \code{"n_readings"}).
#' @param  daily_valid  The minimum number of hourly readings in a day for the
#'   daily average to be considered valid (default \code{18}).
#'   
#' @return a data frame with annual average values per year
#' @seealso \code{\link{pm_daily_avg}}
#' @export

pm_annual_average <- function(data, date = "date", val = "avg_24h", nr = "n_readings", 
                              by = NULL, daily_valid = 18) {
  
  data <- data[!is.na(data[[val]]) & data[[nr]] >= 18, ]
  
  if (!inherits(data[[date]], "Date")) {
    if (test_time_interval(dates) != 86400) stop("Time interval of date column must be one day")
  }
  
  data$year <- get_year_from_date(data[[date]])
  
  by <- c(by, "year")
  
  avg_formula <- interp(~round(mean(x), 1), x = as.name(val))
  
  ans <- group_by_(data, .dots = by)
  ans <- summarise_(ans, 
                    n_days = ~ n(),
                    ann_avg = avg_formula)
  
  ans

}
