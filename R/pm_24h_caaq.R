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

#'Calculates the three year average for the PM2.5 24 hour metric
#'
#'Flags if based on only two years.
#'
#'@import dplyr
#'@param  data data frame containing annual 98th percentile values.
#'@param  year name of the column containing the year. Default \code{"year"}
#'@param  val name of the column containing annual 98th percentile values.
#'  Default \code{"ann_98_percentile"}
#'@param  by character vector of grouping variables in data, probably an id if
#'  using multiple sites. Even if not using multiple sites, you shoud specfify
#'  the id column so that it is retained in the output.
#'@param  cyear The year to calculate the metric for (this will be the latest of
#'  the three years used in the calculation). Can be "latest" to use the latest 
#'  year found in the data frame, or an integer denoting the year.
#'@export
#'@return a data frame, with year, the value, and the number of years the value 
#'  is based on

pm_24h_caaq <- function(data, year = "year", val = "ann_98_percentile", 
                         by = NULL, cyear = "latest") {
  vars <- c(year, val, by)
  
  for (var in vars) {
    if (!var %in% names(data)) stop(var, " is not a column in data")
  }
  
  if (inherits(cyear, c("integer", "numeric"))) {
    if (!cyear %in% data[[year]]) stop(cyear, " does not exist in data")
  } else if (cyear == "latest") {
    cyear <- max(data[[year]])
  } else {
    stop("cyear must be an integer or 'latest'")
  }
  
  if (!inherits(data[[val]], "numeric")) stop(val, "is not numeric")
  
  years <- seq(to = cyear, length.out = 3)
  
  rows <- data[data[[year]] %in% years & !is.na(data[[val]]), , drop = FALSE]
  
  if (is.null(by)) {
    if (any(duplicated(rows[[year]]))) stop("duplicate values in ", year, 
                                               " but no grouping variable(s) specified")
  } else {
    rows <- group_by_(rows, .dots = by)
  }
  
  caaq_formula <- interp(~ifelse(n_years >= 2, round_caaqs(mean(x), 0), NA_real_), 
                         x = as.name(val))
  
  ret <- summarise_(rows, 
                    caaq_year    = cyear,
                    min_year     = interp(~min(x), x = as.name(year)),
                    max_year     = interp(~max(x), x = as.name(year)),
                    n_years      = ~n(),
                    metric = ~"pm2.5_24h", 
                    metric_value = caaq_formula, 
                    caaqs = ~cut_achievement(metric_value, "pm2.5_24h"), 
                    mgmt = ~cut_management(metric_value, "pm2.5_24h"))
  
  # Hack to get ordered levels back in the data frame if group_by was used:
  if (!is.ordered(ret$caaqs)) {
    ret$caaqs <- ordered(ret$caaqs, levels = levels(cut_achievement(1, "pm2.5_24h")))
  }
  
  if (!is.ordered(ret$mgmt)) {
    ret$mgmt <- ordered(ret$mgmt, levels = levels(cut_management(1, "pm2.5_24h")))
  }
  
  ret
  
}
