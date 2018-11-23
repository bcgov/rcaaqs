# Copyright 2018 Province of British Columbia
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

as.caaqs <- function(x, param, dt, val, by) {
  param <- match.arg(param, names(params()))
  structure(x, class = unique(c(param, "caaqs", class(x))),
            vars = list(dt = dt, val = val, by = by), 
            by_vals = unique(x$caaqs[, by]))
}

get_by_vals <- function(x) attr(x, "by_vals")
get_by <- function(x) attr(x, "vars")[["by"]]
get_val <- function(x) attr(x, "vars")[["val"]]
get_dt <- function(x) attr(x, "vars")[["dt"]]
get_param <- function(x) intersect(class(x), names(params()))

params <- function() c("pm2.5_annual" = "PM2.5 annual", 
                       "pm2.5_24h" = "PM2.5 24h", 
                       "o3" = "Ozone", 
                       "so2_1yr" = "SO2 annual average", 
                       "so2_3yr" = "SO2 1 hr", 
                       "no2_1yr" = "NO2 annual average", 
                       "no2_3yr" = "NO2 1 hr" )

#' @export
print.caaqs <- function(x, ...) {
  print_summary(x, ...)
}

#' @export
summary.caaqs <- function(object, ...) {
  print_summary(object, ...)
}

print_summary <- function(x, ...) {
  cat("\nCAAQS results for", params()[get_param(x)], "\n")
  if (inherits(x, "caaqs_mgmt")) {
    cat("  * Includes CAAQS managment results\n")
    if (!is.null(get_eetf(x))) {
      cat("  * Exclusions have been made due to EETFs\n")
    }
  } else {
    cat("  * Includes CAAQS ambient results only\n")
  }
  cat("--------------------------------------\n")
  cat(length(x), "elements:\n")
  for (n in names(x)) {
    cat("      *", n, "(", nrow(x[[n]]), "rows x", ncol(x[[n]]), "columns )\n")
  }
  cat("Access them with the extract_ functions (see ?caaqs_extractors)\n\n")
}

# Generics + defaults -------------------------------------------------


#' Extract results from a caaqs analysis
#'
#' @param x An object of class `"caaqs"` (result of running a `*_caaqs()` function)
#'
#' @return Varies according to the function and the parameter being analyzed:
#' 
#' `extract_caaqs()`:
#' - caaqs results
#' 
#' `extract_daily()`:
#' - pm2.5_24h, pm2.5_annual: Daily average
#' - o3, so2_3yr, no2_3yr: Daily maximum
#' - so2_1yr, no2_1yr: None
#' 
#' `extract_yearly()`:
#' - pm2.5_24h: Annual 98th percentile daily average
#' - pm2.5_annual: Annual average of daily averages
#' - o3: Annual 4th highest daily maximum
#' - so2_1yr, no2_1yr: Annual average of hourly values
#' - so2_3yr: Annual 99th percentile or daily maximums
#' - no2_3yr: Annual 98th percentile or daily maximums
#' 
#' `extract_three_yr_rolling()`
#'  - pm2.5_24h, pm2.5_annual, o3, so2_3yr, no2_3yr: Three-year rolling average of the 
#'  yearly value.
#'  - so2_1yr, no2_1yr: None
#' 
#' @name caaqs_extractors
NULL
#> NULL

#' @rdname caaqs_extractors
#' @export
extract_caaqs <- function(x) UseMethod("extract_caaqs")

#' @export
extract_caaqs.default <- function(x) {
  stop("No method defined for object of type ", 
       paste(class(x), collapse = ", "), call. = FALSE)
}

#' @rdname caaqs_extractors
#' @export
extract_daily <- function(x) UseMethod("extract_daily")

extract_daily.default <- function(x) {
  stop("No method defined for object of type ", 
       paste(class(x), collapse = ", "), call. = FALSE)
}

#' @rdname caaqs_extractors
#' @export
extract_yearly <- function(x) UseMethod("extract_yearly")

extract_yearly.default <- function(x) {
  stop("No method defined for object of type ", 
       paste(class(x), collapse = ", "), call. = FALSE)
}

#' @rdname caaqs_extractors
#' @export
extract_three_yr_rolling <- function(x) UseMethod("extract_three_yr_rolling")

extract_three_yr_rolling.default <- function(x) {
  stop("No method defined for object of type ", 
       paste(class(x), collapse = ", "), call. = FALSE)
}

# Methods -------------------------------------------------

## caaqs (all) -------------------------------------------------
#' @export
extract_caaqs.caaqs <- function(x) x[["caaqs"]]

## PM 24hr -------------------------------------------------
#' @export
extract_daily.pm2.5_24h <- function(x) x[["daily_avg"]]

#' @export
extract_yearly.pm2.5_24h <- function(x) x[["yearly_98"]]

#' @export
extract_three_yr_rolling.pm2.5_24h <- function(x) x[["three_yr_rolling"]]

## PM annual -------------------------------------------------
#' @export
extract_daily.pm2.5_annual <- function(x) x[["daily_avg"]]

#' @export
extract_yearly.pm2.5_annual <- function(x) x[["yearly_avg"]]

#' @export
extract_three_yr_rolling.pm2.5_annual <- function(x) x[["three_yr_rolling"]]

## Ozone -------------------------------------------------
#' @export
extract_daily.o3 <- function(x) x[["daily_max"]]

#' @export
extract_yearly.o3 <- function(x) x[["ann_4th_highest"]]

#' @export
extract_three_yr_rolling.o3 <- function(x)x[["three_yr_rolling"]]

## SO2 1yr -------------------------------------------------
#' @export
extract_yearly.so2_1yr <- function(x) x[["yearly_hr"]]

## SO2 3yr -------------------------------------------------
#' @export
extract_daily.so2_3yr <- function(x) x[["daily_max"]]

#' @export
extract_yearly.so2_3yr <- function(x) x[["yearly_99"]]

#' @export
extract_three_yr_rolling.so2_3yr <- function(x) x[["three_yr_rolling"]]

## NO2 1yr -------------------------------------------------
#' @export
extract_yearly.no2_1yr <- function(x) x[["yearly_hr"]]

## NO2 3yr -------------------------------------------------
#' @export
extract_daily.no2_3yr <- function(x) x[["daily_max"]]

#' @export
extract_yearly.no2_3yr <- function(x) x[["yearly_98"]]

#' @export
extract_three_yr_rolling.no2_3yr <- function(x) x[["three_yr_rolling"]]
