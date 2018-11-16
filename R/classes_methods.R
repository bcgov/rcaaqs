as.caaqs <- function(x, param) {
  param <- match.arg(param, c("pm_annual", "pm_24h", "o3", "so2_1yr", 
                                    "so2_3yr", "no2_1yr", "no2_3yr"))
  structure(x, class = c(param, "caaqs", class(x)))
}

summary.caaqs <- function(x) {
  cat("CAAQS results for", class(x)[1], "\n")
  cat("--------------------------------------\n")
  cat(length(x), "elements:\n")
  for (n in names(x)) {
    cat("      *", n, "(", nrow(x[[n]]), "rows x", ncol(x[[n]]), "columns )\n")
  }
  cat("Access them directly using `$` notation or use the extract_ functions (see ?extract)\n")
}

print.caaqs <- summary.caaqs

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
#' - pm_24h, pm_annual: Daily average
#' - o3, so2_3yr, no2_3yr: Daily maximum
#' - so2_1yr, no2_1yr: None
#' 
#' `extract_yearly()`:
#' - pm_24h: Annual 98th percentile daily average
#' - pm_annual: Annual average of daily averages
#' - o3: Annual 4th highest daily maximum
#' - so2_1yr, no2_1yr: Annual average of hourly values
#' - so2_3yr: Annual 99th percentile or daily maximums
#' - no2_3yr: Annual 98th percentile or daily maximums
#' 
#' `extract_three_yr_rolling()`
#'  - pm_24h, pm_annual, o3, so2_3yr, no2_3yr: Three-year rolling average of the 
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
extract_caaqs.caaqs <- function(x) x$caaqs

## PM 24hr -------------------------------------------------
#' @export
extract_daily.pm_24h <- function(x) x$daily_avg

#' @export
extract_yearly.pm_24h <- function(x) x$yearly_98

#' @export
extract_three_yr_rolling.pm_24h <- function(x) x$three_yr_rolling

## PM annual -------------------------------------------------
#' @export
extract_daily.pm_annual <- function(x) x$daily_avg

#' @export
extract_yearly.pm_annual <- function(x) x$yearly_avg

#' @export
extract_three_yr_rolling.pm_annual <- function(x) x$three_yr_rolling

## Ozone -------------------------------------------------
#' @export
extract_daily.o3 <- function(x) x$daily_max

#' @export
extract_yearly.o3 <- function(x) x$ann_4th_highest

#' @export
extract_three_yr_rolling.o3 <- function(x)x$three_yr_rolling

## SO2 1yr -------------------------------------------------
#' @export
extract_yearly.so2_1yr <- function(x) x$yearly_hr

## SO2 3yr -------------------------------------------------
#' @export
extract_daily.so2_3yr <- function(x) x$daily_max

#' @export
extract_yearly.so2_3yr <- function(x) x$yearly_99

#' @export
extract_three_yr_rolling.so2_3yr <- function(x) x$three_yr_rolling

## NO2 1yr -------------------------------------------------
#' @export
extract_yearly.no2_1yr <- function(x) x$yearly_hr

## NO2 3yr -------------------------------------------------
#' @export
extract_daily.no2_3yr <- function(x) x$daily_max

#' @export
extract_yearly.no2_3yr <- function(x) x$yearly_98

#' @export
extract_three_yr_rolling.no2_3yr <- function(x) x$three_yr_rolling
