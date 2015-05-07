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

#' Calculate the annual 98th percentile of daily average PM2.5 values according 
#' to CAAQS standards
#' 
#' Also computes the data completeness criteria (optional). Designed to be used 
#' with the output from \code{\link{pm_daily_avg}}.
#' @import dplyr
#' @import lazyeval
#' @param  data data frame
#' @param  date the name of the "date" column (as a character string). Default
#'   \code{"date"}
#' @param  val the name of the column with daily average PM2.5 values. Default
#'   \code{"avg_24h"}.
#' @param  by character vector of grouping variables in data, probably an id if 
#'   using multiple sites. Even if not using multiple sites, you shoud specfify 
#'   the id column so that it is retained in the output.
#' @param  std the value of the PM2.5 standard (default 28).
#' @param nr the column containing the number of readings per day (default
#'   \code{"n_readings"}).
#' @param  daily_valid  The minimum number of hourly readings in a day for the
#'   daily average to be considered valid (default \code{18}).
#' @param type type of 98th percentile calculation to use. Default "caaqs"
#' @export
#' @seealso \code{\link{pm_daily_avg}}, \code{\link{quantile2}}
#' @return  A data frame with 98th percentiles of daily averages, per year

pm_98_percentile <- function(data, date = "date", val = "avg_24h", nr = "n_readings", 
                             by = NULL, std = 28, daily_valid = 18, type = "caaqs") {
  data <- data[!is.na(data[[val]]) & data[[nr]] >= daily_valid, ]
  
  if (!inherits(data[[date]], "Date")) {
    if (test_time_interval(dates) != 86400) stop("Time interval of date column must be one day")
  }
      
  data$year <- get_year_from_date(data[[date]])
  
  by <- c(by, "year")
  
  ans <- group_by_(data, .dots = by)
  ans <- summarise_(ans, 
                    n_days = ~ n(),
                    ann_98_percentile = interp(~quantile2(x, type = y), 
                                               x = as.name(val), 
                                               y = type),
                    exceed = ~ ann_98_percentile > std)
  ans <- ungroup(ans)
  
  ans
  
}

#' Return the rank that should be used to determine the 98th percentile given a 
#' number of valid days
#' 
#' Wraps \code{\link[stats]{quantile}} but with different defaults, adds another
#' \code{type}, "caaqs", where the percentile (default 0.98) is calculated 
#' according to the caaqs methods.
#' @param x numeric vector whose sample quantiles are wanted.
#' @param probs numeric vector of probablities with values in \eqn{[0,1]}. 
#'   Default \code{0.98}
#' @param na.rm logical; if true, any \code{NA} and \code{NaN}'s are removed 
#'   from \code{x} before the quantiles are computed. Default \code{FALSE}
#' @param names logical; if true, the result has a names attribute. Set to FALSE
#'   for speedup with many probs. Default \code{FALSE}
#' @param type \code{"caaqs"} (default) or an integer between 1 and 9 selecting
#'   one of the nine base quantile algorithms be used. See
#'   \code{\link[stats]{quantile}} for details
#'   
#' @return A vector of \code{length(probs)}; if \code{names = TRUE}, it has a
#'   \code{names} attribute
#' @seealso \code{\link[stats]{quantile}}
#' @export
#' 
quantile2 <- function(x, probs = 0.98, na.rm = FALSE, names = FALSE, type = "caaqs") {
  if (missing(x) || length(x) == 0) {
    warning("No non-missing arguments to quantile2; returning -Inf")
    return(-Inf)
  }
  if (!inherits(x, c("integer", "numeric"))) stop("x is not numeric")
  if (!all(probs <= 1 & probs >= 0)) stop("probs should be between 0 and 1")
  if (!(type == "caaqs" || type %in% 1:9)) stop("type needs to be either 'caaqs' or 1:9")
  
  if (type == "caaqs") {
    if (na.rm) {
      x <- x[!is.na(x)]
    } else if (anyNA(x)) {
       stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
    }
    x <- sort(x, decreasing = TRUE)
    n <- length(x)
    i.d <- probs * n
    i <- floor(i.d)
    ret <- x[n - i]
    if (names) names(ret) <- paste0(probs * 100, "%")
  } else {
    ret <- quantile(x = x, probs = probs, na.rm = na.rm, names = names, type = type)
  }
  ret
}
