# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.


#' Compute the rolling sum with different windows
#'
#' @param x the input numeric value
#' @param width The maximum number of values used to compute the sum.
#' 
#' @return a vector y where y[i] = sum(x[i:i-1]) removing NAs, and ignoring
#'   non-positive indices.
#'   
#' @noRd

rolling_sum <- function(x, width) {
  x <- ifelse(is.na(x), 0, x)
  cumsum(x) - dplyr::lag(cumsum(x), width, default = 0)
}

#' Compute the rolling mean with different windows and thresholds.
#'
#' @param x the input numeric value
#' @param width The maximum number of values used to compute the mean.
#' @param thresh If at least thresh values are not used in computing the mean,
#'   then the output will be set to NA.
#' 
#' @return A rolling mean (see [rcaaqs::rolling_sum()]), except if not
#'   at least thresh values, it returns NA.
#'   
#' @noRd

rolling_mean <- function(x, width = 8, thresh = 0) {
  available <- rolling_sum(!is.na(x), width = width)
  sum <- rolling_sum(x, width = width)
  ifelse(available >= thresh, sum/available, NA)
}

#' Number within a rolling window and step size (for dates or numeric)
#'
#' @param x the date, date_time, or numeric value
#' @param interval some numeric step size. for date-times, in seconds.
#' @param window some numeric look-back window in `interval` units.
#' 
#' @return vector of same length as x, that returns, for each x, the number 
#'   behind x that were within `window*interval` units, including x itself.
#' 
#' @examples
#' rcaaqs:::n_within_window(c(1,2,7,8), interval = 1, window = 2)
#' # c(1,2,1,2)
#' 
#' @noRd

n_within_window <- function(x, window) {
  rolling_sum(!is.na(x), width = window)
}

#' Calculate a rolling mean, but fill in missing date values with NA.
#'
#' @param dat the input date or date-time value
#' @param val some numeric value to be interpolated with NAs.
#' @param interval some numeric step size. for date-times, in seconds.
#' @param width to calculate the mean, look back from current date 
#' to width*interval behind.
#' @param valid_thresh If there are less than width values available, then the
#'   mean will still be calculated if there are at least valid_thresh available.
#'   Set to equal width if this behaviour is not desired.
#' @param digits how many digits should the value be rounded to?
#' 
#' @return vector with all input dates and values, interpolated with NAs.
#' 
#' @noRd

# NOT CURRENTLY USED - Data filled with NAs in earlier step
# so don't need to fill with this function
filled_rolling_mean <- function(dat, val, interval = 3600, 
                                width = 8, valid_thresh = 6, digits = 1) {
  dat_val_padded <- pad_date_time(dat, val, interval)
  roll_mean <- round_caaqs(rolling_mean(
    dat_val_padded$val, width, valid_thresh), digits)
  # Ensure order and length is same as input.
  roll_mean[match(dat, dat_val_padded$dat)] 
}

#' Find the maximum, return NA if no non-NA values.
#'
#' `max()` typically returns -Inf (and a warning) if no non-NA values; this
#' wrapper avoids that.
#' 
#' @param  x numeric
#' 
#' @return The maximum value of x, with NAs removed. If no non-NA values, return
#'   NA.
#'   
#' @noRd

max_na <- function(x) {
  if (length(stats::na.omit(x)) == 0) NA else max(x, na.rm = TRUE)
}

#' Find the mean, return NA if no non-NA values.
#'
#' `mean()` typically returns a warning if no non-NA values; this wrapper
#' avoids that.
#' 
#' @param  x numeric
#' 
#' @return The maximum value of x, with NAs removed. If no non-NA values, return
#'   NA.
#'   
#' @noRd

mean_na <- function(x) {
  if (length(stats::na.omit(x)) == 0) NA else mean(x, na.rm = TRUE)
}

#' Find the quantile, return NA if no non-NA values.
#' 
#' Wraps [quantile2()] but returns NA if no non-NA values
#' 
#' @param x numeric vector whose sample quantiles are wanted.
#' @param probs numeric vector of probablities with values in \eqn{[0,1]}. 
#'   Default `0.98`
#' @param na.rm logical; if true, any `NA` and `NaN`'s are removed 
#'   from `x` before the quantiles are computed. Default `FALSE`
#' @param names logical; if true, the result has a names attribute. Set to FALSE
#'   for speedup with many probs. Default `FALSE`
#' @param type `"caaqs"` (default) or an integer between 1 and 9 selecting 
#'   one of the nine base quantile algorithms be used. See 
#'   [stats::quantile()] for details
#'   
#' @return A vector of `length(probs)`; if `names = TRUE`, it has a
#'   `names` attribute
#'   
#' @seealso [quantile2()]
#' 
#' @noRd

quantile2_na <- function(x, probs = 0.98, na.rm = TRUE, names = FALSE, type = "caaqs") {
  if (length(stats::na.omit(x)) == 0) {
    NA
  } else {
    quantile2(x, probs = probs, na.rm = na.rm, names = names, type = type)
  }
}
