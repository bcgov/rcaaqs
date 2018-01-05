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

rcaaqs_env <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  assign("olson_names", OlsonNames(), envir = rcaaqs_env)
}

#' Find time interval of a vector of regularly spaced dates/datetimes
#'
#' @param x vector of dates/datetimes
#'
#' @return character
test_time_interval <- function(x) {
  len <- min(100, length(x))
  test_x <- sort(x[1:len])
  m_diff <- Mode(diff(as.numeric(test_x)))
  if (inherits(x, "Date")) m_diff <- m_diff * 3600 * 24
  m_diff
}


#' Find the mode of a vector of numbers
#'
#' @param x vector of numbers
#'
#' @return vector of length 1
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Round up from 5 (drop-in replacement for base::round)
#' 
#' CAAQS methods requires always rounding up numbers ending in 5, 
#' which is different than the default round, which uses the 'round to even'
#' rule. See ?round
#'
#' @param x vector of numbers to round
#' @param digits integer indicating the number of decimal places to round to (default 0)
#'
#' @return numeric vector
#' @export
round_caaqs <- function(x, digits = 0) {
  ## Scale x so we are rounding to a whole number
  scale <- 10^digits
  scaled_x <- x * scale
  
  ## Add (or subtract) 0.5 to the scaled x, truncate it to the nearest 
  ## whole number, then get back to original magnitude by dividing by scale
  trunc(scaled_x + sign(x) * 0.5) / scale
}

check_vars <- function(vars, data, name_data = "data") {
  if(!is.data.frame(data)) stop("'", deparse(substitute(data)), "' is not a data frame", call. = FALSE)
  for(var in vars) {
    if(!is.character(var) && !is.null(var)) stop("'", var, "' is not a character vector", call. = FALSE)
    if(!(var %in% names(data)) && !is.null(var)) stop("'", var, "' is not a column in ", name_data, call. = FALSE)
  }
}

check_mode <- function(var, df, c) {
  if(mode(df[[var]]) != c) stop("Column '", var, "' is not ", c, call. = FALSE)
}

#' Defunct functions in rcaaqs
#' 
#' These functions have been removed from rcaaqs.
#' 
#' \itemize{
#'  \item \code{\link{format_date}}: This function is defunct. Use \code{\link{format_caaqs_dt}} instead.
#' }
#' 
#' @name rcaaqs-deprecated
NULL


