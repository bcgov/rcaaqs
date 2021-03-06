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

rcaaqs_env <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  assign("olson_names", OlsonNames(), envir = rcaaqs_env)
}

#' Find time interval of a vector of regularly spaced dates/datetimes
#'
#' @param x vector of dates/datetimes
#'
#' @return character
#' @noRd
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
#' 
#' @noRd

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
#' @noRd

round_caaqs <- function(x, digits = 0) {
  ## Scale x so we are rounding to a whole number
  scale <- 10^digits
  scaled_x <- x * scale
  
  ## Add (or subtract) 0.5 to the scaled x, truncate it to the nearest 
  ## whole number, then get back to original magnitude by dividing by scale
  trunc(scaled_x + sign(x) * 0.5) / scale
}

check_vars <- function(vars, data) {
  if(!is.data.frame(data)) stop("'", deparse(substitute(data)), 
                                "' is not a data frame", call. = FALSE)
  for(var in vars) {
    if(!is.character(var) && !is.null(var)) stop("'", var, 
                                                 "' is not a character vector", 
                                                 call. = FALSE)
    if(!all(var %in% names(data)) && !is.null(var)) {
      stop("'", var, "' is not a column in '", 
           deparse(substitute(data)), "'", call. = FALSE)
    }
  }
}

check_one <- function(...) {
  for(a in list(...)) {
    if(length(a) > 1) stop("'", deparse(substitute(a)), 
                           "' takes only one argument", call. = FALSE)
  }
}

check_class <- function(var, data, c) {
  stp <- FALSE
  if (c == "POSIXct") {
    if(!lubridate::is.POSIXct(data[[var]])) stp <- TRUE
  } else if(c == "numeric") {
    if(!is.numeric(data[[var]])) stp <- TRUE
  } else {
    if(!any(class(data[[var]]) == c)) stp <- TRUE
  }
  if(stp) stop("Column '", var, "' is not ", c, call. = FALSE)
}

check_groups <- function(data, dt) {

  # Check for duplicates (considering grouping)
  dup <- dplyr::summarize(data, 
                          n = length(.data[[dt]]), 
                          n1 = length(unique(.data[[dt]])),
                          dup = .data$n != .data$n1)
  if(any(dup$dup)) {
    msg <- paste0("Duplicate values in '", dt, "'.")
    if(is.null(by)) msg <- paste0(msg, " Consider using 'by' to specify grouping argument(s).")
    if(!is.null(by)) msg <- paste0(msg, " Consider adding additional grouping variables to 'by'.")
    stop(msg, call. = FALSE)
  }
}

# Confirm that exclude_df has correct structure
check_exclude <- function(data, dt, by, exclude_df, exclude_df_dt) {

  if (!all(by %in% names(exclude_df))) {
    stop(paste0(paste0("'", setdiff(by, names(exclude_df)), "'", collapse = ", "),
                       " not found in exclusion data"), 
         call. = FALSE)
  }
  if (!all(by %in% names(data))) {
    stop(paste0(paste0("'", setdiff(by, names(data)), "'", collapse = ", "),
                " not found in sample data"),
         .call = FALSE)
  }
  if(missing(dt)) stop("dt not specified")
  if(length(dt) != 1) {
    stop("dt must be a character vector of length one specifying the column name",
         call. = FALSE)
  }
  
  if(!lubridate::is.POSIXt(data[[dt]]) & !lubridate::is.Date(data[[dt]])) {
    stop(paste0(dt, " is not a date or time column"),
         call. = FALSE)
  }
  
  if(missing(exclude_df_dt) || 
     !(length(exclude_df_dt) %in% c(1,2))) {
    stop("Length of exclude_df_dt must be one or two",
         call. = FALSE)
  }
  
  if(!all(exclude_df_dt %in% names(exclude_df))) {
    stop("exclude_df_dt must be column names in exclusion data",
         call. = FALSE)
  }
  
  if(!lubridate::is.Date(exclude_df[[exclude_df_dt[1]]])) {
    stop("Can only exclude whole days (not specific hours)",
         call. = FALSE)
  }
}

#' Defunct functions in rcaaqs
#' 
#' These functions have been removed from rcaaqs.
#' 
#' \itemize{
#'  \item [format_date()]: This function is defunct. Use [format_caaqs_dt()] instead.
#' }
#' 
#' @name rcaaqs-deprecated
NULL

# Function to check which version of tidyr, as syntax for nest() changed in v1.0
# https://tidyr.tidyverse.org/articles/in-packages.html
tidyr_new_interface <- function() {
  utils::packageVersion("tidyr") > "0.8.99"
}
