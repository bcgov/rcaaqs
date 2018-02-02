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


#' Given exclude rows from input dataset based on supplied data.frame which
#' specifies specific dates or date ranges.
#'
#' @param  data Dataframe
#' @param  dt character the column containing date-times
#' @param  by  an optional character vector specifying the columns to join on.
#' @param  exclusion_df  the data.frame that has all the columns in the by 
#'   parameter, in addition exactly one or two date columns.
#' @param  exclusion_date_cols a character vector with exactly one or two date
#'   columns.
#' 
#' @return dataframe with the specified dates or date ranges excluded.
#' 
#' @noRd

exclude_data <- function(data, dt, by, exclusion_df, exclusion_date_cols, val) {
  if (!all(by %in% names(exclusion_df))) 
    stop(paste0(setdiff(by, names(exclusion_df)), " not found in exclusion data"))
  if (!all(by %in% names(data))) 
    stop(paste0(setdiff(by, names(data)), " not found in sample data"))
  if(missing(dt))
    stop("dt not specified")
  if(length(dt) != 1)
    stop("dt must be a character vector of length one specifying the column name")
  
  if(!lubridate::is.POSIXt(data[[dt]]) & !lubridate::is.Date(data[[dt]]))
    stop(paste0(dt, " is not a date or time column"))
  
  if(missing(exclusion_date_cols) || 
     !(length(exclusion_date_cols) %in% c(1,2))) 
    stop("Length of exclusion_date_cols must be one or two")
  
  if(!all(exclusion_date_cols %in% names(exclusion_df))) {
    stop("exclusion_date_cols must be column names in exclusion data")
  }
  
  if(!is.Date(exclusion_df[[exclusion_date_cols[1]]])) {
    stop("Can only exclude whole days (not specific hours)")
  }
  
  if(length(exclusion_date_cols) == 1) {
    exclusion_date_col <- exclusion_date_cols
    # If date_time exclusions, but date data, ambiguity results.
    if(lubridate::is.POSIXt(exclusion_df[[exclusion_date_col]]) & 
       lubridate::is.Date(data[[dt]])) 
      stop("Exclusion column is a time, but the data column is a date")
    # If date exclusions, but date_time data, need to convert
    # Need to find a new name for the column.
    dt_new_set <- FALSE
    if(lubridate::is.Date(exclusion_df[[exclusion_date_col]]) & 
       lubridate::is.POSIXt(data[[dt]])) {
      dt_new <- find_new_name("date", names(data))
      data[[dt_new]] <- time_to_date(data[[dt]])
      dt_new_set <- TRUE
    }
    by_vector <- c(by, stats::setNames(exclusion_date_col, 
                                if (dt_new_set) dt_new else dt))
    
    exclusion_df$exclude <- TRUE
    
    ret_val <- dplyr::left_join(data, exclusion_df, by = by_vector)
    ret_val <- dplyr::mutate(ret_val, 
                             exclude = replace(exclude, is.na(exclude), FALSE))
    ret_val[[val]][ret_val$exclude] <- NA
    if(dt_new_set) ret_val[[dt_new]] <- NULL
    
  } else { # Must be a date range
    
    names(exclusion_df)[names(exclusion_df) == exclusion_date_cols[1]] <- "start"
    names(exclusion_df)[names(exclusion_df) == exclusion_date_cols[2]] <- "end"

    start <- exclusion_df$start
    end <- exclusion_df$end
    
    if (!((lubridate::is.Date(start) & lubridate::is.Date(end)) | 
          (lubridate::is.POSIXt(start) & lubridate::is.POSIXt(end)))) {
      stop("Exclusion data frame date/time columns must be both dates or times")
    }
    # start, dt
    if(lubridate::is.POSIXt(start) & lubridate::is.Date(data[[dt]])) {
      stop("Exclusion columns are date-times, but data columns are dates")
    }
    
    if(lubridate::is.POSIXt(start) & lubridate::tz(start) != lubridate::tz(data[[dt]])) {
      stop("Exclusion time columns are in a different timezone from the data")
    }
    
    # Get dates in data if dt is times and exclusion is by date
    rmv <- c("start", "end")
    if(lubridate::is.Date(start) & lubridate::is.POSIXt(data[[dt]])) {
      dt_new <- find_new_name("date", names(data))
      data[[dt_new]] <- time_to_date(data[[dt]])
      dt <- dt_new
      rmv <- c(rmv, dt)
    }
    
    ret_val <- dplyr::left_join(data, exclusion_df, by = by)
    ret_val <- dplyr::mutate(ret_val, 
                             exclude = !(is.na(.data$start) |
                                           is.na(.data$end) |
                                           .data[[dt]] < .data$start |
                                           .data[[dt]] > .data$end))
    ret_val <- dplyr::select(ret_val, - dplyr::one_of(rmv))
    ret_val[[val]][ret_val$exclude] <- NA
  }
  return(ret_val)
}

find_new_name <- function(desired, existing_names) {
  possible <- c(desired, paste(desired, seq_along(existing_names), sep = "_"))
  utils::head(possible[!(possible %in% existing_names)], 1)
}
