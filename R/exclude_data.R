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
#' @param  exclude_df  the data.frame that has all the columns in the by 
#'   parameter, in addition exactly one or two date columns.
#' @param  exclude_df_dt a character vector with exactly one or two date
#'   columns.
#' 
#' @return dataframe with the specified dates or date ranges excluded.
#' 
#' @noRd

exclude_data <- function(data, dt, by, 
                         exclude_df, exclude_df_dt, 
                         val, quiet = FALSE) {

  if(!quiet) message("  Excluding data...")
  
  exclude_df <- dplyr::ungroup(exclude_df)
  
  if(!is.null(by)) {
    exclude_df <- dplyr::mutate_at(exclude_df, by, as.character)
  }
  
  
  if(length(exclude_df_dt) == 1) {
    exclusion_date_col <- exclude_df_dt
    # If date_time exclusions, but date data, ambiguity results.
    if(lubridate::is.POSIXt(exclude_df[[exclusion_date_col]]) & 
       lubridate::is.Date(data[[dt]])) {
      stop("Exclusion column is a time, but the data column is a date")
    }
    # If date exclusions, but date_time data, need to convert
    # Need to find a new name for the column.
    dt_new_set <- FALSE
    if(lubridate::is.Date(exclude_df[[exclusion_date_col]]) & 
       lubridate::is.POSIXt(data[[dt]])) {
      dt_new <- find_new_name("date", names(data))
      data[[dt_new]] <- time_to_date(data[[dt]])
      dt_new_set <- TRUE
    }
    by_vector <- c(by, stats::setNames(exclusion_date_col, 
                                if (dt_new_set) dt_new else dt))
    
    exclude_df$excluded <- TRUE
    
    ret_val <- dplyr::left_join(data, exclude_df, by = by_vector)
    ret_val <- dplyr::mutate(ret_val, 
                             excluded = replace(.data$excluded, 
                                                is.na(.data$excluded), FALSE))
    ret_val[[val]][ret_val$excluded] <- NA

    if(dt_new_set) ret_val <- dplyr::select(ret_val, - !!rlang::sym(dt_new))
    
  } else { # Must be a date range
    
    names(exclude_df)[names(exclude_df) == exclude_df_dt[1]] <- "start"
    names(exclude_df)[names(exclude_df) == exclude_df_dt[2]] <- "end"

    start <- exclude_df$start
    end <- exclude_df$end
    
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
    
    ret_val <- dplyr::left_join(data, exclude_df, by = by)
    ret_val <- dplyr::mutate(ret_val, 
                             excluded = !(is.na(.data$start) |
                                            is.na(.data$end) |
                                            .data[[dt]] < .data$start |
                                            .data[[dt]] > .data$end))
    ret_val <- dplyr::select(ret_val, -dplyr::one_of(rmv))
    ret_val[[val]][ret_val$excluded] <- NA
  }
  return(ret_val)
}

find_new_name <- function(desired, existing_names) {
  possible <- c(desired, paste(desired, seq_along(existing_names), sep = "_"))
  utils::head(possible[!(possible %in% existing_names)], 1)
}
