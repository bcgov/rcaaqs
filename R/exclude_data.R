#'Given exclude rows from input dataset based on supplied data.frame which specifies specific dates or date ranges.
#'
#' @importFrom  lubridate is.POSIXt is.Date
#' @importFrom  stats setNames
#' @param  data Dataframe
#' @param  dt character the column containing date-times
#' @param  by  an optional character vector specifying the columns to join on.
#' @param  exclusion_df  the data.frame that has all the columns in the 
#' by parameter, in addition exactly one or two date columns.
#' @param  exclusion_date_cols a character vector with exactly one or two date columns.
# '@return dataframe with the specified dates or date ranges excluded.
exclude_data <- function(data, dt, by, exclusion_df, exclusion_date_cols) {
  if (!all(by %in% names(exclusion_df))) 
    stop(paste0(setdiff(by, names(exclusion_df)), " not found in exclusion data"))
  if (!all(by %in% names(data))) 
    stop(paste0(setdiff(by, names(data)), " not found in sample data"))
  if(missing(dt))
    stop("dt not specified")
  if(length(dt) != 1)
    stop("dt must be a character vector of length one specifying the column name")
  
  if(!is.POSIXt(data[[dt]]) & !is.Date(data[[dt]]))
    stop(paste0(dt, " is not a date or time column"))
  
  if(missing(exclusion_date_cols) || 
     !(length(exclusion_date_cols) %in% c(1,2))) 
    stop("Length of exclusion_date_cols must be one or two")
  
  if(length(exclusion_date_cols) == 1) {
    exclusion_date_col <- exclusion_date_cols
    # If date_time exclusions, but date data, ambiguity results.
    if(is.POSIXt(exclusion_df[[exclusion_date_col]]) & 
       is.Date(data[[dt]])) 
      stop("Exclusion column is a time, but the data column is a date")
    # If date exclusions, but date_time data, need to convert
    # Need to find a new name for the column.
    dt_new_set <- FALSE
    if(is.Date(exclusion_df[[exclusion_date_col]]) & 
       is.POSIXt(data[[dt]])) {
      dt_new <- find_new_name("date", names(data))
      data[[dt_new]] <- time_to_date(data[[dt]])
      dt_new_set <- TRUE
    }
    by_vector <- c(by, setNames(exclusion_date_col, 
                                if (dt_new_set) dt_new else dt))
    ret_val <- anti_join(data, exclusion_df, by = by_vector)
    if(dt_new_set) ret_val[[dt_new]] <- NULL
    
  } else { # Must be a date range
    start_name <- exclusion_date_cols[1]
    end_name <- exclusion_date_cols[2]
    start <- exclusion_df[[start_name]]
    end <- exclusion_df[[end_name]]
    if (!((is.Date(start) & is.Date(end)) | 
          (is.POSIXt(start) & is.POSIXt(end))))
      stop("Exclusion data frame date/time columns must be both dates or times")
    # start, dt
    if(is.POSIXt(start) & is.Date(data[[dt]]))
      stop("Exclusion columns are date-times, but data columns are dates")
    row_number_name <- find_new_name("row_number", names(data))
    data[[row_number_name]] <- seq.int(nrow(data))
    joined_all <- left_join(data, exclusion_df, by = by)
    filtered <- joined_all[
      is.na(joined_all[[start_name]]) | is.na(joined_all[[end_name]]) |
        joined_all[[dt]] < joined_all[[start_name]] |
        joined_all[[dt]] > joined_all[[end_name]],]
    filtered <- filtered[!(names(filtered) %in% c(start_name, end_name))]
    unique_rows <- unique(filtered) # Without row_number, this would remove duplicate rows.
    unique_rows[[row_number_name]] <- NULL
    ret_val <- unique_rows
  }
  return(ret_val)
}

#' @importFrom  utils head
find_new_name <- function(desired, existing_names) {
  possible <- c(desired, paste(desired, seq_along(existing_names), sep = "_"))
  head(possible[!(possible %in% existing_names)], 1)
}
