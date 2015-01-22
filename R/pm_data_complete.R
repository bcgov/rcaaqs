#' Calculates the percentage of days with readings annually, as well as for each
#' quarter for each year in a dataset.
#' 
#' Designed to be used with the output from \code{\link{pm_avg_daily}}
#' @import dplyr
#' @import lazyeval
#' @param  data data frame (likely the result of running \code{\link{pm_avg_daily}})
#' @param  datecol the name of the "date" column (as a character string)
#' @param  valcol the name of the column with daily average PM2.5 values
#' @param  ... grouping variables, probably an id if using multiple sites. Even 
#'             if not using multiple sites, you shoud specfify the id column so 
#'             that it is retained.
#' @param  year_valid  The percentage of valid days required in a year (default 75). 
#'                     Must be named as it comes after grouping variables (...)
#' @param  q_valid  The percentage of valid days required in each quarter (default 60). 
#'                  Must be named as it comes after grouping variables (...)
#' @export
#' @seealso \code{\link{pm_avg_daily}}
#' @return A data frame with percentage of days with readings annually, as well 
#'         as one for each quarter. Also includes whether or not the annual and 
#'         quarterly requirements are met
#' @examples \dontrun{
#' 
#'}
pm_data_complete <- function(data, datecol, valcol, ..., year_valid = 75, q_valid = 60) {
  data <- data[!is.na(data[[valcol]]),]
  
  if (!inherits(data[[datecol]], "Date")) {
    time_interval <- find_time_int(data[[datecol]])
    if (!grepl("86400", time_interval)) stop("Time interval of date column can't be less than one day")
  }
  
  data$year <- as.integer(as.POSIXlt(data[[datecol]])$year + 1900)
  
  dots <- list(..., "year")
  
  annual_formula <- interp(~percent_valid_days(x, q = "year"), 
                           x = as.name(datecol))
  q1_formula <- interp(~percent_valid_days(x, q = "Q1"), x = as.name(datecol))
  q2_formula <- interp(~percent_valid_days(x, q = "Q2"), x = as.name(datecol))
  q3_formula <- interp(~percent_valid_days(x, q = "Q3"), x = as.name(datecol))
  q4_formula <- interp(~percent_valid_days(x, q = "Q4"), x = as.name(datecol))
  
  data %>%
    group_by_(.dots = dots) %>%
    summarise_(n_days = ~n(),
               percent_valid_annual = annual_formula, 
               percent_valid_q1 = q1_formula, 
               percent_valid_q2 = q2_formula, 
               percent_valid_q3 = q3_formula, 
               percent_valid_q4 = q4_formula) %>%
    rowwise() %>%
    mutate(annual_valid = percent_valid_annual >= year_valid,
           quarters_valid = all(c(percent_valid_q1, percent_valid_q2, 
                                  percent_valid_q3, percent_valid_q4) >= q_valid))
}

#' Given a vector of dates (in a single year), calculate the percentage of days in a quarter
#' 
#' @param  dates a vector of dates
#' @param  q the time period of interest, one of: "year","Q1","Q2","Q3","Q4"
#' @param  tz the timezone the dates are in. Default Etc/GMT-8 with no Daylight savings
#' @export
#' @return  A percentage of days in the specified quarter that are in the supplied vector
#' @examples \dontrun{
#' 
#'}
percent_valid_days <- function(dates, q = c("year","Q1","Q2","Q3","Q4"), tz = "Etc/GMT-8") {
  
  if (!inherits(dates, "Date")) {
    time_interval <- find_time_int(dates)
    if (!grepl("86400", time_interval)) stop("Time interval of date column can't be less than one day")
  }
  
  if (!tz %in% OlsonNames()) stop(tz, " is not a valid timezone.")
  
  q = match.arg(q)
  
  dates <- na.omit(dates)
  
  if (any(duplicated(dates))) {
    dates <- unique(dates)
    warning("There were duplicate dates detected. Duplicates were discarded and calculation proceeded")
  }
  
  year <- as.POSIXlt(dates[1])$year + 1900
  
  q_lengths <- c(year = -difftime(paste0(year, "-01-01"), paste0(year, "-12-31"), 
                                  tz = tz, units = "days"),
                 Q1   = -difftime(paste0(year, "-01-01"), paste0(year, "-03-31"), 
                                  tz = tz, units = "days"), 
                 Q2   = -difftime(paste0(year, "-04-01"), paste0(year, "-06-30"), 
                                  tz = tz, units = "days"), 
                 Q3   = -difftime(paste0(year, "-07-01"), paste0(year, "-09-30"), 
                                  tz = tz, units = "days"), 
                 Q4   = -difftime(paste0(year, "-10-01"), paste0(year, "-12-31"), 
                                  tz = tz, units = "days")) + 1
  
  q_length = q_lengths[q]
  
  if (q == "year") {
    q_dates <- dates
  } else {
    q_dates <- dates[quarters(dates) %in% q]
  }
  
  ret <- (length(q_dates) / q_length) * 100
  
  ret
}

