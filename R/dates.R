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

#'Convert character date string from Raw air data to POSIXct
#'
#' Likely data downloaded from DataBC
#' @importFrom lubridate fast_strptime
#' @param  dates vector of dates to convert as characters.
#' @param format the format of the character dates
#' @param lt return date object as POSIXlt. Default \code{FALSE}
#' @export
#' @return dataframe with filled in dates
format_date <- function(dates, format="%Y-%m-%d %H:%M:%S", lt = FALSE) {
  fast_strptime(dates, format = format, tz = "Etc/GMT+8", lt = lt)
}

#'Fill gaps in a date sequence
#'
#'Given a dataframe with one column as a date sequence, fill gaps in the dat
#'sequence.
#' @param  df Dataframe
#' @param  date_col the column containing dates
#' @param  interval The interval in the date sequence. If \code{NULL}, calculated
#'  automatically.
#' @param  fill_cols Columns to fill with the value in the column (should be
#'  columns where value is same in every row, such as an ID.)
#' @param  add_ymd logical. Should the date be split into year, month, and day
#'  columns and added to the output?
#' @export
# '@return dataframe with filled in dates
#' @examples \dontrun{
#' foo <- data.frame(Date = seq(as.Date("2008-01-01"), as.Date("2008-12-01"), by = "month"), 
#'                   val = round(rnorm(12, 5, 2), 1), label = rep("a", 12))
#' bar <- foo[-c(2,5,6,7,10),]
#' date_fill(bar, "Date", interval = "1 month", fill_cols = "label")
#'}
date_fill <- function(df, date_col, interval = NULL, fill_cols = NULL, add_ymd = FALSE) {
  
  if (!is.null(interval) && 
        (!is.numeric(interval) && 
           !grep("sec|min|hour|day|DSTday|week|month|quarter|year", interval))) {
    stop("Specified interval is not valid. See ?seq.Date and ", 
         "?seq.POSIXt for help, or let the function find it for you")
  }
  
  df <- df[order(df[[date_col]]), ]
  
  dates <- df[[date_col]]
  
  ## Function to check for non-uniform intervals
  diff_ints <- function(d) {
    ddiff <- as.numeric(diff(d))
    maxdiff <- abs(max(ddiff, na.rm = TRUE) - min(ddiff, na.rm = TRUE))
    maxdiff > 0 
  }
  
  if (diff_ints(dates)) {
    sdate <- min(dates, na.rm = TRUE)
    edate <- max(dates, na.rm = TRUE)
    
    if (is.null(interval)) {
      interval <- test_time_interval(dates)
    }
    
    full_dates <- data.frame(date = seq(sdate, edate, by = interval))
    out <- merge(df, full_dates, by.x = date_col, by.y = "date", all = TRUE)
    
    if (!is.null(fill_cols)) {
      for (col in fill_cols) {
        fill_name <- out[1, col]
        out[,col] <- fill_name
      }
    }
    
  } else {
    out <- df
  }
  
  if (add_ymd) out <- add_ymd(out, "date")
  
  out
}

#'Add a year, month, and day column to a dataframe based on a date column
#'
#' @param df dataframe
#' @param datecol the column in df that contains the dates as a character string
#' @param outnames the names for the new columns in order of year, month, day 
#'                 (defaults to "year", "month", "day")
#' @param tz the timezone
#' @export
#' @return a dataframe with the new columns added

add_ymd <- function(df, datecol, tz = "Etc/GMT+8", outnames = NULL) {
  if (is.null(outnames)) outnames <- c("year", "month", "day")
  int <- intersect(outnames, names(df))
  if (length(int) > 0) {
    ques <- function(int) {
      ans <- tolower(readline(paste("Column(s)", paste0(int, collapse = ", "), 
                             "exist(s) in dataframe. Overwrite? (y/n) ")))
      if (ans %in% c("y", "n")) return(ans) else ques(int)
    }
    
    overwrite <- ques(int)
    if (tolower(overwrite) == "n") stop("Exiting function on user request")
  }

  dateslt <- as.POSIXlt(df[, datecol], tz = tz)
  df[,outnames[1]] <- dateslt$year + 1900
  df[,outnames[2]] <- dateslt$mon + 1
  df[,outnames[3]] <- dateslt$mday
  df
}

#'Calcuate number of days in quarter
#'
#' @importFrom lubridate leap_year
#' @param  quarter the quarter of the year (1-4)
#' @param year the year as a numeric
#' @return vector with the number of days in that quarter year combination.
days_in_quarter <- function(quarter, year) 
  c(90,91,92,92)[quarter] + (leap_year(year) & quarter == 1)

#'Calcuate number of days in year
#'
#' @importFrom lubridate leap_year
#' @param year the year as a numeric
#' @return vector with the number of days in that year.
days_in_year <- function(year) 
  365 + lubridate::leap_year(year)

#'Interpolate a sequence of dates and values with NA when dates are missing.
#'
#' @importFrom tidyr full_seq
#' @param dat the date, date_time, or numeric value
#' @param val some value to be interpolated
#' @param interval some numeric step size. for date-times, in seconds.
#' @return data.frame with all input dates and values, interpolated with NAs.
pad_date_time <- function(dat, val, interval) {
  all_dates <- full_seq(dat, interval)
  data.frame(dat = all_dates, 
             val = val[match(all_dates, dat)])
}
get_year_from_date <- function(date) {
  as.integer(as.POSIXlt(date)$year + 1900)
}

get_month_from_date <- function(date) {
  as.integer(as.POSIXlt(date)$mon + 1)
}

get_day_from_date <- function(date) {
  as.integer(as.POSIXlt(date)$mday)
}

# Simply using as.Date on a time converts based on UTC.
# This conversion just truncates the time part.
time_to_date <- function(date_time) 
  as.Date(date_time, attr(date_time, "tzone"))

#'Calcuate number (or proportion) of valid days in each year and quarter
#'
#' @importFrom lubridate quarter
#' @importFrom tidyr spread_
#' @param data the input data.frame
#' @param date the date
#' @param by the grouping variable
#' @param units either "prop" if you want proportion of valid days or "days" is absolute number of days is required.
#' @return data.frame with valid days in each year and quarter
valid_by_quarter <- function(data, date, by, units = c("prop", "days")) {
  data <- mutate_(data, 
                  year    = interp(~get_year_from_date(date), date = as.name(date)), 
                  quarter = interp(~quarter(date), date = as.name(date)))
  data <- group_by_(data, .dots = c(by, "year", "quarter"))
  data <- summarise(data, days = n())
  data <- ungroup(data)
  
  # Cheap way to fill in quarters.  
  all_q = do.call(rbind, replicate(4, unique(data[c(by, "year")]), simplify = FALSE))
  all_q$quarter <- rep(1:4, each = nrow(all_q)/4)
  data <- left_join(all_q, data, by = c(by, "year", "quarter"))
  data$days <- ifelse(is.na(data$days), 0, data$days)
  data$valid_in_quarter <- data$days
  if(units == "prop") 
    data$valid_in_quarter <- data$valid_in_quarter / days_in_quarter(data$quarter, data$year)
  data <- group_by_(data, .dots = c(by, "year"))
  data <- mutate_(data, valid_in_year = ~sum(days))
  if(units == "prop") 
    data$valid_in_year <- data$valid_in_year / days_in_year(data$year)
  data <- arrange_(data, .dots = c(by, "year", "quarter"))
  
  # Format to wide columns.  
  data <- group_by_(data, .dots = c("year", by))
  data <- select_(data, .dots = c(by, "year", "quarter", "valid_in_quarter", "valid_in_year"))
  data <- spread_(data, "quarter", "valid_in_quarter", sep = "_")
  
  ungroup(data)
}

