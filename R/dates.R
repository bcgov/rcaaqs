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



#' Format date-times in raw air quality data.
#' 
#' Intended for use with hourly air quality readings. Ensures that the
#' date/times are in class POSIXct in the correct timezone, and ensures that the
#' timestamp is associated with the previous hour as dictated in caaqs guidance
#' manual.
#'
#' @param  x Vector of date-times as character or POSIXlt/ct.
#' @param format The format of the character dates
#' @param prev_hour Should the timestamp be assigned to the previous hour as
#'   dictated by the CAAQS guidance document? Default `TRUE`. This is
#'   accomplished by subtracting one second off the times.
#' @param tz The timezone of the date-times. See Details below.
#' 
#' @details You can set the timezone that you are working with in two different
#'   ways. You can set it globally with: `options("rcaaqs.timezone" =
#'   "your_timezone")` or set it each time you call the function by setting the
#'   `tz` argument to "your_timezone" where "your_timezone" is a valid 
#'   timezone - see [OlsonNames()]. The default (when
#'   `options("rcaaqs.timezone")` is unset) is `"Etc/GMT+8"`, which is
#'   equivalent to Pacific Standard Time, but does not use Daylight Savings 
#'   (i.e., is GMT+8 all year). This is the standard for Air Quality Monitoring
#'   in British Columbia.
#'
#' @return POSIXct vector
#'
#' @export

format_caaqs_dt <- function(x, format="%Y-%m-%d %H:%M:%S", prev_hour = TRUE,
                            tz = getOption("rcaaqs.timezone", default = "Etc/GMT+8")) {
  
  if (!tz %in% rcaaqs_env$olson_names) stop("Invalid timezone. See OlsonNames()", call. = FALSE)
  
  if (is.character(x))
    x <- lubridate::fast_strptime(x, format = format, tz = tz, lt = FALSE)
  else if (lubridate::is.POSIXt(x)) {
    
    if (lubridate::tz(x) != tz) {
      lubridate::tz(x) <- tz
    }
    
    if (lubridate::is.POSIXlt(x)) {
      x <- as.POSIXct(x)
    }
    
  } else {
    stop("x should be a character string in the format described by the 'format' argument, or POSIXt", call. = FALSE)
  }
  
  if (prev_hour) {
    x <- x - 1
  }
  x
  
}

#' Fill gaps in a date sequence
#'
#' Given a dataframe with one column as a date sequence, fill gaps in the date 
#' sequence.
#' 
#' @param  data Dataframe
#' @param  date_col The column containing dates
#' @param  interval The interval in the date sequence. If `NULL`, calculated
#'  automatically.
#' @param  fill_cols Columns to fill with the value in the column (should be
#'  columns where value is same in every row, such as an ID.)
#' @param  add_ymd logical. Should the date be split into year, month, and day
#'  columns and added to the output?
#'  
#' @return Dataframe with filled in dates
#' 
#' @examples \dontrun{
#' foo <- data.frame(Date = seq(as.Date("2008-01-01"), as.Date("2008-12-01"), by = "month"), 
#'                   val = round(rnorm(12, 5, 2), 1), label = rep("a", 12))
#' bar <- foo[-c(2,5,6,7,10),]
#' date_fill(bar, "Date", interval = "1 month", fill_cols = "label")
#' }
#' 
#' @export

date_fill <- function(data, date_col, interval = NULL, fill_cols = NULL, add_ymd = FALSE) {
  
  if (!is.null(interval) && 
        (!is.numeric(interval) && 
           !grep("sec|min|hour|day|DSTday|week|month|quarter|year", interval))) {
    stop("Specified interval is not valid. See ?seq.Date and ", 
         "?seq.POSIXt for help, or let the function find it for you")
  }
  
  data <- data[order(data[[date_col]]), ]
  
  dates <- data[[date_col]]
  
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
    out <- merge(data, full_dates, by.x = date_col, by.y = "date", all = TRUE)
    
    if (!is.null(fill_cols)) {
      for (col in fill_cols) {
        fill_name <- out[1, col]
        out[,col] <- fill_name
      }
    }
    
  } else {
    out <- data
  }
  
  if (add_ymd) out <- add_ymd(out, "date")
  
  out
}

#' Add a year, month, and day column to a dataframe based on a date column
#'
#' @param data dataframe
#' @param datecol the column in data that contains the dates as a character string
#' @param outnames the names for the new columns in order of year, month, day 
#'   (defaults to "year", "month", "day")
#' @param tz the timezone
#' 
#' @return a dataframe with the new columns added
#' 
#' @noRd

add_ymd <- function(data, datecol, tz = getOption("rcaaqs.timezone", default = "Etc/GMT+8"), 
                    outnames = NULL) {
  if (is.null(outnames)) outnames <- c("year", "month", "day")
  int <- intersect(outnames, names(data))
  if (length(int) > 0) {
    ques <- function(int) {
      ans <- tolower(readline(paste("Column(s)", paste0(int, collapse = ", "), 
                             "exist(s) in dataframe. Overwrite? (y/n) ")))
      if (ans %in% c("y", "n")) return(ans) else ques(int)
    }
    
    overwrite <- ques(int)
    if (tolower(overwrite) == "n") stop("Exiting function on user request")
  }

  dateslt <- as.POSIXlt(data[, datecol], tz = tz)
  data[,outnames[1]] <- dateslt$year + 1900
  data[,outnames[2]] <- dateslt$mon + 1
  data[,outnames[3]] <- dateslt$mday
  data
}

#' Calculate number of days in quarter
#'
#' @param  quarter the quarter of the year (1-4)
#' @param year the year as a numeric
#' 
#' @return vector with the number of days in that quarter year combination.
#' 
#' @noRd

days_in_quarter <- function(quarter, year) 
  c(90,91,92,92)[quarter] + (lubridate::leap_year(year) & quarter == 1)

#' Calculate number of days in year
#'
#' @param year the year as a numeric
#' 
#' @return vector with the number of days in that year.
#' 
#' @noRd
days_in_year <- function(year) {
  365 + lubridate::leap_year(year)
}

#' Interpolate a sequence of dates and values with NA when dates are missing
#'
#' @param dat the date, date_time, or numeric value
#' @param val some value to be interpolated
#' @param interval some numeric step size. for date-times, in seconds
#' 
#' @return data.frame with all input dates and values, interpolated with NAs.
#' 
#' @noRd

pad_date_time <- function(dat, val, interval) {
  all_dates <- tidyr::full_seq(dat, interval)
  data.frame(dat = all_dates, 
             val = val[match(all_dates, dat)])
}

# Simply using as.Date on a time converts based on UTC.
# This conversion just truncates the time part.
# THIS IS EQUIVALENT TO, BUT FASTER THAN, lubridate::as_date
time_to_date <- function(date_time) {
  as.Date(date_time, attr(date_time, "tzone"))
}

#' Calculate number (or proportion) of valid days in each year and quarter
#'
#' @param data the input data.frame
#' @param date the date
#' @param by the grouping variable
#' @param units either "prop" if you want proportion of valid days or "days" is
#'   absolute number of days is required.
#'   
#' @return data.frame with valid days in each year and quarter
#' 
#' @noRd

valid_by_quarter <- function(data, date, by, val, units = c("prop", "days")) {
  data <- dplyr::ungroup(data)
  data <- dplyr::mutate(data,
                        year = lubridate::year(!!rlang::sym(date)),
                        quarter = lubridate::quarter(!!rlang::sym(date)))

  data <- dplyr::group_by(data, !!!rlang::syms(c(by, "year", "quarter")))
  data <- dplyr::summarise(data, days = length(.data$year[!is.na(.data[[val]])]))
  data <- dplyr::ungroup(data)
  
  # Cheap way to fill in quarters
  all_q <- do.call(rbind, replicate(4, unique(data[c(by, "year")]), simplify = FALSE))
  all_q$quarter <- rep(1:4, each = nrow(all_q)/4)
  data <- dplyr::left_join(all_q, data, by = c(by, "year", "quarter"))
  data$days <- ifelse(is.na(data$days), 0, data$days)
  data$valid_in_quarter <- data$days
  
  if(units == "prop") {
    data$valid_in_quarter <- data$valid_in_quarter / days_in_quarter(data$quarter, data$year)
  }
  data <- dplyr::group_by(data, !!!rlang::syms(c(by, "year")))
  
  data <- dplyr::mutate(data, valid_in_year = sum(.data$days))
  
  if(units == "prop") {
    data$valid_in_year <- data$valid_in_year / days_in_year(data$year)
  }
  data <- dplyr::arrange(data, !!!rlang::syms(c(by, "year", "quarter")))
  
  # Format to wide columns
  data <- dplyr::group_by(data, !!!rlang::syms(c("year", by)))
  data <- dplyr::select(data, dplyr::all_of(c(by, "year", "quarter", "valid_in_quarter", "valid_in_year")))
  data <- tidyr::spread(data, "quarter", "valid_in_quarter", sep = "_")
  
  dplyr::ungroup(data)
}

