#'Convert character date string from ENVISTA to POSIXct
#'
#' @param  dates vector of dates to convert
#' @export
#' @return dataframe with filled in dates
format_date <- function(dates) {
  as.POSIXct(dates, format = "%d%b%Y:%H:%M:%S", tz = "Etc/GMT-8")
}

#'Fill gaps in a date sequence
#'
#'Given a dataframe with one column as a date sequence, fill gaps in the dat sequence.
#' @param  df Dataframe
#' @param  date_col the column containing dates
#' @param  interval The interval in the date sequence. If \code{NULL}, calculated automatically.
#' @param  fill_cols Columns to fill with the value in the column (should be columns where value is same in every row, such as an ID.)
#' @param  add_ymd logical. Should the date be split into year, month, and day columns and added to the output?
#' @export
#' @return dataframe with filled in dates
#' @examples \dontrun{
#'
#'}
date_fill <- function (df, date_col, interval = NULL, fill_cols = NULL, add_ymd = FALSE) {
  
  if (!is.null(interval) && 
        (!is.numeric(interval) && 
           !grep("sec|min|hour|day|DSTday|week|month|quarter|year", interval))) {
    stop("Specified interval is not valid. See ?seq.Date and ", 
         "?seq.POSIXt for help, or let the function find it for you")
  }
  
  dates <- as.data.frame(df)[,date_col]
  
  start.date <- min(dates, na.rm = TRUE)
  end.date <- max(dates, na.rm = TRUE)
  
  if (is.null(interval)) {
    if (class(dates)[1] == "Date") {
      interval <- "day"
    } else {
      interval <- find_time_int(dates)
    }
  }
  
  if (length(unique(diff(dates))) != 1L) {
    all.dates <- data.frame(date = seq(start.date, end.date, 
                                       by = interval))
    out <- merge(df, all.dates, all = TRUE)
    
    if (!is.null(fill_cols)) {
      for (col in fill_cols) {
        fill_name <- out[1,col]
        out[,col] <- fill_name
      }
    }
    
  } else {
    out <- df
  }
  
  if (add_ymd) out <- add_ymd(out, "date")
  
  out
}

#'Find time interval in a date sequence
#'
#'Taken from package openair
#' @param dates vector of dates
#' @export
#' @return an integer reflecting the number of seconds in the time interval
#' @examples \dontrun{
#'
#'}
find_time_int <- function (dates) {
  dates <- unique(dates)
  len <- length(dates)
  len <- min(c(100, len))
  id <- which.max(table(diff(as.numeric(dates[order(dates[1:len])]))))
  seconds <- as.numeric(names(id))
  if ("POSIXt" %in% class(dates)) 
    seconds <- paste(seconds, "sec")
  if (class(dates)[1] == "Date") {
    seconds <- 3600 * 24
    seconds <- paste(seconds, "sec")
  }
  seconds
}

#'Add a year, month, and day column to a dataframe based on a date column
#'
#' @importFrom lubridate year month day floor_date
#' @param df dataframe
#' @param datecol the column in df that contains the dates as a character string
#' @param outnames the names for the new columns in order of year, month, day 
#'        (defaults to "year", "month", "day")
#' @export
#' @return a dataframe with the new columns added
#' @examples \dontrun{
#'
#'}
add_ymd <- function(df, datecol, outnames = NULL) {
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

  df[,outnames[1]] <- year(floor_date(df[,datecol], "year"))
  df[,outnames[2]] <- month(floor_date(df[,datecol], "month"))
  df[,outnames[3]] <- day(floor_date(df[,datecol], "day"))
  df
}
