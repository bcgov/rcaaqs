#'Fill gaps in a date sequence
#'
#'Given a dataframe with one column as a date sequence, fill gaps in the dat sequence.
#' @param df Dataframe
#' @param  date_col the column containing dates
#' @param  interval The interval in the date sequence. If \code{NULL}, calculated automatically.
#' @param  fill_cols Columns to fill with the value in the column (should be columns where value is same in every row, such as an ID.)
#' @export
#' @return dataframe with filled in dates
#' @examples \dontrun{
#'
#'}
date_fill <- function (df, date_col, interval = NULL, fill_cols = NULL) {
  
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
  
  out
}
