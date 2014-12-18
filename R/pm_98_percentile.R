#'Calculate the annual 98th percentile of daily average PM2.5 values according
#'to CAAQS standards
#'
#'Designed to be used with the output from \code{\link{pm_avg_daily}}
#'@import dplyr
#'@import lazyeval
#'@param  data data frame
#'@param  datecol the name of the "date" column (as a character string)
#'@param  valcol the name of the column with daily average PM2.5 values
#'@param  ... grouping variables in data, probably an id if using multiple
#'  sites. Even if not using multiple sites, you shoud specfify the id column so
#'  that it is retained in the output.
#'@param  std the value of the PM2.5 standard (default 28). Must be named as it 
#'  comes after grouping variables (...)
#'@export
#'@seealso \code{\link{pm_avg_daily}}
#'@return  A data frame with 98th percentiles of daily averages, per year
#' @examples \dontrun{
#' 
#'}
pm_98_percentile <- function(data, datecol, valcol, ..., std = 28) {
  data <- data[!is.na(data[[valcol]]),]
  
  if (!inherits(data[[datecol]], "Date")) {
    time_interval <- find_time_int(data[[datecol]])
    if (!grepl("86400", time_interval)) stop("Time interval of date column can't be less than one day")
  }
      
  data$year <- as.integer(as.POSIXlt(data[[datecol]])$year + 1900)
  
  dots <- list(..., "year")
  
  ans <- group_by_(data, .dots = dots) %>%
    transmute_(n_days            = ~n(),
               rep_date          = interp(~x, x = as.name(datecol)),
               ann_98_percentile = interp(~x, x = as.name(valcol)),
               exceed            = ~ann_98_percentile > std) %>%
    arrange(desc(ann_98_percentile)) %>%
    slice(cut_rank(n_days[1])) %>%
    ungroup()
  
  ans
  
}


#' Return the rank that should be used to determine the 98th percentile given a
#' number of valid days
#' 
#' @param n the number of valid days in a year
#' @return  a number from 1 to 8
#' @keywords internal
#' @examples \dontrun{
#' 
#'}
cut_rank <- function(n) {
  if (!inherits(n, c("integer", "numeric"))) stop("n is not an integer/numeric")
  if (n < 1) stop("n is less than 1")
  if (n > 366) stop("n is greater than 366")
  
  cuts <- c(1,50,100,150,200,250,300,350,366)
  ret <- cut(n, cuts, include.lowest = TRUE, labels = 1:8, right = TRUE)
  as.numeric(ret)
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
