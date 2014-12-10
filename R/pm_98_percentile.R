#' Calculate the annual 98th percentile of daily average PM2.5 values according to CAAQS standards
#' 
#' Designed to be used with the output from \code{\link{pm_avg_daily}}
#' @import dplyr
#' @import lazyeval
#' @param  data data frame
#' @param  yearcol the name of the "year" column (as a character string)
#' @param  valcol the name of the column with daily average PM2.5 values
#' @param  ... grouping variables, probably an id if using multiple sites
#' @export
#' @seealso \code{\link{pm_avg_daily}}
#' @return  A data frame with 98th percentiles of daily averages, per year
#' @examples \dontrun{
#' 
#'}
pm_98_percentile <- function(data, datecol, valcol, ...) {
  data <- data[!is.na(data[[valcol]]),]
  
  data$year <- as.POSIXlt(data[[datecol]])$year + 1900
  
  dots <- list(..., "year")
  
  arrange_formula <- interp(~desc(x), x = as.name(valcol))
  
  cut_rank <- function(n) {
    cuts <- c(1,50,100,150,200,250,300,350,366)
    ret <- cut(n, cuts, include.lowest = TRUE, labels = 1:8, right = TRUE)
    as.numeric(ret)
  }
  
  ans <- group_by_(data, .dots = dots) %>%
    mutate_(n_days = ~n(),
      percent_valid_annual = , 
      percent_valid_q1 = , 
      percent_valid_q2 = , 
      percent_valid_q3 = , 
      percent_valid_q4 = ) %>%
    arrange_(arrange_formula) %>%
    slice(cut_rank(n()))
  
  ans
  
}


#' Given a vector of dates (in a single year), calculate the percentage of days in a quarter
#' 
#' @param  date a vecvtor of dates
#' @param  tz the timezone the dates are in. Default Etc/GMT-8 with no Daylight savings
#' @param  q the quarter of interest
#' @export
#' @return  A percentage of days in the specified quarter that are in the supplied vector
#' @examples \dontrun{
#' 
#'}
percent_valid_days <- function(dates, tz = "Etc/GMT-8", q = c("year","Q1","Q2","Q3","Q4")) {
  q = match.arg(q)
  
  dates <- na.omit(dates)
  
  year <- as.POSIXlt(dates[1])$year + 1900
  
  q_lengths <- c(year = -difftime(paste0(year, "-01-01"), paste0(year, "-12-31"), 
                                  tz = tz, units = "days"), 
                 Q1 = -difftime(paste0(year, "-01-01"), paste0(year, "-03-31"), 
                                tz = tz, units = "days"), 
                 Q2 = -difftime(paste0(year, "-04-01"), paste0(year, "-06-30"), 
                                tz = tz, units = "days"), 
                 Q3 = -difftime(paste0(year, "-07-01"), paste0(year, "-09-30"), 
                                tz = tz, units = "days"), 
                 Q4 = -difftime(paste0(year, "-10-01"), paste0(year, "-12-31"), 
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

