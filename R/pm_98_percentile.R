#' Calculate the annual 98th percentile of daily average PM2.5 values according 
#' to CAAQS standards
#' 
#' Also computes the data completeness criteria (optional). Designed to be used
#' with the output from \code{\link{pm_daily_avg}}.
#' @import dplyr
#' @import lazyeval
#' @param  data data frame
#' @param  datecol the name of the "date" column (as a character string)
#' @param  valcol the name of the column with daily average PM2.5 values
#' @param  by character vector of grouping variables in data, probably an id if using multiple 
#'   sites. Even if not using multiple sites, you shoud specfify the id column 
#'   so that it is retained in the output.
#' @param  std the value of the PM2.5 standard (default 28).
#' @param completeness Should the completeness criteria be calculated (default 
#'   TRUE)
#' @param  year_valid  The percentage of valid days required in a year (default 
#'   75). Only required if calculating the completeness criteria.
#' @param  q_valid  The percentage of valid days required in each quarter 
#'   (default 60). Only required if calculating the completeness criteria.
#' @export
#' @seealso \code{\link{pm_daily_avg}}
#' @return  A data frame with 98th percentiles of daily averages, per year

pm_98_percentile <- function(data, datecol, valcol, by = NULL, std = 28, 
                             completeness = TRUE, year_valid = 75, q_valid = 60) {
  data <- data[!is.na(data[[valcol]]), ]
  
  if (!inherits(data[[datecol]], "Date")) {
    time_interval <- find_time_int(data[[datecol]])
    if (!grepl("86400", time_interval)) stop("Time interval of date column can't be less than one day")
  }
      
  data$year <- as.integer(as.POSIXlt(data[[datecol]], tz = "Etc/GMT+8")$year + 1900)
  
  by <- c(by, "year")
  
  ans <- group_by_(data, .dots = by)
  ans <- transmute_(ans, 
                    n_days       = ~ n(),
                    rep_date          = datecol,
                    ann_98_percentile = valcol,
                    exceed            = ~ ann_98_percentile > std)
  ans <- arrange_(ans, ~desc(ann_98_percentile))
  ans <- slice_(ans, ~n_tile(n_days[1], 0.98))
  ans <- ungroup(ans)
  
  if (completeness) {
    comp <- pm_data_complete(data = data, datecol = datecol, valcol = valcol, 
                             by = by, year_valid = year_valid, q_valid = q_valid)
    comp <- ungroup(comp)
    comp <- select_(comp, ~ -n_days)
    
    ans <- merge(ans, comp, by = by)
    ans <- mutate_(ans, 
                   use_but_incomplete = ~ exceed & annual_valid & !quarters_valid, 
                   use = ~ (annual_valid & quarters_valid) | use_but_incomplete)
  }
  
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
n_tile <- function(n, tile = 0.98) {
  if (!inherits(n, c("integer", "numeric"))) stop("n is not an integer/numeric")
  if (n < 1) stop("n is less than 1")
  if (n > 366) stop("n is greater than 366")
  if (!(tile <= 1 && tile >=0)) stop("tile should be between 0 and 1")

  i.d <- tile * n
  i <- floor(i.d)
  
  ret <- n - i
  as.integer(ret)
}
