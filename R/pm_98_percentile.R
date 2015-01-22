#' Calculate the annual 98th percentile of daily average PM2.5 values according 
#' to CAAQS standards
#' 
#' Also computes the data completeness criteria (optional). Designed to be used
#' with the output from \code{\link{pm_avg_daily}}.
#' @import dplyr
#' @import lazyeval
#' @param  data data frame
#' @param  datecol the name of the "date" column (as a character string)
#' @param  valcol the name of the column with daily average PM2.5 values
#' @param  ... grouping variables in data, probably an id if using multiple 
#'   sites. Even if not using multiple sites, you shoud specfify the id column 
#'   so that it is retained in the output.
#' @param  std the value of the PM2.5 standard (default 28). Must be named as it
#'   comes after grouping variables (...)
#' @param completeness Should the completeness criteria be calculated (default 
#'   TRUE)
#' @param  year_valid  The percentage of valid days required in a year (default 
#'   75). Must be named as it comes after grouping variables (...). Only 
#'   required if calculating the completeness criteria.
#' @param  q_valid  The percentage of valid days required in each quarter 
#'   (default 60). Must be named as it comes after grouping variables (...). 
#'   Only required if calculating the completeness criteria.
#' @export
#' @seealso \code{\link{pm_avg_daily}}
#' @return  A data frame with 98th percentiles of daily averages, per year
#' @examples \dontrun{
#' 
#' }
pm_98_percentile <- function(data, datecol, valcol, ..., std = 28, 
                             completeness = TRUE, year_valid = 75, q_valid = 60) {
  data <- data[!is.na(data[[valcol]]),]
  
  if (!inherits(data[[datecol]], "Date")) {
    time_interval <- find_time_int(data[[datecol]])
    if (!grepl("86400", time_interval)) stop("Time interval of date column can't be less than one day")
  }
      
  data$year <- as.integer(as.POSIXlt(data[[datecol]])$year + 1900)
  
  dots <- list(..., "year")
  
  ans <- group_by_(data, .dots = dots)
  ans <- transmute_(ans, n_days       = ~n(),
                    rep_date          = interp(~x, x = as.name(datecol)),
                    ann_98_percentile = interp(~x, x = as.name(valcol)),
                    exceed            = ~ann_98_percentile > std)
  ans <- arrange_(ans, ~desc(ann_98_percentile))
  ans <- slice_(ans, ~cut_rank(n_days[1]))
  ans <- ungroup(ans)
  
  if (completeness) {
    comp <- pm_data_complete(data = data, datecol = datecol, valcol = valcol, 
                             ..., year_valid = year_valid, q_valid = q_valid)
    comp <- ungroup(comp)
    comp <- comp[,-which(names(comp) == "n_days")]
    
    ans <- merge(ans, comp, by = unlist(dots))
    ans$use_but_incomplete <- ans[["exceed"]] & !ans[["quarters_valid"]]
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
cut_rank <- function(n) {
  if (!inherits(n, c("integer", "numeric"))) stop("n is not an integer/numeric")
  if (n < 1) stop("n is less than 1")
  if (n > 366) stop("n is greater than 366")
  
  cuts <- c(1,50,100,150,200,250,300,350,366)
  ret <- cut(n, cuts, include.lowest = TRUE, labels = 1:8, right = TRUE)
  as.numeric(ret)
}
