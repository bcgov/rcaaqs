#'Calcualte the daily average PM2.5 reading, following CAAQS data completeness
#'rules
#'
#'@param data data frame with date and value
#'@param datecol the name (as a character string) of the date column
#'@param valcol the name (as a character string) of the PM2.5 value column
#'@param by character vector of grouping variables in data, probably an id if using multiple sites.
#'  Even if not using multiple sites, you shoud specfify the id column so that
#'  it is retained in the output.
#'@import dplyr
#'@import lazyeval
#'@export
#'@return data frame with the daily averages, can be input into 
#'        \code{\link{pm_98_percentile}}
#'@seealso \code{\link{pm_98_percentile}}
#'@examples \dontrun{
#'
#'}
pm_daily_avg <- function(data, datecol, valcol, by = NULL) {
  ## if datecol is a datetime column, convert to date
  if (inherits(data[[datecol]], "POSIXt")) {
    data[[datecol]] <- as.Date(data[[datecol]], tz = "Etc/GMT+8")
  }
  
  # Capture grouping variables
  by <- c(by, datecol)
  
  get_year_from_date <- function(date) {
    as.integer(as.POSIXlt(date, tz = "Etc/GMT+8")$year + 1900)
  }
  
  ## Capture the formulas for the summaries
  readings_formula <- interp(~length(na.omit(x)), x = as.name(valcol))
  avg_formula <- interp(~ifelse(n_readings >= 18, 
                                round(mean(x, na.rm = TRUE), 1),
                                NA_real_),
                        x = as.name(valcol))
  
  year_formula <- interp(~get_year_from_date(x), x = as.name(datecol))
  
  res <- group_by_(data, .dots = by)
  res <- summarise_(res, n_readings = readings_formula,
                    avg_24hr        = avg_formula)
  res <- mutate_(res, year = year_formula)
  ret <- ungroup(res)
  ret
}

## This should work but lazy(date) does weird stuff. See 
## https://github.com/hadley/lazyeval/issues/18
# PM_avg_daily <- function(data, datecol, valcol) {
#   datecol <- lazy(datecol)
#   group_formula <- interp(~as.Date(x), x = datecol)
#   
#   valcol <- lazy(valcol)
#   readings_formula <- interp(~length(na.omit(x)), x = valcol)
#   avg_formula <- interp(~ifelse(n_readings >= 18, mean(x, na.rm = TRUE), 
#                                 NA_real_), 
#                         x = valcol)
#   
#   res <- group_by_(data, group_formula)
#     ret <- summarise_(res, n_readings = readings_formula,  
#                avg_24hr = avg_formula)
# }
# PM_avg_daily(data, date, value)
