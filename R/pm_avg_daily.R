#' Calcualte the daily average PM2.5 reading, following CAAQS data completeness rules
#'
#' @param data data frame with date and value
#' @param  datecol the name (as a character string) of the date column
#' @param  valcol the name (as a character string) of the PM2.5 value column
#' @import dplyr
#' @import lazyeval
#' @export
#' @return data frame with the daily averages
#' @examples \dontrun{
#'
#'}
pm_avg_daily <- function(data, datecol, valcol) {
  ## if datecol is a datetime column, convert to date
  data$date <- as.Date(data[[datecol]])
  
  ## capture the formulas for the summaries
  readings_formula <- interp(~length(na.omit(x)), x = as.name(valcol))
  avg_formula <- interp(~ifelse(n_readings >= 18, mean(x, na.rm = TRUE),
                                NA_real_),
                        x = as.name(valcol))
  
  group_by(data, date) %>%
    summarise_(n_readings = readings_formula,
               avg_24hr = avg_formula)
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
#   group_by_(data, group_formula) %>%
#     summarise_(n_readings = readings_formula,  
#                avg_24hr = avg_formula)
# }
# PM_avg_daily(data, date, value)
