#' Calcualte the daily average PM2.5 reading, following CAAQS data completeness rules
#'
#' @param data data frame with date and value
#' @param datecol the name (as a character string) of the date column
#' @param valcol the name (as a character string) of the PM2.5 value column
#' @param ... grouping variables, probably an id if using multiple sites
#' @import dplyr
#' @import lazyeval
#' @importFrom lubridate year
#' @export
#' @return data frame with the daily averages, can be input into \code{\link{pm_98_percentile}}
#' @seealso \code{\link{pm_98_percentile}}
#' @examples \dontrun{
#'
#'}
pm_avg_daily <- function(data, datecol, valcol, ...) {
  ## if datecol is a datetime column, convert to date
  if (inherits(data[[datecol]], "POSIXt")) {
    data[[datecol]] <- as.Date(data[[datecol]])
  }
  
  # Capture grouping variables
  dots <- list(..., datecol)
  
  get_year_from_date <- function(date) {
    as.integer(as.POSIXlt(date)$year + 1900)
  }
  
  ## Capture the formulas for the summaries
  readings_formula <- interp(~length(na.omit(x)), x = as.name(valcol))
  avg_formula <- interp(~ifelse(n_readings >= 18, mean(x, na.rm = TRUE),
                                NA_real_),
                        x = as.name(valcol))
  year_formula <- interp(~get_year_from_date(x), x = as.name(datecol))
  
  group_by_(data, .dots = dots) %>%
    summarise_(n_readings = readings_formula,
               avg_24hr   = avg_formula) %>%
    mutate_(year = year_formula) %>% 
    ungroup()
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
