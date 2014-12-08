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
pm_98_percentile <- function(data, yearcol, valcol, ...) {
  data <- data[!is.na(data[[valcol]]),]
  
  dots <- list(..., yearcol)
  
  arrange_formula <- interp(~desc(x), x = as.name(valcol))
  
  cut_rank <- function(n) {
    cuts <- c(1,50,100,150,200,250,300,350,366)
    ret <- cut(n, cuts, include.lowest = TRUE, labels = 1:8, right = TRUE)
    as.numeric(ret)
  }
  
  ans <- group_by_(data, .dots = dots) %>%
    arrange_(arrange_formula) %>%
    slice(cut_rank(n()))
  
  ans
  
}
