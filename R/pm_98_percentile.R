#' Calculate the annual 98th percentile of daily average PM2.5 values according to CAAQS standards
#' 
#' Designed to be used with the output from \code{\link{pm_avg_daily}}
#' @import dplyr
#' @import lazyeval
#' @param  data data frame
#' @param  yearcol the name of the "year" column (as a character string)
#' @param  valcol the name of the column with daily average PM2.5 values
#' @param  idcol (optional) the name of the column containing station identifier (e.g., ems_id)
#' @export
#' @seealso \code{\link{pm_avg_daily}}
#' @return  A data frame with 98th percentiles of daily averages, per year
#' @examples \dontrun{
#' 
#'}
pm_98_percentile <- function(data, yearcol, valcol, idcol = NULL) {
  data <- data[!is.na(data[[valcol]]),]
  
  rank_and_slice <- function(data, yearcol, valcol, idcol) {
    data <- group_by_(data, yearcol)
    
    N_formula <- interp(~length(x), x = as.name(valcol))
    arrange_formula <- interp(~desc(x), x = as.name(valcol))
    slice_formula <- interp(~ranks$rank[ranks[[yearcol]] == max(x)], 
                            x = as.name(yearcol))
    
    ranks <- data %>%
      summarise_(N = N_formula, 
                 i = ~floor(N * 0.98), 
                 rank = ~N - i)
    
    ans <- data %>%
      arrange_(arrange_formula) %>%
      slice_(slice_formula)
    
    ans 
  }
  
  if (!is.null(idcol)) {
    out <- lapply(unique(data[[idcol]]), function(x) {
      data <- data[data[[idcol]] == x,]
      rank_and_slice(data = data, yearcol = yearcol, valcol = valcol)
    })
    out <- rbind_all(out) ## rbind_all soon to be deprecated, will need to use bind_rows
  } else {
    out <- rank_and_slice(data = data, yearcol = yearcol, valcol = valcol)
  }
  
  out
  
}
