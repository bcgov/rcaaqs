#' Bin a vector of air quaility readings into CAAQS management levels
#'
#' Can be used for Ozone, PM2.5 (daily or annual)
#' @param x vector of air quality readings (pollutant concentrations)
#' @param  metric What is the metric? Must be one of: \code{"o3", "pm2.5_annual", "pm2.5_daily"}
#' @param  drop_na Should NA values be dropped, or retained as factor level "No Data" (default)?
#' @export
#' @keywords
#' @seealso
#' @return factor
#' @alias
#' @examples \dontrun{
#'
#'}
cut_caaq <- function(x, metric, drop_na = FALSE) {
  
  metrics <- c("o3", "pm2.5_annual", "pm2.5_daily")
  
  metric <- tolower(metric)
  
  if (!metric %in% metrics) {
    stop(metric, " is not a valid metric value. it must be one of ", 
         paste0("'", metrics, "'", collapse = ", "))
  }
  
  if (!drop_na) x[is.na(x)] <- -Inf
  
  breaks <- c(-Inf, 0, 50, 56, 63, Inf, 
              -Inf, 0, 4.0, 6.4, 10.0, Inf, 
              -Inf, 0, 10, 19, 28, Inf)
  names(breaks) <- c(rep(metrics[1], 6), rep(metrics[2],6), rep(metrics[3],6))
  breaks <- breaks[names(breaks) == metric]
  
  labels <- c("No Data", 
              "Actions for Keeping Clean Areas Clean", 
              "Actions for Preventing Air Quality Deterioration", 
              "Actions for Preventing CAAQS Exceedance", 
              "Actions for Achieving Air Zone CAAQS")
  
  if (drop_na) {
    breaks <- breaks[-1]
    labels <- labels[-1]
  }
  
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE, right = TRUE, 
      ordered_result = TRUE)
}
