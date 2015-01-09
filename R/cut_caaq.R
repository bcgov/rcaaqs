#' Bin a vector of air quaility readings into CAAQS management levels
#'
#' Can be used for Ozone, PM2.5 (daily or annual)
#' @param x vector of air quality readings (pollutant concentrations)
#' @param  metric What is the metric? Must be one of: \code{"o3", "pm2.5_annual", "pm2.5_daily"}
#' @param  output should the function output labels (\code{"labels"}; default
#'  or break values in unicode (\code{"breaks_u"}) or break values in html (\code{"breaks_h"})?
#' @param  drop_na Should NA values be dropped, or retained as factor level "Insufficient Data" (default)?
#' @export
#' @return factor
#' @examples \dontrun{
#'
#'}
cut_caaq <- function(x, metric, output = "labels", drop_na = FALSE) {
  
  metrics <- c("o3", "pm2.5_annual", "pm2.5_daily")
  
  metric <- tolower(metric)
  
  if (!metric %in% metrics) {
    stop(metric, " is not a valid metric value. it must be one of ", 
         paste0("'", metrics, "'", collapse = ", "))
  }
  
  if (!output %in% c("labels", "breaks_h", "breaks_u")) {
    stop(output, " is not a valid input for output. It must be either 'labels',", 
         "'breaks_u' (for unicode encoding), or 'breaks_h' (for html encoding)")
  }
  
  
  if (!drop_na) x[is.na(x)] <- -Inf
  
  breaks <- c(-Inf, 0, 50, 56, 63, Inf, 
              -Inf, 0, 4.0, 6.4, 10.0, Inf, 
              -Inf, 0, 10, 19, 28, Inf)
  names(breaks) <- c(rep(metrics[1], 6), rep(metrics[2],6), rep(metrics[3],6))
  breaks <- breaks[names(breaks) == metric]
  
  u_units <- c(o3 = "ppb", pm2.5_annual = "\u03BCg/m\u00B3", pm2.5_daily = "\u03BCg/m\u00B3")
  u_unit <- u_units[names(u_units) == metric]
  
  html_units <- c(o3 = "ppb", pm2.5_annual = "&mu;g/m&sup3;", pm2.5_daily = "&mu;g/m&sup3;")
  html_unit <- html_units[names(html_units) == metric]
  
  if (output == "labels") {
    labels <- c("Insufficient Data", 
                "Actions for Keeping Clean Areas Clean", 
                "Actions for Preventing Air Quality Deterioration", 
                "Actions for Preventing CAAQS Exceedance", 
                "Actions for Achieving Air Zone CAAQS")
  } else if (output == "breaks_u") {
    labels <- c("Insufficient Data", 
                paste0("\u2264 ", breaks[3], u_unit), 
                paste0("\u003E ", breaks[3], u_unit, " \u0026 \u2264 ", breaks[4], u_unit), 
                paste0("\u003E ", breaks[4], u_unit, " \u0026 \u2264 ", breaks[5], u_unit), 
                paste0("\u003E ", breaks[5], u_unit))
  } else if (output == "breaks_h") {
    labels <- c("Insufficient Data", 
                paste0("&leq; ", breaks[3], html_unit), 
                paste0("&gt; ", breaks[3], html_unit, " &amp; &leq; ", breaks[4], html_unit), 
                paste0("&gt; ", breaks[4], html_unit, " &amp; &leq; ", breaks[5], html_unit), 
                paste0("&gt; ", breaks[5], html_unit))
  }
  
  if (drop_na) {
    breaks <- breaks[-1]
    labels <- labels[-1]
  }
  
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE, right = TRUE, 
      ordered_result = TRUE)
}
