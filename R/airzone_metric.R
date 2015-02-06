#' Calculate the airzone metric and achievement status
#'
#'<full description>
#' @param df The dataframe
#' @param  n The column containing the number of years each 3yr avg is based on
#' @param  avg The column containing the 3 yr averages
#' @param  std the standard (e.g. 63 for ozone)
#' @param  stn_id the column containing the station ids
#' @param  stn_name the column containing the station names
#' @export
#' @return A dataframe with two columns: metric value and achievement status
#' @examples \dontrun{
#'
#'}
airzone_metric <- function(df, n, avg, stn_id, stn_name, std) {
  
  lvls <- c("Achieved", "Not Achieved", "Insufficient Data")
  
  if (all(is.na(df[,avg]))) {
    maxn <- NA
    caaq_metric <- NA
    caaq_status <- factor("Insufficient Data", levels = lvls)
    rep_station_id <- NA
    rep_station_name <- NA
  } else {
    maxn <- max(df[,n], na.rm = TRUE)
    
    df <- df[df[,n] == maxn, , drop = FALSE]
    
    i <- which.max(df[df[,n] == maxn,avg])
    
    caaq_metric <- df[i,avg]
    caaq_status <- factor(ifelse(caaq_metric <= std, lvls[1], lvls[2]), 
                          levels = lvls)
    rep_station_id <- df[i,stn_id]
    rep_station_name <- df[i,stn_name]
  }
  
  data.frame(nyears = maxn, caaq_metric, caaq_status, 
             rep_station_id, rep_station_name, stringsAsFactors = FALSE)
  
}
