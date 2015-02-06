#' Bin a vector of air quaility readings into CAAQS management levels
#'
#' Can be used for Ozone, PM2.5 (daily or annual)
#' @param x vector of air quality readings (pollutant concentrations)
#' @param  parameter What is the parameter? Must be one of: \code{"o3", "pm2.5_annual", "pm2.5_daily"}
#' @param  output should the function output labels (\code{"labels"}; default
#'  or break values in unicode (\code{"breaks_u"}) or break values in html (\code{"breaks_h"}), 
#'  or colour values (\code{"colour"} or \code{"color"})?
#' @param  drop_na Should NA values be dropped, or retained as factor level "Insufficient Data" (default)?
#' @export
#' @return factor
#' @examples \dontrun{
#'
#'}
cut_caaq <- function(x, parameter, output = "labels", drop_na = FALSE) {
  
  l <- mgmt_levels(parameter)

  if (!output %in% c("labels", "breaks_h", "breaks_u", "colour", "color")) {
    stop(output, " is not a valid input for output. It must be either 'labels',", 
         "'breaks_u' (for unicode encoding), 'breaks_h' (for html encoding)", 
         " or 'colour' to get hexadecimal colour codes")
  }
  
  breaks <- c(l$lower_breaks, Inf)
  
  if (output == "labels") {
    labels <- l$labels
  } else if (output == "breaks_u") {
    labels <- l$val_labels_unicode
  } else if (output == "breaks_h") {
    labels <- l$val_labels_html
  } else if (output %in% c("colour", "color")) {
    labels <- l$colour
  }
  
  labels <- as.character(labels)
  
  if (!drop_na && any(is.na(x))) {
    x[is.na(x)] <- -Inf
    x[x == 0] <- 0.0001
    breaks <- c(-Inf, breaks)
    if (output %in% c("colour", "color")) {
      labels <- c("#CCCCCC", labels)
    } else {
      labels <- c("Insufficient Data", labels)
    }
  }

  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE, right = TRUE, 
      ordered_result = TRUE)
}

#' Return CAAQS management levels for any, or all, parameters
#' 
#' @param parameter one of "all" (default), "o3", "PM2.5_annual", "PM2.5_daily"
#' @export
#' @return A data frame
#' @examples \dontrun{
#' get_levels("o3") 
#'}
mgmt_levels <- function(parameter = "all") {
  
  caaqs_levels <- caaqs_levels
  
  parameter <- tolower(parameter)
  
  match.arg(parameter, c(levels(caaqs_levels$parameter), "all"))
  
  if (parameter == "all") return(caaqs_levels)
            
  l <- caaqs_levels[caaqs_levels$parameter == parameter, ]
  l
}
