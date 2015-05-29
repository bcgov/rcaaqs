# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#'Bin a vector of air quaility readings into CAAQS management levels
#'
#'Can be used for Ozone, PM2.5 (24h or annual)
#'@param x vector of air quality readings (pollutant concentrations)
#'@param  parameter What is the parameter? Must be one of: \code{"o3",
#'  "pm2.5_annual", "pm2.5_24h"}
#'@param  output should the function output labels (\code{"labels"}; default or
#'  break values in unicode (\code{"breaks_u"}) or break values in html
#'  (\code{"breaks_h"}), or colour values (\code{"colour"} or \code{"color"})?
#'@param  drop_na Should NA values be dropped, or retained as factor level
#'  "Insufficient Data" (default)?
#'@export
#'@return factor

cut_management <- function(x, parameter, output = "labels", drop_na = FALSE) {
  cut_caaq(type = "management", x = x, parameter = parameter, output = output, 
           drop_na = drop_na)
}

#'Bin a vector of air quaility readings into CAAQS achievement levels
#'
#'Can be used for Ozone, PM2.5 (24h or annual)
#'@param x vector of air quality readings (pollutant concentrations)
#'@param  parameter What is the parameter? Must be one of: \code{"o3",
#'  "pm2.5_annual", "pm2.5_24h"}
#'@param  output should the function output labels (\code{"labels"}; default or
#'  break values in unicode (\code{"breaks_u"}) or break values in html
#'  (\code{"breaks_h"}), or colour values (\code{"colour"} or \code{"color"})?
#'@param  drop_na Should NA values be dropped, or retained as factor level
#'  "Insufficient Data" (default)?
#'@export
#'@return factor

cut_achievement <- function(x, parameter, output = "labels", drop_na = FALSE) {
  cut_caaq(type = "achievement", x = x, parameter = parameter, output = output, 
           drop_na = drop_na)
}

#' @keywords internal
cut_caaq <- function(type, x, parameter, output, drop_na) {
  levels <- get_levels(type, parameter)

  breaks <- c(levels$lower_breaks, Inf)
  
  sub_na <- !drop_na && any(is.na(x))
  
  labels <- get_labels(levels, output, drop_na)
  
  if (!drop_na) {
    x[is.na(x)] <- -Inf
    x[x == 0] <- 0.000001
    breaks <- c(-Inf, breaks)
  }
  
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE, right = TRUE, 
      ordered_result = TRUE)
}

#'Return CAAQS levels for any or all, parameters, for either achievement or
#'management reporting
#'
#'@param type one of "achievement", "management"
#'@param parameter one of "all" (default), "o3", "PM2.5_annual", "PM2.5_24h"
#'@export
#'@return A data frame
#' @examples \dontrun{
#' get_levels("o3") 
#'}
get_levels <- function(type, parameter = "all") {
  
  if (type == "achievement") {
    levels <- achievement_levels
  } else if (type == "management") {
    levels <- management_levels
  } else {
    stop("type must be one of achievement or management")
  }
  
  parameter <- tolower(parameter)
  
  if (!all(parameter %in% c(levels(levels$parameter), "all"))) 
    stop("Unrecognized parameter(s)")
  
  if ("all" %in% parameter) return(levels)
  
  levels[levels$parameter %in% parameter, ]
}

#' Get labels
#'
#' @param x dataframe - a subset of achievement_levels or caaqs_levels
#' @param output one of: "labels", "breaks_h", "breaks_u", "colour", "color"
#' @keywords internal
#' @return character vector
get_labels <- function(x, output, drop_na) {
  
  if (!output %in% c("labels", "breaks_h", "breaks_u", "colour", "color")) {
    stop(output, " is not a valid input for output. It must be either 'labels',", 
         "'breaks_u' (for unicode encoding), 'breaks_h' (for html encoding)", 
         " or 'colour' to get hexadecimal colour codes")
  }
  
  if (output == "labels") {
    labels <- x[["labels"]]
  } else if (output == "breaks_u") {
    labels <- x[["val_labels_unicode"]]
  } else if (output == "breaks_h") {
    labels <- x[["val_labels_html"]]
  } else if (output %in% c("colour", "color")) {
    labels <- x[["colour"]]
  }
  
  labels <- as.character(labels)
  
  if (!drop_na) {
    if (output %in% c("colour", "color")) {
      labels <- c("#CCCCCC", labels)
    } else {
      labels <- c("Insufficient Data", labels)
    }
  }
  
  labels
}

#' get the CAAQS value for a given parameter
#'
#' @param parameter parameter of interest. One or more of 
#'
#' @return a named vector of standards
#' @keywords internal
#'
get_std <- function(parameter = "all") {
  levels <- get_levels("achievement", parameter = parameter)
  stds <- levels[levels$labels == "Not Achieved", "lower_breaks", drop = TRUE]
  names(stds) <- levels[levels$labels == "Not Achieved", "parameter", drop = TRUE]
  stds
}

#' get the CAAQS value for a given parameter
#'
#' @param parameter parameter of interest. One or more of 
#'
#' @return a named vector of units
#' @keywords internal
#'
get_units <- function(parameter = "all") {
  levels <- get_levels("achievement", parameter = parameter)
  units_df <- unique(levels[c("parameter", "units_unicode")])
  units <- as.character(units_df[["units_unicode"]])
  names(units) <- units_df[["parameter"]]
  units
}

#' Get a vector of colours for management levels or achievement levels
#' 
#' @param type "achievement" or "management"
#' @param drop_na Should NA (i.e., Insufficient Data) be excluded (\code{TRUE})
#'   or includeded \code{FALSE} (the default)?
#'   
#' @return named vector of colours (hex)
#' @export
get_colours <- function(type = "management", drop_na = FALSE) {
  if (!type %in% c("management", "achievement")) 
    stop("type must be 'achievement' or 'management'")
  levels <- get_levels(type = type, parameter = "pm2.5_24h")
  colours <- get_labels(levels, "colour", drop_na = drop_na)
  names(colours) <- get_labels(levels, "labels", drop_na = drop_na)
  colours
}
