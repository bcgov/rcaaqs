# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

#'Bin a vector of air quaility readings into CAAQS management levels
#'
#'Can be used for Ozone, PM2.5 (24h or annual)
#'@param x vector of air quality readings (pollutant concentrations)
#'@param  parameter What is the parameter? Must be one of: `"o3",
#'  "pm2.5_annual", "pm2.5_24h"`
#'@param  output should the function output labels (`"labels"`; default or
#'  break values in unicode (`"breaks_u"`) or break values in html
#'  (`"breaks_h"`), or colour values (`"colour"` or `"color"`)?
#'@param  drop_na Should NA values be dropped, or retained as factor level
#'  "Insufficient Data" (default)?
#'@noRd
#'@return factor

cut_management <- function(x, parameter, output = "labels", drop_na = FALSE) {
  cut_caaqs(type = "management", x = x, parameter = parameter, output = output, 
           drop_na = drop_na)
}

#'Bin a vector of air quaility readings into CAAQS achievement levels
#'
#'Can be used for Ozone, PM2.5 (24h or annual)
#'@param x vector of air quality readings (pollutant concentrations)
#'@param  parameter What is the parameter? Must be one of: `"o3",
#'  "pm2.5_annual", "pm2.5_24h"`
#'@param  output should the function output labels (`"labels"`; default or
#'  break values in unicode (`"breaks_u"`) or break values in html
#'  (`"breaks_h"`), or colour values (`"colour"` or `"color"`)?
#'@param  drop_na Should NA values be dropped, or retained as factor level
#'  "Insufficient Data" (default)?
#'@noRd
#'@return factor

cut_achievement <- function(x, parameter, output = "labels", drop_na = FALSE) {
  cut_caaqs(type = "achievement", x = x, parameter = parameter, output = output, 
           drop_na = drop_na)
}

#' @keywords internal
cut_caaqs <- function(type, x, parameter, output, drop_na) {
  levels <- get_levels(type, parameter)
  breaks <- c(levels$lower_breaks, Inf)
  labels <- get_labels(levels, output, drop_na)
  
  if (!drop_na) {
    x[is.na(x)] <- -Inf
    x[x == 0] <- 0.000001
    breaks <- c(-Inf, breaks)
  }
  
  cut(x, breaks = breaks, labels = labels, ordered_result = TRUE, 
      include.lowest = TRUE, right = TRUE)
}

#' Return CAAQS levels for any or all, parameters, for either achievement or 
#' management reporting
#'
#' @param type one of "achievement", "management"
#' @param parameter one of "all" (default), "o3", "PM2.5_annual", "PM2.5_24h"
#' 
#' @return A data frame
#' 
#' @examples \dontrun{
#'  get_levels("o3") 
#' }
#' 
#' @noRd

get_levels <- function(type, parameter = "all") {
  
  if (type == "achievement") {
    levels <- rcaaqs::achievement_levels
  } else if (type == "management") {
    levels <- rcaaqs::management_levels
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
#' @param x Data frame. A subset of achievement_levels or caaqs_levels
#' @param output Character. One of: "labels", "breaks_h", "breaks_u", "colour",
#'   "color"
#' @keywords internal
#' @return character vector
#' 
#' @noRd

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
  
  labels <- factor(labels, levels = labels, ordered = TRUE)
  if(drop_na) labels <- droplevels(labels[-1])
  
  if(output == "labels") labels else as.character(labels)
}

#' Get the CAAQS value for a given parameter
#'
#' @param parameter parameter of interest. One or more of 
#'
#' @return a named vector of standards
#' @keywords internal
#' 
#' @noRd

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
#' @noRd

get_units <- function(parameter = "all") {
  levels <- get_levels("achievement", parameter = parameter)
  units_df <- unique(levels[c("parameter", "units_unicode")])
  units <- as.character(units_df[["units_unicode"]])
  names(units) <- units_df[["parameter"]]
  units[!is.na(units)]
}

#' Get a vector of colours for management levels or achievement levels
#' 
#' @param type "achievement" or "management"
#' @param drop_na Should NA (i.e., Insufficient Data) be excluded (`TRUE`) 
#'   or includeded `FALSE` (the default)?
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
