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


#' Calculate the airzone metric
#'
#' <full description>
#' 
#' @param data The dataframe
#' @param n_years The column containing the number of years each 3yr avg is
#'   based on
#' @param az The airzone column
#' @param ambient_metric_val The column containing the ambient CAAQS metric for individual stations
#' @param ambient_caaqs The column containing the CAAQS achievement levels for individual
#'   stations
#' @param excluded Logical vector indicating if ambient data was excluded for calculating
#'   the CAAQS management levels
#' @param mgmt_metric_val The column containing the management CAAQS metric for individual stations
#' @param mgmt The column containing the CAAQS management levels for individual
#'   stations
#' @param keep The columns in the input data that you would like to retain in the 
#'   output data frame. You can make it a named vector to rename the column in 
#'   the output. Use the form `keep = c(new_name = "existing_name")`. This 
#'   can also be used to rename any of the columns specified by n_years, az, or 
#'   val.
#'  
#' @return A dataframe with 9 columns: airzone, number of years in calculation,
#'   ambient CAAQS metric value, CAAQS achievement levels, ambient reporting station id,
#'   logical vector indicating if ambient data was excluded for management calculations, 
#'   management CAAQS metric value, CAAQS management levels, and management reporting station id.
#'
#' @export

airzone_metric <- function(data, n_years = "n_years", az = "airzone", 
                           ambient_metric_val = "ambient_metric_value",
                           ambient_caaqs = "ambient_caaqs",
                           excluded = "excluded", mgmt_metric_val = "mgmt_metric_value",
                           mgmt = "mgmt_level", keep = NULL) {
  
  # Check inputs
  check_vars(c(n_years, az, ambient_metric_val, ambient_caaqs, mgmt_metric_val, mgmt), data)
  check_one(n_years, az, ambient_metric_val, ambient_caaqs, mgmt_metric_val, mgmt)
  check_class(n_years, data, "numeric")
  check_class(ambient_metric_val, data, "numeric")
  check_class(mgmt_metric_val, data, "numeric")
  
  # Take station with max ambient_metric_value for each airzone
  data <- tidyr::nest(data, - !!rlang::sym(az))
  data1 <- dplyr::mutate(data, data = purrr::map(.data$data, ~ dplyr::slice(.x, which.max(.x[[ambient_metric_val]]))))
  data1 <- tidyr::unnest(data1)
  data1 <- dplyr::arrange(data1, !!rlang::sym(az))
  
  # Arrange column order
  data1 <- data1[, c(az, n_years, ambient_metric_val, ambient_caaqs, 
                   setdiff(c(keep, "ems_id"), c(az, n_years, ambient_metric_val, ambient_caaqs)))]
  
  colnames(data1)[which(names(data1) == "ems_id")] <- "ambient_rep_stn_id" 

  # Take station with max mgmt_metric_value for each airzone
  data2 <- dplyr::mutate(data, data = purrr::map(.data$data, ~ dplyr::slice(.x, which.max(.x[[mgmt_metric_val]]))))
  data2 <- tidyr::unnest(data2)
  data2 <- dplyr::arrange(data2, !!rlang::sym(az))
  
  # Arrange column order
  data2 <- data2[, c(az, n_years, excluded, mgmt_metric_val, mgmt,
               setdiff(c(keep, "ems_id"), c(az, n_years, excluded, mgmt_metric_val, mgmt)))]
  
  colnames(data2)[which(names(data2) == "ems_id")] <- "mgmt_rep_stn_id" 
  
  # Join dataframes
  data <- dplyr::left_join(data1, data2)
    
  data
  
  # Rename keep columns if asked to
  if (!is.null(names(keep))) {
    for (k in keep) {
      n <- names(keep)[keep == k]
      if (nchar(n) > 0) {
        names(data)[names(data) == k] <- n
      }
    }
  }
  data
}

parse_incomplete <- function(data, n_years, val) {
  if (all(data[[n_years]] < 3)) {
    data <- data
  } else {
    data[data[[n_years]] < 3, val] <- NA
  }
  data
}

#' Assign locations to airzones
#' 
#' With a data set containing station locations, calculate which airzones each 
#' station belongs to. Requires an sf object from the
#' `sf` package containing the airzone names and locations (polygons).
#' 
#' For British Columbia, consider using the `airzones()` function to
#' get an sf object of BC airzones from the [bcmaps][bcmaps::bcmaps()] package.
#' 
#' @param data Data frame. Contains station ids and air quality data
#' @param airzones sf PPOLYGON or MULTIPOLYGON reflecting airzone
#'   locations
#' @param az Character. Name of airzones column in the 'airzones'
#'   sf object
#' @param station_id Character. Name of the station_id column in 'data'
#' @param coords Character vector. Names of the columns containing longitude and 
#'   latitude (respectively) for each station. Defaults to 'lon' and 'lat'
#' 
#' @examples 
#' 
#' \dontrun{
#' # Using the airzones function from the bcmaps package
#' bc_airzones <- bcmaps::airzones()
#' 
#' by_airzones <- assign_airzone(data, bc_airzones)
#' }
#'
#' @export
assign_airzone <- function(data, airzones, az = "Airzone", 
                           station_id = "ems_id",
                           coords = c("lat", "lon")) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("sf package is required for this function. Please install it.", 
         call. = FALSE)
  }
  # Check inputs
  check_vars(c(station_id, coords), data)
  check_one(az, station_id)
  check_class(coords[1], data, "numeric")
  check_class(coords[2], data, "numeric")
  
  if (!inherits(airzones, "sf") || 
                !all(sf::st_is(airzones, c("POLYGON", "MULTIPOLYGON")))) {
    stop("airzones must be an sf object of POLYGONs or MULTIPOLYGONs")
  }

  check_vars(az, airzones)
  
  if (!(az %in% names(airzones))) {
    stop("'", az, "' is not a column in the airzones object")
  }
  
  # Rename to standard
  names(airzones)[names(airzones) == az] <- "airzone"
  names(data)[names(data) == coords[1]] <- "lon"
  names(data)[names(data) == coords[2]] <- "lat"
  
  if (min(data$lat, na.rm = TRUE) < -90 | max(data$lat, na.rm = TRUE) > 90) 
    stop("latitude can only range from -90 to +90")
  if (min(data$lon, na.rm = TRUE) < -180 | max(data$lon, na.rm = TRUE) > 180) 
    stop("longitude can only range from -180 to +180")

  # Convert data to sf
  data <- sf::st_as_sf(data, coords = c("lon", "lat"), crs = 4326, 
                           remove = FALSE)
  
  # convert data to same projection as airzones
  data <- sf::st_transform(data, sf::st_crs(airzones))
  data <- sf::st_join(data, airzones[, "airzone", drop = FALSE])
  
  # return without sfc geometry column
  sf::st_set_geometry(data, NULL)
}
