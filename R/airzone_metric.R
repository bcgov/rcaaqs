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
#' @param az Airzone column
#' @param caaqs_val Column containing the CAAQS metric for individual stations
#' @param caaqs Column containing the CAAQS achievement levels for individual
#'   stations
#' @param mgmt Column containing the CAAQS management levels for individual
#'   stations
#' @param keep Columns in the input data that you would like to retain in the 
#'   output data frame. You can make it a named vector to rename the column in 
#'   the output. Use the form \code{keep = c(new_name = "existing_name")}. This 
#'   can also be used to rename any of the columns specified by n_years, az, or 
#'   val.
#'  
#' @return A dataframe with 6 columns: airzone, number of years in calculation,
#'   CAAQS metric value, CAAQS achievement levels, and CAAQS management levels
#'
#' @export

airzone_metric <- function(data, n_years = "n_years", az = "airzone", 
                           caaqs_val = "metric_value", caaqs = "caaqs", 
                           mgmt = "mgmt", keep = NULL) {
  
  # Check inputs
  check_vars(c(n_years, az, caaqs_val, caaqs), data)
  check_one(n_years, az, caaqs_val, caaqs)
  check_class(n_years, data, "numeric")
  check_class(caaqs_val, data, "numeric")
  
  # Take station with max caaqs for each airzone
  data <- tidyr::nest(data, - !!rlang::sym(az))
  data <- dplyr::mutate(data, data = purrr::map(.data$data, ~ dplyr::slice(.x, which.max(.x[[caaqs_val]]))))
  data <- tidyr::unnest(data)
  data <- dplyr::arrange(data, !!rlang::sym(az))
  
  # Arrange column order
  data <- data[, c(az, n_years, caaqs_val, caaqs, mgmt,
               setdiff(keep, c(az, n_years, caaqs_val, caaqs, mgmt)))]
  
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
#' \code{sf} package containing the airzone names and locations (polygons).
#' 
#' For British Columbia, consider using the \code{airzones()} function to
#' get an sf object of BC airzones from the \code{\link[bcmaps]{bcmaps}} package.
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
