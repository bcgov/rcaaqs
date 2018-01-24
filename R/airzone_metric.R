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
#' @param caaq the column containing the caaq metric for individual stations
#' @param keep columns in the input data that you would like to retain in the 
#'   output data frame. You can make it a named vector to rename the column in 
#'   the output. Use the form \code{keep = c(new_name = "existing_name")}. This 
#'   can also be used to rename any of the columns specified by n_years, az, or 
#'   val.
#'  
#' @return A dataframe with four columns: airzone, number of years in
#'  calculation, and caaq metric value
#'
#' @export

airzone_metric <- function(data, n_years = "n_years", az = "airzone", 
                           caaq = "caaq_metric", keep = NULL) {
  
  # Check inputs
  check_vars(c(n_years, az, caaq), data)
  if (!is.numeric(data[[n_years]])) stop("Data column ", n_years, " must be numeric")
  if (!is.numeric(data[[caaq]])) stop("Data column", caaq, " must be numeric")
  
  # set caaq column to NA if n_years < 3 (unless only have 2 years)
  data <- tidyr::nest(data, - !!!rlang::syms(az))
  data <- dplyr::mutate(data, data = purrr::map(.data$data, ~ parse_incomplete(., n_years, caaq)))
  
  # Take station with max caaq for each airzone
  data <- dplyr::mutate(data, data = purrr::map(.data$data, ~ dplyr::slice(.x, which.max(.x[[caaq]]))))
  data <- tidyr::unnest(data)
  data <- dplyr::arrange(data, !!!rlang::syms(az))
  
  # Arrange column order
  data <- data[, c(az, n_years, caaq,
               setdiff(keep, c(az, n_years, caaq)))]
  
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
#' station belongs to. Requires a Spatial Polygons Data Frame object from the
#' \code{sp} package containing the airzone names and locations (polygons).
#' 
#' For British Columbia, consider using the \link[bcmaps]{airzones("sp")}
#' function to extract a Spatial Polygons Data Frame of BC airzones from the 
#' \code{\link{bcmaps}} package.
#' 
#' @param data Data frame. Contains station ids and air quality data
#' @param airzones SpatialPolygonsDataFrame. Polygons reflecting airzone
#'   locations
#' @param az Character. Name of airzones column in the 'airzones'
#'   SpatialPolygonsDataFrame object
#' @param station_id Character. Name of the station_id column in 'data'
#' @param coords Character vector. Names of the columns containing latitude and 
#'   longitude (respectively) for each station. Defaults to 'lat' and 'lon'
#' 
#' @examples 
#' 
#' \dontrun{
#' # Using the airzone spatial dataframe from the bcmaps package
#' bc_airzones <- bcmaps::airzones("sp")
#' 
#' by_airzones <- assign_airzone(data, bc_airzones)
#' }
#'
#' @export
assign_airzone <- function(data, airzones, az = "Airzone", 
                           station_id = "ems_id",
                           coords = c("lat", "lon")) {
  # Check inputs
  check_vars(c(station_id, coords), data)
  check_class(coords[1], data, "numeric")
  check_class(coords[2], data, "numeric")
  
  if(all(class(airzones) != "SpatialPolygonsDataFrame")) {
    stop("airzones must be a SpatialPolygonsDataFrame (from the 'sp' package)")
  }

  check_vars(az, airzones@data, name_data = "the airzones Spatial Polygons Data Frame")
  
  if(!(az %in% names(airzones@data))) {
    stop("'", az, "' is not a column in the Spatial Polygons Data Frame")
  }
  
  # Rename to standard
  names(airzones@data)[names(airzones@data) == az] <- "airzone"
  names(data)[names(data) == coords[1]] <- "lat"
  names(data)[names(data) == coords[2]] <- "lon"
  
  if(min(data$lat, na.rm = TRUE) < -90 | max(data$lat, na.rm = TRUE) > 90) stop("latitude can only range from -90 to +90")
  if(min(data$lon, na.rm = TRUE) < -180 | max(data$lon, na.rm = TRUE) > 180) stop("longitude can only range from -180 to +180")
  
  
  # Get stations data
  st <- dplyr::select(data, station_id, "lat", "lon")
  st <- dplyr::distinct(st)
  
  # Convert all to long/lat
  st_coord <- st
  sp::coordinates(st_coord) <- c("lon", "lat")
  sp::proj4string(st_coord) <- "+proj=longlat +datum=WGS84"
  
  airzones <- sp::spTransform(airzones, sp::CRS("+proj=longlat +datum=WGS84"))
  
  # Calculate overlap between stations and airzones
  st <- cbind(st, sp::`%over%`(st_coord, airzones))
  
  # Merge with dataframe
  data <- dplyr::left_join(data, st[, c(station_id, "airzone")], by = station_id)
  data
}
