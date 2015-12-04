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

#' A function to summarise occurrences of a variable within a polygon
#'
#' @param  polys SpatialPolygonsDataFrame to summarise by
#' @param  points SpatialPointsDataFrame to summarise
#' @param  fn The summary function with which to summarise point data in each polygon
#' @param  ... Other arguments to fn
#' @export
#' @return A data frame with the same number of rows as the SpatialPolygonsDataFrame.

poly_summary <- function(polys, points, fn, ...) {
  
  if (!requireNamespace("sp", quietly = TRUE)) {
    stop("sp Package required for this function to work. Please install it", 
         call. = FALSE)
  }
  
  if (!class(points) == "SpatialPointsDataFrame") {
    stop("points is not of class SpatialPointsDataFrame")
  }
  
  if (!class(polys) == "SpatialPolygonsDataFrame") {
    stop("polys is not of class SpatialPolygonsDataFrame")
  }
  
  if (!class(fn) == "function") {
    stop("fn is not a valid function")
  }
  
  out <- sapply(sp::over(polys, sp::geometry(points), returnList = TRUE), simplify = FALSE, 
                function(x) {
                  data <- points@data[x,]
                  ret <- fn(data, ...)
                  return(ret)  
                })
  
  out <- do.call("rbind", out)
  out$poly_id <- rownames(out)
  out
}
