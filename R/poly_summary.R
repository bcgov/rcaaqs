#' A function to summarise occurrences of a variable within a polygon
#'
#' @import sp
#' @param  polys SpatialPolygonsDataFrame to summarise by
#' @param  points SpatialPointsDataFrame to summarise
#' @param  fn The summary function with which to summarise point data in each polygon
#' @param  ... Other arguments to fn
#' @export
#' @return A data frame with the same number of rows as the SpatialPolygonsDataFrame.
#' @examples \dontrun{
#'
#'}
poly_summary <- function(polys, points, fn, ...) {
  
  if (!class(points) == "SpatialPointsDataFrame") {
    stop("points is not of class SpatialPointsDataFrame")
  }
  
  if (!class(polys) == "SpatialPolygonsDataFrame") {
    stop("polys is not of class SpatialPolygonsDataFrame")
  }
  
  if (!class(fn) == "function") {
    stop("fn is not a valid function")
  }
  
  out <- sapply(over(polys, geometry(points), returnList = TRUE), simplify = FALSE, 
                function(x) {
                  data <- points@data[x,]
                  ret <- fn(data, ...)
                  return(ret)  
                })
  
  out <- do.call("rbind", out)
  out$poly_id <- rownames(out)
  out
}
