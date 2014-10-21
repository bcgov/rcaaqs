#' Output a spatial*DataFrame to geojson
#'
#' Deals with a peculiarity of writeOGR where file has to be \code{".geojson"}, optionally preceeded by a path, but cannot have a filename preceeding the \code{".geojson"}.
#' Also deals with factors (including ordered factors)
#' @import rgdal
#' @param spdf The SpatialDataFrame
#' @param  outpath The output path
#' @param  outname The output filename
#' @export
#' @return Nothing
#' @examples \dontrun{
#' output_geojson(myspatialdataframe, "out", "file.geojson")
#'
#'}
output_geojson <- function (spdf, outpath, outname) {
  
  if(any(missing(spdf), missing(outpath), missing(outname))) {
    stop("You must provide all arguments")
  }
  
  spdf@data <- data.frame(lapply(spdf@data, function(x) {
    if ("factor" %in% class(x)) {
      x <- as.character(x)
      message("Column is a factor, converting to character")
    }
    x
  }), stringsAsFactors = FALSE)
  
  dsn <- paste0(outpath, "/.geojson")
  
  writeOGR(spdf, dsn = dsn, layer = "", driver = "GeoJSON")
  system(paste0("mv ", dsn, " ", outpath, "/", outname))
}
