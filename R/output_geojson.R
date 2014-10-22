#' Output a spatial*DataFrame to geojson
#'
#' Deals with a peculiarity of writeOGR where file has to be \code{".geojson"}, optionally preceeded by a path, but cannot have a filename preceeding the \code{".geojson"}.
#' Also deals with factors (including ordered factors)
#' @import rgdal
#' @param input The SpatialDataFrame
#' @param  destpath The output path
#' @param  outfilename The output filename
#' @export
#' @return Nothing (writes file to specified location)
#' @examples \dontrun{
#' output_geojson(myspatialdataframe, "out", "file.geojson")
#'
#'}
output_geojson <- function(input, destpath = "~/", outfilename = "myfile"){
  if (!grepl("\\.geojson$",outfilename)) {
    outfilename <- paste0(outfilename, ".geojson")
  }
  
  destpath <- path.expand(destpath)
  
  basefilename <- strsplit(outfilename,"\\.")[[1]][1]
  
  unlink(c(file.path(destpath, outfilename), 
           file.path(destpath, basefilename)))
  
  if (!file.exists(destpath)) dir.create(destpath)
  
  input@data <- data.frame(lapply(input@data, function(x) {
    if ("factor" %in% class(x)) {
      x <- as.character(x)
      message("Column is a factor, converting to character")
    }
    x
  }), stringsAsFactors = FALSE)
  
  dsn <- file.path(destpath, basefilename)
  writeOGR(input, dsn = dsn, layer = "", driver = "GeoJSON")
  
  dest <- file.path(destpath, outfilename)
  file.rename(dsn, dest)
  
  message("Success! File is at ", dest)
}

## Try to integrate it with https://github.com/ropensci/togeojson/blob/master/R/to_geojson.r and do a PR there
