library("sp")
library("rgdal")

bc_albers <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

unzip("data-raw/airzones.zip", exdir = "data-raw")

airzone_map <- readOGR(dsn = "data-raw/airzones", layer = "fixed-Polygon", stringsAsFactors = FALSE)

airzone_name_lu <- data.frame(FID = c(0,1,2,3,4,5,6), 
                              Airzone = c("Northeast", "Northwest", 
                                          "Southern Interior", 
                                          "Lower Fraser Valley", 
                                          "Central Interior", 
                                          "Coastal", "Georgia Strait"), 
                              stringsAsFactors = FALSE)

airzone_map <- merge(airzone_map, airzone_name_lu, by = "FID")

airzone_map <- spTransform(airzone_map, CRSobj = CRS(bc_albers))

devtools::use_data(airzone_map, pkg = devtools::as.package("."), overwrite = TRUE, compress = "xz")
