library("sp")
library("rgdal")

bc_albers <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

airzones_databc <- "http://catalogue.data.gov.bc.ca/dataset/e8eeefc4-2826-47bc-8430-85703d328516/resource/4cea4b22-36a3-4ea7-9346-7e872e747076/download/bcairzonesalbersshp.zip"
airzones_zip <- "data-raw/airzones.zip"

download.file(airzones_databc, airzones_zip)

unzip(airzones_zip, exdir = "data-raw/airzones")

airzone_map <- readOGR(dsn = "data-raw/airzones", layer = "bc_air_zones", stringsAsFactors = FALSE)

airzone_map <- spTransform(airzone_map, CRSobj = CRS(bc_albers))

devtools::use_data(airzone_map, pkg = devtools::as.package("."), overwrite = TRUE, compress = "xz")
