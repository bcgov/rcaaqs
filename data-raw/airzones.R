library("sp")
library("rgdal")

bc_albers <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

unzip("data-raw/airzones.zip", exdir = "data-raw")

airzones <- readOGR(dsn = "data-raw/airzones", layer = "fixed-Polygon", stringsAsFactors = FALSE)

airzones <- spTransform(airzones, CRSobj = CRS(bc_albers))

devtools::use_data(airzones, pkg = devtools::as.package("."), overwrite = TRUE, compress = "xz")
