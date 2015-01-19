library("sp")
library("rgdal")

bc_albers <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
nad_83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

unzip("data-raw/airzones.zip", exdir = "data-raw")

unzip("data-raw/regional_districts_ceei.zip", exdir = "data-raw/rd")
