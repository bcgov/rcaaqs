library("sp")
library("rgdal")
# library("ggplot2")

bc_albers <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
nad_83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

unzip("data-raw/regional_districts_ceei.zip", exdir = "data-raw/rd")

rd_map <- readOGR(dsn = "data-raw/rd/C_PIT_07RD", layer = "C_PIT_07RD_polygon", stringsAsFactors = FALSE)
rd_map <- rd_map["UNIT_NAME"]
names(rd_map) <- tolower(names(rd_map))

# Convert to NAD83 to make lat lines horizontal and long lines vertical
rd_map <- spTransform(rd_map, CRS(nad_83))

stikine <- subset(rd_map, unit_name == "Stikine")
peace <- subset(rd_map, unit_name == "Peace River")

stikine_coords <- stikine@polygons[[1]]@Polygons[[1]]@coords

n_lat_stikine <- rle(round(stikine_coords[,2], 3))[[2]][1]

peace_coords <- peace@polygons[[1]]@Polygons[[1]]@coords

peace_long_rle <- rle(round(peace_coords[,1], 3))
peace_lat_rle <- rle(round(peace_coords[,2], 3))

e_long_peace <- peace_long_rle[[2]][1]
n_lat_peace <- peace_lat_rle[[2]][length(peace_lat_rle[[2]])]

n_rockies_corner_ne <- matrix(c(e_long_peace, n_lat_stikine), nrow = 1)
n_rockies_corner_se <- matrix(c(e_long_peace, n_lat_peace), nrow = 1)

stikine_ne_bound <- stikine_coords[stikine_coords[,1] > -128 & 
                                     stikine_coords[,2] > n_lat_peace,]

end_of_n_bound <- rle(round(stikine_ne_bound[,2], 3))[[1]][1]
n_rockies_w_bound <- stikine_ne_bound[end_of_n_bound:nrow(stikine_ne_bound),]
n_rockies_w_bound <- apply(n_rockies_w_bound, 2, rev)

## Find the westernmost flat point on the northern peace border
flat_peace_west_i <- sum(peace_lat_rle$lengths[1:(length(peace_lat_rle$lengths) - 1)]) + 1
flat_peace_west <- peace_coords[flat_peace_west_i, , drop = FALSE]

long_int <- abs(n_rockies_w_bound[,1] - flat_peace_west[,1]))
lat_int <- which.min(abs(n_rockies_w_bound[,2] - flat_peace_west[,2]))

if (long_int != lat_int) cat("Long and Lat indexes for the intesection of peace and stikine are not the same!")

n_rockies_w_bound <- n_rockies_w_bound[-c(1:lat_int),]

n_rockies_bound <- rbind(n_rockies_corner_ne, n_rockies_corner_se, n_rockies_w_bound)

n_rockies_poly <- Polygons(list(Polygon(n_rockies_bound, hole = FALSE)), ID = "28")

n_rockies_spPoly <- SpatialPolygons(list(n_rockies_poly), proj4string = CRS(proj4string(rd_map)))

n_rockies_spPolyDF <- SpatialPolygonsDataFrame(n_rockies_spPoly, 
                                               data.frame(unit_name = "Northern Rockies", 
                                                          row.names = 28,
                                                          stringsAsFactors = FALSE))

rd_map <- rbind(rd_map, n_rockies_spPolyDF)

## Convert back to BC Albers
rd_map <- spTransform(rd_map, CRS(bc_albers))

## Differentiate RD types
rd_map@data$region_type <- ifelse(rd_map@data$unit_name == "Northern Rockies", 
                                  "Regional Municipality", 
                                  ifelse(rd_map@data$unit_name == "Stikine", 
                                         "Unincorporated Area", "Regional District"))

newgg <- fortify(rd_map, region = "unit_name")

ggplot(data = newgg, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = group)) + 
  geom_path(colour = "white")
