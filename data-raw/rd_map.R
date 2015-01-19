library("sp")
library("rgdal")
library("magrittr")

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

# Make southern boundary 
n_rockies_s_bound <- peace_coords[nrow(peace_coords):(nrow(peace_coords)-(rev(peace_lat_rle[[1]])[1] - 1)), ]

## Make eastern boundary
e_long_peace <- peace_long_rle[[2]][1]
n_lat_peace <- peace_lat_rle[[2]][length(peace_lat_rle[[2]])]
n_rockies_e_bound <- matrix(c(rep(e_long_peace, 50), 
                              seq(n_lat_stikine, n_lat_peace, length.out = 50))
                            , ncol = 2)

## Make western boundary
stikine_ne_bound <- stikine_coords[stikine_coords[,1] > -128 & 
                                     stikine_coords[,2] > n_lat_peace,]

end_of_n_bound <- rle(round(stikine_ne_bound[,2], 3))[[1]][1]
n_rockies_w_bound <- stikine_ne_bound[end_of_n_bound:nrow(stikine_ne_bound),]

# Reverse direction (clockwise)
n_rockies_w_bound <- apply(n_rockies_w_bound, 2, rev)

## Get westernmost flat point on the southern N Rockies border
w_south_n_rockies_flat <- n_rockies_s_bound[nrow(n_rockies_s_bound), , drop  = FALSE]

## find the point on the stikine eastern border closest to the westernmost point
## on the flat sourthern N Rockies border
long_int <- which.min(abs(n_rockies_w_bound[,1] - w_south_n_rockies_flat[,1]))
lat_int <- which.min(abs(n_rockies_w_bound[,2] - w_south_n_rockies_flat[,2]))

if (long_int != lat_int) cat("Long and Lat indexes for the intesection of peace and stikine are not the same!")

n_rockies_w_bound <- n_rockies_w_bound[-c(1:lat_int),]

## Make northern boundary
n_rockies_n_bound <- matrix(c(seq(n_rockies_w_bound[nrow(n_rockies_w_bound), 1], 
                                  e_long_peace, length.out = 60), 
                              rep(n_lat_stikine, 60)), ncol = 2)

## Combine boundaries into one
n_rockies_bound <- rbind(n_rockies_n_bound, n_rockies_e_bound, 
                         n_rockies_s_bound, n_rockies_w_bound)

## Convert points -> Polygons -> SpatialPolygons -> SpatialPolygonsDataFrame
n_rockies_spPolyDF <- Polygon(n_rockies_bound, hole = FALSE) %>% 
  list() %>% 
  Polygons(ID = "28") %>%
  list() %>% 
  SpatialPolygons(proj4string = CRS(proj4string(rd_map))) %>% 
  SpatialPolygonsDataFrame(data = data.frame(unit_name = "Northern Rockies", 
                                             row.names = 28,
                                             stringsAsFactors = FALSE))

## Combine Northern Rockies SpatialPolygonsDataFrame with rd_map
rd_map <- rbind(rd_map, n_rockies_spPolyDF)

## Differentiate RD types
rd_map@data$region_type <- ifelse(rd_map@data$unit_name == "Northern Rockies", 
                                  "Regional Municipality", 
                                  ifelse(rd_map@data$unit_name == "Stikine", 
                                         "Unincorporated Area", "Regional District"))

## Convert back to BC Albers
rd_map <- spTransform(rd_map, CRS(bc_albers))

## Plot to verify
# library("ggplot2")
# 
# rd_gg <- fortify(rd_map, region = "unit_name")
# 
# ggplot(data = rd_gg, aes(x = long, y = lat, group = group)) + 
#   geom_polygon(aes(fill = id)) + 
#   geom_path(colour = "white")

