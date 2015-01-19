airzones <- readOGR(dsn = "data-raw/airzones", layer = "fixed-Polygon", stringsAsFactors = FALSE)

airzones <- spTransform(airzones, CRSobj = CRS(bc_albers))

use_data(airzones, pkg = as.package("."), overwrite = TRUE, compress = "xz")
