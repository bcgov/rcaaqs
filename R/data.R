#' Borders of British Columbia Regional Districts
#' 
#' Borders were taken from the Community Energy Emissions Inventory shapefile 
#' available from the source below. Northern Rockies Regional Municipality was 
#' added, as it was not available in the original source file.
#'
#' @format A SpatialPolgonsDataFrame with 29 polygons and 2 variables:
#' \describe{
#' \item{region_name}{The name of the region}
#' \item{region_type}{The type of region (Regional District, Regional Municipality, or Unincorporated Area)}
#' }
#' @source \url{http://catalogue.data.gov.bc.ca/dataset/ceei-primary-indicators-total-2007-regional-districts}
"rd_map"

#' Borders of British Columbia Airzones
#'
#' @format A SpatialPolgonsDataFrame with 7 polygons and 1 variable
#' \describe{
#' \item{Airzone}{The name of the airzone}
#' }
"airzone_map"

#' CAAQS management levels for air pollutaionts
#'
#' @format A data frame with 8 variables:
#' \describe{
#'   \item{parameter}{The name of the parameter}
#'   \item{labels}{Name of CAAQS Management Level}
#'   \item{lower_breaks}{Lower concentration in the category}
#'   \item{upper_breaks}{Upperer concentration in the category}
#'   \item{units_html}{The units in html codes}
#'   \item{units_unicode}{The units in unicode}
#'   \item{val_labels_html}{Labels for the category values in html}
#'   \item{val_labels_unicode}{Labels for the category values in unicode}
#'   \item{colour}{The colour of the category (in hexadecimal code)}
#'   
#' }
"caaqs_levels"
