# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#' Borders of British Columbia Airzones
#'
#' @format A SpatialPolgonsDataFrame with 7 polygons and 1 variable
#' \describe{
#' \item{Airzone}{The name of the airzone}
#' }
#' @source \url{http://catalogue.data.gov.bc.ca/dataset/british-columbia-air-zones}
"airzone_map"

#' CAAQS Management levels for air pollutaionts
#'
#' @format A data frame with 8 variables:
#' \describe{
#'   \item{parameter}{The name of the parameter}
#'   \item{labels}{Name of CAAQS Management Level}
#'   \item{lower_breaks}{Lower concentration in the category}
#'   \item{upper_breaks}{Upper concentration in the category}
#'   \item{units_html}{The units in html codes}
#'   \item{units_unicode}{The units in unicode}
#'   \item{val_labels_html}{Labels for the category values in html}
#'   \item{val_labels_unicode}{Labels for the category values in unicode}
#'   \item{colour}{The colour of the category (in hexadecimal code)}
#'   
#' }
"management_levels"

#' CAAQS Achievement levels for air pollutaionts
#'
#' @format A data frame with 8 variables:
#' \describe{
#'   \item{parameter}{The name of the parameter}
#'   \item{labels}{Achieved or Not Achieved}
#'   \item{lower_breaks}{Lower concentration in the category}
#'   \item{upper_breaks}{Upper concentration in the category}
#'   \item{units_html}{The units in html codes}
#'   \item{units_unicode}{The units in unicode}
#'   \item{val_labels_html}{Labels for the category values in html}
#'   \item{val_labels_unicode}{Labels for the category values in unicode}
#'   \item{colour}{The colour of the category (in hexadecimal code)}
#'   
#' }
"achievement_levels"
