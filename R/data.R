# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

#' CAAQS Management levels for air pollutaionts
#'
#' @format A data frame with 8 variables:
#' \describe{
#'   \item{parameter}{The name of the parameter}
#'   \item{labels}{Name of CAAQS Management Level}
#'   \item{lower_breaks}{Lower concentration in the category}
#'   \item{upper_breaks}{Upper concentration in the category}
#'   \item{units}{Units in plain ascii}
#'   \item{units_html}{The units in html codes}
#'   \item{units_unicode}{The units in unicode}
#'   \item{val_labels}{Labels for the category in plain ascii}
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
#'   \item{units}{Units in plain ascii}
#'   \item{units_html}{The units in html codes}
#'   \item{units_unicode}{The units in unicode}
#'   \item{val_labels}{Labels for the category in plain ASCII}
#'   \item{val_labels_html}{Labels for the category values in html}
#'   \item{val_labels_unicode}{Labels for the category values in unicode}
#'   \item{colour}{The colour of the category (in hexadecimal code)}
#'   
#' }
"achievement_levels"

#' Sample hourly PM2.5 data for 3 years from 10 stations
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{ems_id}{id of the monitoring station}
#'   \item{date_time}{date/time (POSIXct) in ETC/GMT+8 timezone}
#'   \item{site}{name of the monitoring station}
#'   \item{year}{year}
#'   \item{parameter}{name of the parameter ("PM2.5")}
#'   \item{value}{PM2.5 hourly value (in ppb)}
#' }
"pm25_sample_data"

#' Sample hourly ozone data for 3 years for 4 stations 
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{ems_id}{id of the monitoring station}
#'   \item{date_time}{date/time (POSIXct) in ETC/GMT+8 timezone}
#'   \item{site}{name of the monitoring station}
#'   \item{year}{year}
#'   \item{month}{month}
#'   \item{parameter}{name of the parameter ("O3")}
#'   \item{value}{ozone hourly value}
#'   \item{units}{the units of value -- always 'ppb'}
#' }
"o3_sample_data"

#' Sample hourly NO2 data for 3 years for 4 stations 
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{ems_id}{id of the monitoring station}
#'   \item{date_time}{date/time (POSIXct) in ETC/GMT+8 timezone}
#'   \item{site}{name of the monitoring station}
#'   \item{year}{year}
#'   \item{month}{month}
#'   \item{parameter}{name of the parameter ("NO2")}
#'   \item{value}{ozone hourly value}
#'   \item{units}{the units of value -- always 'ppb'}
#' }
"no2_sample_data"

#' Sample hourly SO2 data for 3 years for 4 stations 
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{ems_id}{id of the monitoring station}
#'   \item{date_time}{date/time (POSIXct) in ETC/GMT+8 timezone}
#'   \item{site}{name of the monitoring station}
#'   \item{year}{year}
#'   \item{month}{month}
#'   \item{parameter}{name of the parameter ("SO2")}
#'   \item{value}{ozone hourly value}
#'   \item{units}{the units of value -- always 'ppb'}
#' }
"so2_sample_data"
