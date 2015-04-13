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

#'Calculate the airzone metric
#'
#'<full description>
#'@import dplyr
#'@import lazyeval
#'@param df The dataframe
#'@param n_years The column containing the number of years each 3yr avg is based
#'  on
#'@param az Airzone column
#'@param  val the column containing the the metric for individual stations
#'@param keep columns in the input df that you would like to retain in the
#'  output data frame. You can make it a named vector to rename the column in
#'  the output. Use the form \code{keep = c(new_name = "existing_name")}. This
#'  can also be used to rename any of the columns specified by n_years, az, or
#'  val.
#'@export
#'@return A dataframe with two columns: metric value and achievement status

airzone_metric <- function(df, n_years = "n_years", az = "Airzone", val, keep = NULL) {
  if (!is.data.frame(df)) stop("df must be a data frame")
  if (!n_years %in% names(df)) stop("n_years is not a column in ", df)
  if (!is.numeric(df[[n_years]])) stop("n_years must be numeric or integer")
  if (!val %in% names(df)) stop("val is not a column in ", df)
  if (!is.numeric(df[[val]])) stop("val must be numeric")

  # set val column to NA if n_years < 3
  df <- group_by_(df, az)
  df <- do_(df, ~parse_incomplete(., n_years, val))
  df <- slice_(df, interp(~which.max(x), x = as.name(val)))
  
  df <- df[,c(az, n_years, val, setdiff(keep, c(az, n_years, val)))]
  
  # Rename keep columns if asked to
  if (!is.null(names(keep))) {
    for (k in keep) {
      n <- names(keep)[keep == k]
      if (nchar(n) > 0) {
        names(df)[names(df) == k] <- n
      }
    }
  }
  df
}

parse_incomplete <- function(df, n_years, val) {
  if (all(df[[n_years]] < 3)) {
    df <- df
  } else {
    df[df[[n_years]] < 3, val] <- NA
  }
  df
}
