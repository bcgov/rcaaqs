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

#' Find time interval of a vector of regularly spaced dates/datetimes
#'
#' @param x vector of dates/datetimes
#'
#' @return character
test_time_interval <- function(x) {
  len <- min(100, length(x))
  test_x <- sort(x[1:len])
  m_diff <- Mode(diff(as.numeric(test_x)))
  if (inherits(x, "Date")) m_diff <- m_diff * 3600 * 24
  m_diff
}


#' Find the mode of a vector of numbers
#'
#' @param x vector of numbers
#'
#' @return vector of length 1
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
