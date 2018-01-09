# Copyright 2017 Province of British Columbia
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

#' This function is defunct. Use \code{\link{format_caaqs_dt}}
#' 
#' @seealso \code{\link{format_caaqs_dt}}
#' 
#' @param dates defunct function. do not use
#' @param format defunct function. do not use 
#' @param lt defunct function. do not use
#'
#' @export
format_date <- function(dates, format, lt) {
  .Defunct(new = "format_caaqs_dt", 
           msg = paste("This function has been removed from the package and been", 
                       "replaced by format_caaqs_dt. format_caaqs_dt puts the datetime", 
                       "into the correct timezone in POSIXct, and automatically assigns", 
                       "readings with the previous hour by subtracting one second from the timestamp."))
}
