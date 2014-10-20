#'Find time interval in a date sequence
#'
#'Taken from package openair
#' @param dates vector of dates
#' @export
#' @keywords
#' @seealso
#' @return an integer reflecting the number of seconds in the time interval
#' @alias
#' @examples \dontrun{
#'
#'}
find_time_int <- function (dates) {
  dates <- unique(dates)
  len <- length(dates)
  len <- min(c(100, len))
  id <- which.max(table(diff(as.numeric(dates[order(dates[1:len])]))))
  seconds <- as.numeric(names(id))
  if ("POSIXt" %in% class(dates)) 
    seconds <- paste(seconds, "sec")
  if (class(dates)[1] == "Date") {
    seconds <- 3600 * 24
    seconds <- paste(seconds, "sec")
  }
  seconds
}
