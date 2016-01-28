#' Flag Execeptional Events or Transboundary Flows affecting air quality
#'
#' @param daily_df data frame of daily air quality values
#' @param id_col column in daily_df that contains the id of air quality monitors
#' @param date_col "date" column in daily_df
#' @param eetf a data frame with at least two columns: \code{id} and \code{date}
#'             that define the monitor IDs and dates for which EEs or TFs have 
#'             been identified. The \code{date} column should be of class \code{Date}, 
#'             \code{POSIXt}, or character in the form \code{"YYYY-MM-DD"}.
#' @return data frame: daily_df with an additional column of logical values indicating EE/TFs.
#' @export
#'
#' @examples
eetf <- function(daily_df, id_col = "ems_id", date_col = "date", eetf) {
  if (!is(daily_df, "data.frame")) {
    stop("daily_df must be a data frame.")
  }
  if (!all(c(id_col, date_col) %in% names(daily_df))) {
    stop("id_col and date_col must both be columns in daily_df.")
  }
  if (!is(eetf, "data.frame") || !all(c("id", "date") %in% names(eetf))) {
    stop("eetf must be a data frame containing 'id' and 'date'")
  }
  if (is.character(eetf$date)) {
    if (!all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", eetf$date))) {
      stop("'date' column in eetf contains malformed dates")
    }
  } else {
    if (!is(eetf$date, "Date") && !is(eetf$date, "POSIXt")) {
      stop("'date' column in eetf must be of class Date, POSIXct, POSIXlt, or character in the form YYYY-MM-DD")
    }
  }
  
  eetf$eetf <- TRUE
  
  join_vec <- c("id", "date")
  join_vec <- setNames(join_vec, c(id_col, date_col))
  
  ret <- dplyr::left_join(daily_df, eetf, by = join_vec)
  ret[is.na(ret$eetf), "eetf"] <- FALSE
  ret
  
}
