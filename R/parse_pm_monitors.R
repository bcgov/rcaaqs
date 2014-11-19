#'Parse multiple columns to extract a simplified PM2.5 monitor type from them.
#'
#'<full description>
#' @param ... Columns to parse, supplied in decreasing order of reliability
#' @export
#' @return a character vector with simplified PM2.5 monitor names
parse_pm_monitors <- function(...) {
  cols <- list(...)
  
  # Create a character matrix of simplified monitor names from each column supplied
  types <- sapply(cols, pm_monitor_type)

  n_col <- ncol(types)

  # Create an initial vector of simplified monitor names based on first (and most reliable) column
  monitor <- types[,1]
  
  # If there was only one column supplied, use that
  if (n_col == 1) return(monitor)

  # Loop across columns of types and update monitor with less reliable data only if monitor is unknown
  for (i in 2:n_col) {
    monitor <- ifelse(is.na(monitor), types[,i], monitor)
  }
  
  monitor
  
}


#' Test whether a given record is from a certain PM monitor by searching across columns that might encode the information
#'
#' Internal function called by parse_pm_monitors
#' @param ... vector (probably column in a data frame) that may have monitor type information in it
#' @keywords internal
#' @return character vector
pm_monitor_type <- function(col) {
  ifelse(grepl("grimm", col, ignore.case = TRUE), "GRIMM", 
         ifelse(grepl("fdms", col, ignore.case = TRUE), "FDMS", 
                ifelse(grepl("teom", col, ignore.case = TRUE), "TEOM", 
                       ifelse(col == "PM25", NA, 
                              "FEM"))))
  
}
