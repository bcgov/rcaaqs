#'Replaces negative values in a vector of air quality readings with
#'CAAQS-compliant values
#'
#'@param x vector of air quality readings
#'@param  type type of air quality readings; one of: "pm25", "ozone", "so2",
#'  "nox"
#'@export
#'@return a vector of the same length as x with negative values replaced with
#'  CAAQS compliant values
#'@details For gases (type = "ozone", "so2", or "nox"), replaces values
#'  \eqn{\le} -5 with \code{NA} and values between -5 and 0 with 0. For type =
#'  "pm25", replaces values \eqn{\le} -3.5 with \code{NA} and values between
#'  -3.5 and 0 with 0.
#' @examples \dontrun{
#'
#'}
clean_neg <- function(x, type = c("pm25", "ozone", "so2", "nox")) {
  type <- tolower(type)
  type <- match.arg(type)
  cutoff <- switch(type, 
                 nox = -5, so2 = -5, ozone = -5, pm25 = -3.5)
  x[x <= cutoff] <- NA
  x[x < 0] <- 0
  x
}
