opts_rcaaqs <- list()
opts_rcaaqs$tz <- Sys.timezone()

#' Set options
#' 
#' @param ... options to set. In the form option_name = "option value". Valid
#'   option names are: \code{tz}.
#' @details Note that this method of setting options does no checking for valid
#'   option values. For setting the timezone (\code{tz}), it is better to use
#'   the shortcut function \link{set_tz}.
#'   
#' @return \code{TRUE} (invisibly)
#' @export
#' 
#' @examples
#' set_opts(tz = "Etc/GMT+8")
set_opts <- function(...) {
  dots <- list(...)
  lapply(names(dots), function(x) {
    if (!x %in% names(opts_rcaaqs)) stop(x, " is not a valid option")}
  )
  opts_rcaaqs[names(dots)] <<- dots
  invisible(TRUE)
}

#' Get Options
#'
#' @param name option name (currently only the timezone, \code{"tz"})
#'
#' @return a list of options specified (all if no arguments given)
#' @export
#'
#' @examples
#' get_opts()
#' get_opts("tz")
get_opts <- function(name = NULL) {
  if (is.null(name)) {
    return(opts_rcaaqs)
  } else {
    opts_rcaaqs[name]
  }
}

#' Set global timezone
#'
#' @param timezone A valid timezone to use in all date_time operations
#'
#' @return \code{TRUE} (invisibly)
#' @export
#'
#' @examples
#' set_tz("Etc/GMT+8")
set_tz <- function(timezone) {
  if (!timezone %in% OlsonNames()) stop(timezone, " is not a valid timezone. See OlsonNames()")
  set_opts(tz = timezone)
  invisible(TRUE)
}
