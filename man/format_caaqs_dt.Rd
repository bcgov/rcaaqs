% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dates.R
\name{format_caaqs_dt}
\alias{format_caaqs_dt}
\title{Format date-times in raw air quality data.}
\usage{
format_caaqs_dt(
  x,
  format = "\%Y-\%m-\%d \%H:\%M:\%S",
  prev_hour = TRUE,
  tz = getOption("rcaaqs.timezone", default = "Etc/GMT+8")
)
}
\arguments{
\item{x}{Vector of date-times as character or POSIXlt/ct.}

\item{format}{The format of the character dates}

\item{prev_hour}{Should the timestamp be assigned to the previous hour as
dictated by the CAAQS guidance document? Default \code{TRUE}. This is
accomplished by subtracting one second off the times.}

\item{tz}{The timezone of the date-times. See Details below.}
}
\value{
POSIXct vector
}
\description{
Intended for use with hourly air quality readings. Ensures that the
date/times are in class POSIXct in the correct timezone, and ensures that the
timestamp is associated with the previous hour as dictated in caaqs guidance
manual.
}
\details{
You can set the timezone that you are working with in two different
ways. You can set it globally with: \code{options("rcaaqs.timezone" = "your_timezone")} or set it each time you call the function by setting the
\code{tz} argument to "your_timezone" where "your_timezone" is a valid
timezone - see \code{\link[=OlsonNames]{OlsonNames()}}. The default (when
\code{options("rcaaqs.timezone")} is unset) is \code{"Etc/GMT+8"}, which is
equivalent to Pacific Standard Time, but does not use Daylight Savings
(i.e., is GMT+8 all year). This is the standard for Air Quality Monitoring
in British Columbia.
}
