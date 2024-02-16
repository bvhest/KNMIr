#' @title date to day-of-year conversion
#'
#' @description
#' \code{date2day_of_year} converts a date into an integer that represents the day-of-the-year.
#'
#' @details
#' Note: this function is a R-base implementation of the lubridate::yday function.
#'
#' @param datum the date that needs to be converted. Date class.
#' @return an integer representing the day-of-the-year.
#' @keywords date-conversion
#' @export
#' @examples
#' date2day_of_year(as.Date("2015-01-01")) # expected value: 1
#' date2day_of_year(as.Date("2015-12-31")) # expected value: 365
date2day_of_year <- function(datum) {
  d <- as.Date(datum)
  strptime(datum, "%d/%m/%Y")$yday + 1
}
