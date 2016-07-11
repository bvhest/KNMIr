#' convert a date into an integer that represents the day of the year.
#'
#' @param datum the date that needs to be converted. Date class.
#' @return an integer representing the day-of-the-year.
#' @keywords date-conversion
#' @export
#' @examples
#'
#' date2DayOfYear(as.Date("2015-01-01")) # expected value: 1
#' date2DayOfYear(as.Date("2015-12-31")) # expected value: 365

date2DayOfYear <- function(datum){
   d <- as.Date(datum)
   strptime(datum, "%d/%m/%Y")$yday+1
}
