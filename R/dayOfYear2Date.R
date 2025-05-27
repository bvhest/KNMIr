#' @title day-of-year to date  conversion
#'
#' @description
#' \code{day_of_year2date} converts a 'day-of-the-year'-value into a date.
#'
#' @param doy the day-of-the-year that needs to be converted. Integer.
#' @param year the year for which the day-of-the-year is applicable. Integer. Default is the current year.
#' @return the date corresponding to the day-of-the-year.
#' @keywords date-conversion
#' @export
#' @examples
#' day_of_year2date(1) # '01-01-2017', when executed in the year 2017.
#' day_of_year2date(1, year = 2010) # '01-01-2010'
day_of_year2date <- function(doy, year = format(Sys.Date(), format = "%Y")) {
  if (as.integer(doy) < 1 || as.integer(doy) > 366) {
    stop("The day-of-the-year does not seem to be a valid value.")
  }

  x <- unique(as.numeric(doy))
  as.Date(x - 1, origin = paste(year, "01-01", sep = "-"))
}

