#' convert a 'day of the year'-value into a date.
#'
#' @param doy the day of the year that needs to be converted. Integer.
#' @param year the year for which the day of the year is applicable. Integer. Default is the current year.
#' @return the date corresponding to the day-of-the-year.
#' @keywords date-conversion
#' @export
#' @examples
#' dayOfYear2Date(1) # '01-01-201x'
#' dayOfYear2Date(1, year=2010) # '01-01-2010'
#' dayOfYear2Date(0) # 'NA'

dayOfYear2Date <- function(doy, year = format(Sys.Date(), format="%Y")){
   if (as.integer(doy) < 1 || as.integer(doy) > 366)
      return(NA)

   x <- unique(as.numeric(doy))
   as.Date(x - 1, origin = paste(year, "01-01", sep="-"))
}
