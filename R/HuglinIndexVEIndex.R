#' this helper-function adds [growing degree days](https://en.wikipedia.org/wiki/Growing_degree-day), either the Huglin-index or the VE-index, to the provided data-frame. See calculateHuglinIndex or calculateVEIndex for more details.
#'
#' @param data      dataframe containing two mandatory columns; the day-of-the-year (dagVjaar), the daily mean temperature (gemTemp) and an optional column containing the year (jaar). Temperatures in degrees Celcius. The data frame can contain the ranges for multiple years.
#' @param dggType   type of degree days that will be added. Two options: 'Huglin' or 'VE'. Default is 'Huglin'.
#' @param startDate the start of the summation interval, formatted as a string 'yyyy-mm-dd'. Defaults to the start of the year.
#' @param endDate   the end of the summation interval, formatted as a string 'yyyy-mm-dd'. Defaults to the end of the year.
#'
#' @return dataframe containing the year (jaar), the day-of-the-year (dagVjaar) and the VE-index at the dagVjaar.
#'
#' @keywords value aggregation
#'
#' @export
#'
add_degree_days <- function(data,
                          dggType = "Huglin",
                          startDate = paste(format(Sys.Date(), format="%Y"), "01-01", sep="-"),
                          endDate = paste(format(Sys.Date(), format="%Y"), "12-31", sep="-")) {

   if (indexType == "Huglin") {
      # bereken Huglin-index:
      HUI <- calculateHuglinIndex(data, startDate = startDate, endDate = endDate)
      # voeg berekende waarden toe aan originele data-frame:
      merge(data, HUI, by=c("stationID", "jaar", "dagVjaar"), all.x = TRUE)

   } else if (indexType == "VE") {
      # bereken VE-index:
      VEI <- calculateVEIndex(data, startDate = startDate, endDate = endDate)
      # voeg berekende waarden toe aan originele data-frame:
      merge(data, VEI, by=c("stationID", "jaar", "dagVjaar"), all.x = TRUE)
   } else {
      data
   }

}

#' This function calculates the [Huglin index](http://www.brabantsewijnbouwers.nl/index.php?section=13&page=57&student=1171),
#' which is one of the statistics that represents the total
#' amount of heat in a season that is bennefical to the growth of plants. More specific, its used to
#' measure the amount of warmth that is required by grapes to start growing, blosson and ripen.
#'
#' It is calculated as the cumulitive summation over the days of the year of the mean of the daily
#' averagee temperature (in degrees Celcius) plus the daily maximum temperature, substracted with 10
#' (the temperature at wihch grapes start growing). This value is only added to the total when its
#' positive. The value is multiplied by a correction for the latitude of the measurement location
#' (1.06 for The Netherlands).
#' The default summation interval, from the first of April until the end of September, has been altered
#' to be able to deal with cool-climate winegrowing that requires a prolonged growing season.
#'
#' Documentation: https://de.wikipedia.org/wiki/Huglin-Index
#'
#' @param dgg dotaframe containing three mandatory columns; the day-of-the-year (dagVjaar), the daily mean temperature (gemTemp), the daily maximum temperature (maxTemp) and an optional column containing the year (jaar). Temperatures in degrees Celcius. The data frame can contain the ranges for multiple years.
#' @param startDate the start of the summation interval, formatted as a string 'yyyy-mm-dd'. Defaults to the start of the year.
#' @param endDate the end of the summation interval, formatted as a string 'yyyy-mm-dd'. Defaults to the end of the year.
#' @return dataframe containing the year (jaar), the day-of-the-year (dagVjaar) and the Huglin-index at the dagVjaar.
#' @keywords date-conversion
#' @export

calculate_Huglin_index <- function(dgg,
                                   startDate = paste(format(Sys.Date(), format="%Y"), "01-01", sep="-"),
                                   endDate = paste(format(Sys.Date(), format="%Y"), "12-31", sep="-")) {
   attach(dgg)

   # convert to day-of-year:
   doyStart <- yday(startDate)
   doyEnd <- yday(endDate)

   # subset op basis van de start en eind datum:
   range <- dgg[dagVjaar>=doyStart && dagVjaar<=doyEnd,]

   # wanneer afwezig; voeg de jaar-kolom toe:
   if (!("jaar" %in% names(dgg))) {
      range$jaar <- format(Sys.Date(), format="%Y")
   }

   # bereken de dagwaarde:
   range$HuglinIndex <- 1.06*pmax( (((range$gemTemp+range$maxTemp)/2)-10), 0)
   # bereken nu de cumulatieve HuglinIndex:
   range <- ddply(range, .(stationID, jaar), transform, somHuglinIndex = cumsum(HuglinIndex))

   detach(dgg)
   range[,c("stationID", "jaar", "dagVjaar", "somHuglinIndex")]
}

#' This function calculates the [VE index](http://www.brabantsewijnbouwers.nl/index.php?section=13&page=57&student=1171),
#' which is one of the statistics that represents the total
#' amount of heat in a season that is bennefical to the growth of plants. More specific, its used to
#' measure the amount of warmth that is required by grapes to start growing, blosson and ripen.
#'
#' It is calculated as the cumulitive summation over the days of the year of the mean of the daily
#' minimum temperature (in degrees Celcius) substracted with 10 (the temperature at wihch grapes
#' start growing). This value is only added to the total when its positive.
#' The default summation interval, from the first of April until the end of September, has been altered
#' to be able to deal with cool-climate winegrowing that requires a prolonged growing season.
#'
#' Documentation: https://de.wikipedia.org/wiki/Wachstumsgradtag
#'
#' @param dgg dotaframe containing two mandatory columns; the day-of-the-year (dagVjaar), the daily mean temperature (gemTemp) and an optional column containing the year (jaar). Temperatures in degrees Celcius. The data frame can contain the ranges for multiple years.
#' @param startDate the start of the summation interval, formatted as a string 'yyyy-mm-dd'. Defaults to the start of the year.
#' @param endDate the end of the summation interval, formatted as a string 'yyyy-mm-dd'. Defaults to the end of the year.
#' @return dataframe containing the year (jaar), the day-of-the-year (dagVjaar) and the VE-index at the dagVjaar.
#' @keywords value aggregation
#' @export

calculate_VE_index <- function(dgg,
                               startDate = paste(format(Sys.Date(), format="%Y"), "01-01", sep="-"),
                               endDate = paste(format(Sys.Date(), format="%Y"), "12-31", sep="-")) {

   attach(dgg)

   # convert to day-of-year:
   doyStart <- yday(startDate)
   doyEnd <- yday(endDate)

   # subset op basis van de start en eind datum:
   range <- dgg[dagVjaar>=doyStart && dagVjaar<=doyEnd,]
   # wanneer afwezig; voeg de jaar-kolom toe:
   if (!("jaar" %in% names(dgg))) {
      range$jaar <- format(Sys.Date(), format="%Y")
   }

   # bereken de dagwaarde:
   range$VEIndex <- pmax((range$gemTemp-10), 0)
   # bereken nu de cumulatieve HuglinIndex:
   range <- ddply(range, .(stationID, jaar), transform, somVEIndex = cumsum(VEIndex))

   detach(dgg)
   range[,c("stationID", "jaar", "dagVjaar", "somVEIndex")]
}
