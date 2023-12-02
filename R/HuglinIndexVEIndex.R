#' @title add degree-day data.
#'
#' @description
#' \code{add_degree_days} adds a column with the degree days based on either the Huglin or VE calculation.
#'
#' @details
#' this helper-function adds growing degree days (see \href{https://en.wikipedia.org/wiki/Growing_degree-day}{growing degree days on Wikipedia}), either the
#' Huglin-index or the VE-index, to the provided data-frame. See the functions \code{\link{calculate_Huglin_index}} or
#' \code{\link{calculate_VE_index}} for more details.
#'
#' NOTE: This function only works correctly when the column names of the data-frame have been translated with the
#'   function 'rename_columns_KNMI_data()'.
#'
#' @param data      dataframe containing four mandatory columns; the stationID, doy (day-of-the-year) and gemTemp (the daily mean temperature) and maxTemp (the daily maximum temperature) and an optional column containing the year (jaar). Temperatures in degrees Celcius. The data frame can contain the ranges for multiple years.
#' @param dggType   type of degree days that will be added. Two options: 'Huglin' or 'VE'. Default is 'Huglin'.
#' @param startDate the start of the summation interval, formatted as a string 'yyyy-mm-dd'. Defaults to the start of the year.
#' @param endDate   the end of the summation interval, formatted as a string 'yyyy-mm-dd'. Defaults to the end of the year.
#' @return the original dataframe with the cumulative Huglin-index or the VE-index added for each day-of-the-year
#' @keywords value aggregation
#' @import data.table
#' @export
#'
add_degree_days <- function(data,
                            dggType = "Huglin",
                            startDate = paste(format(Sys.Date(), format="%Y"), "01-01", sep="-"),
                            endDate = paste(format(Sys.Date(), format="%Y"), "12-31", sep="-")) {

   # stop deze functie als de kolommen nog niet zijn hernoemd.
   if (!("stationID" %in% names(data)))
      stop("This function can only be applied after the data-frame columns have been renamed with the function 'rename_columns_KNMI_data()'.")
   if (!("stationID" %in% names(data) & "doy" %in% names(data) & "gemTemp" %in% names(data)))
      stop("This function needs three mandatory columns; stationID, doy (day-of-the-year) and gemTemp (the daily mean temperature).")

   # wanneer afwezig; voeg de jaar-kolom toe:
   if (!("jaar" %in% names(data))) {
      data$jaar <- format(Sys.Date(), format="%Y")
   }

   if (dggType == "Huglin") {
      # bereken Huglin-index:
      HUI <- calculate_Huglin_index(data, startDate = startDate, endDate = endDate)
      # voeg berekende waarden toe aan originele data-frame:
      data <- merge(data, HUI, by = c("stationID", "jaar", "doy"), all.x = TRUE)
   } else if (dggType == "VE") {
      # bereken VE-index:
      VEI <- calculate_VE_index(data, startDate = startDate, endDate = endDate)
      # voeg berekende waarden toe aan originele data-frame:
      data <- merge(data, VEI, by = c("stationID", "year", "doy"), all.x = TRUE)
   }
   return(data)
}

#' @title calculate the Huglin-index.
#'
#' @description
#' \code{calculate_Huglin_index} calculates the degree days according to the Huglin-algorithm.
#'
#' @details
#' This function calculates the Huglin index (see \href{http://www.brabantsewijnbouwers.nl/index.php?section=13&page=57&student=1171}{Klimaat en de Druivelaar}
#' on the website of the Brabantse Wijnbouwers (in Dutch)).
#' The Huglin index is one of the statistics that represents the total
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
#' NOTE: This function only works correctly when the column names of the data-frame have been translated with the
#'   function 'rename_columns_KNMI_data()'.
#'
#' Documentation: \url{https://de.wikipedia.org/wiki/Huglin-Index}.
#'
#' @param dgg dotaframe containing three mandatory columns; the day-of-the-year (doy), the daily mean temperature (gemTemp), the daily maximum temperature (maxTemp) and an optional column containing the year (jaar). Temperatures in degrees Celcius. The data frame can contain the ranges for multiple years.
#' @param startDate the start of the summation interval, formatted as a string 'yyyy-mm-dd'. Defaults to the start of the year.
#' @param endDate the end of the summation interval, formatted as a string 'yyyy-mm-dd'. Defaults to the end of the year.
#' @return dataframe containing the year, the day-of-the-year (doy) and the Huglin-index for each day-of-the-year.
#' @keywords date-conversion
#' @import plyr
#' @export

calculate_Huglin_index <- function(dgg,
                                   startDate = paste(format(Sys.Date(), format="%Y"), "01-01", sep="-"),
                                   endDate = paste(format(Sys.Date(), format="%Y"), "12-31", sep="-")) {

   # stop deze functie als de kolommen nog niet zijn hernoemd.
   if (!("stationID" %in% names(dgg)))
      stop("This function can only be applied after the data-frame columns have been renamed with the function 'rename_columns_KNMI_data()'.")

#   attach(dgg); on.exit(detach(name = dgg))

   # convert to day-of-year:
   doyStart <- lubridate::yday(startDate)
   doyEnd <- lubridate:::yday(endDate)

   # subset op basis van de start en eind datum:
   range <- dgg[dgg$doy >= doyStart & dgg$doy <= doyEnd,]

   # bereken de dagwaarde:
   range$HuglinIndex <- 1.06 * pmax((((range$gemTemp + range$maxTemp) / 2) - 10), 0)

   # bereken nu de cumulatieve HuglinIndex:
   range <- plyr::ddply(range, .(stationID, jaar), transform, somHuglinIndex = cumsum(HuglinIndex))

#   detach(name = dgg)
   return(range[,c("stationID", "jaar", "doy", "somHuglinIndex")])
}

#' @title calculate the VE-index.
#'
#' @description
#' \code{calculate_VE_index} calculates the degree days according to the VE-algorithm.
#'
#' @details
#' This function calculates the VE index (see \href{http://www.brabantsewijnbouwers.nl/index.php?section=13&page=57&student=1171}{Klimaat en de Druivelaar}
#' on the website of the Brabantse Wijnbouwers (in Dutch)).
#' The VE index is one of the statistics that represents the total
#' amount of heat in a season that is bennefical to the growth of plants. More specific, its used to
#' measure the amount of warmth that is required by grapes to start growing, blosson and ripen.
#'
#' It is calculated as the cumulitive summation over the days of the year of the mean of the daily
#' minimum temperature (in degrees Celcius) substracted with 10 (the temperature at wihch grapes
#' start growing). This value is only added to the total when its positive.
#' The default summation interval, from the first of April until the end of September, has been altered
#' to be able to deal with cool-climate winegrowing that requires a prolonged growing season.
#'
#' NOTE: This function only works correctly when the column names of the data-frame have been translated with the
#'   function 'rename_columns_KNMI_data()'.
#'
#' Documentation: \url{https://de.wikipedia.org/wiki/Wachstumsgradtag}.
#'
#' @param dgg dotaframe containing two mandatory columns; the day-of-the-year (doy), the daily mean temperature (gemTemp) and an optional column containing the year (jaar). Temperatures in degrees Celcius. The data frame can contain the ranges for multiple years.
#' @param startDate the start of the summation interval, formatted as a string 'yyyy-mm-dd'. Defaults to the start of the year.
#' @param endDate the end of the summation interval, formatted as a string 'yyyy-mm-dd'. Defaults to the end of the year.
#' @return dataframe containing the year, the day-of-the-year (doy) and the Huglin-index for each day-of-the-year.
#' @keywords value aggregation
#' @import plyr
#' @export

calculate_VE_index <- function(dgg,
                               startDate = paste(format(Sys.Date(), format="%Y"), "01-01", sep="-"),
                               endDate = paste(format(Sys.Date(), format="%Y"), "12-31", sep="-")) {

   # stop deze functie als de kolommen nog niet zijn hernoemd.
   if (!("stationID" %in% names(dgg)))
      stop("This function can only be applied after the data-frame columns have been renamed with the function 'rename_columns_KNMI_data()'.")

#   attach(dgg); on.exit(detach(name = dgg))

   # convert to day-of-year:
   doyStart <- data.table::yday(startDate)
   doyEnd <- data.table::yday(endDate)

   # subset op basis van de start en eind datum:
   range <- dgg[dgg$doy >= doyStart && dgg$doy <= doyEnd,]
   # bereken de dagwaarde:
   range$VEIndex <- pmax((range$gemTemp - 10), 0)
   # bereken nu de cumulatieve HuglinIndex:
   range <- plyr::ddply(range, .(stationID, jaar), transform, somVEIndex = cumsum(VEIndex))

#   detach(name = dgg)
   return(range[,c("stationID", "jaar", "doy", "somVEIndex")])
}
