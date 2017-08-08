#' @title get KNMI climate data from zip-files.
#'
#' @description
#' \code{get_climate_data_zip} retrieves KNMI data by downloading prepared KNMI zip-files.
#'
#' @details
#' This function retrieves raw climate data collected by the official KNMI weather stations. It is optimised for
#' retrieving large sets of data that have been prepared by the KNMI for download. If a year in the past is selected,
#' the call is forwarded to the function \code{\link{get_climate_data_api}}. The function \code{\link{get_climate_data_api}}
#' in this package is better suited to retrieve data for very specific date-ranges.
#'
#' You can specify a specific station or get data from all the stations at once (the default).
#' When the from and to date parameters are not proviced, all measurements for the current year are returned. Otherwise
#' the data is subsetted to the given interval.
#'
#' The original KNMI API is described at the web-page \href{http://www.knmi.nl/nederland-nu/klimatologie/daggegevens}{Daggegevens van het weer in Nederland}.
#'
#' Note: this function also works for the measurement stations in the North Sea. When the parameter station = "ALL"
#'       and return_only_land = FALSE, the data for all stations on land and sea is returned. With return_only_land = TRUE
#'       (the default) only the data for the land-based stations is returned.
#'
#' @param stationID ID for the KNMI station. The available stations can be retrieved with the function 'list_stations()'. Defaults to "ALL". . Note: a string of characters in the format 'iii'.
#' @param from startdate for the time-window. Defaults to the start of the current year. If the returned data is from a later date, no prior data is available for the selected station. Note: a string of characters in the format 'yyyymmdd'.
#' @param to enddate for the time-window. Defaults to yesterday. If the returned data is from an earlier date, no recent data is available for the selected station. Note: a string of characters in the format 'yyyymmdd'.
#' @param return_only_land boolean indicating that only the data for the land-based stations is returned. Defaults to "TRUE".
#' @return a data frame.
#' @format The returned data frame contains the following columns:
#' \itemize{
#'   \item STN      = ID of measurementstation;
#'   \item YYYYMMDD = Datum (YYYY=jaar MM=maand DD=dag);
#'   \item DDVEC	Vectorgemiddelde windrichting in graden (360=noord, 90=oost, 180=zuid, 270=west, 0=windstil/variabel). Zie \url{http://www.knmi.nl/kennis-en-datacentrum/achtergrond/klimatologische-brochures-en-boeken};
#'   \item FHVEC	Vectorgemiddelde windsnelheid (in 0.1 m/s). Zie \url{http://www.knmi.nl/kennis-en-datacentrum/achtergrond/klimatologische-brochures-en-boeken};
#'   \item FG	Etmaalgemiddelde windsnelheid (in 0.1 m/s);
#'   \item FHX	Hoogste uurgemiddelde windsnelheid (in 0.1 m/s);
#'   \item FHXH	Uurvak waarin FHX is gemeten;
#'   \item FHN	Laagste uurgemiddelde windsnelheid (in 0.1 m/s);
#'   \item FHNH	Uurvak waarin FHN is gemeten;
#'   \item FXX	Hoogste windstoot (in 0.1 m/s);
#'   \item FXXH	Uurvak waarin FXX is gemeten;
#'   \item TG	Etmaalgemiddelde temperatuur (in 0.1 graden Celsius);
#'   \item TN	Minimum temperatuur (in 0.1 graden Celsius);
#'   \item TNH	Uurvak waarin TN is gemeten;
#'   \item TX	Maximum temperatuur (in 0.1 graden Celsius);
#'   \item TXH	Uurvak waarin TX is gemeten;
#'   \item T10N	Minimum temperatuur op 10 cm hoogte (in 0.1 graden Celsius);
#'   \item T10NH	6-uurs tijdvak waarin T10N is gemeten;
#'   \item SQ	Zonneschijnduur (in 0.1 uur) berekend uit de globale straling (-1 voor <0.05 uur);
#'   \item SP	Percentage van de langst mogelijke zonneschijnduur;
#'   \item Q	Globale straling (in J/cm2);
#'   \item DR	Duur van de neerslag (in 0.1 uur);
#'   \item RH	Etmaalsom van de neerslag (in 0.1 mm) (-1 voor <0.05 mm);
#'   \item RHX	Hoogste uursom van de neerslag (in 0.1 mm) (-1 voor <0.05 mm);
#'   \item RHXH	Uurvak waarin RHX is gemeten;
#'   \item PG	Etmaalgemiddelde luchtdruk herleid tot zeeniveau (in 0.1 hPa) berekend uit 24 uurwaarden;
#'   \item PX	Hoogste uurwaarde van de luchtdruk herleid tot zeeniveau (in 0.1 hPa);
#'   \item PXH	Uurvak waarin PX is gemeten;
#'   \item PN	Laagste uurwaarde van de luchtdruk herleid tot zeeniveau (in 0.1 hPa);
#'   \item PNH	Uurvak waarin PN is gemeten;
#'   \item VVN	Minimum opgetreden zicht;
#'   \item VVNH	Uurvak waarin VVN is gemeten;
#'   \item VVX	Maximum opgetreden zicht;
#'   \item VVXH	Uurvak waarin VVX is gemeten;
#'   \item NG	Etmaalgemiddelde bewolking (bedekkingsgraad van de bovenlucht in achtsten, 9=bovenlucht onzichtbaar);
#'   \item UG	Etmaalgemiddelde relatieve vochtigheid (in procenten);
#'   \item UX	Maximale relatieve vochtigheid (in procenten);
#'   \item UXH	Uurvak waarin UX is gemeten;
#'   \item UN	Minimale relatieve vochtigheid (in procenten);
#'   \item UNH	Uurvak waarin UN is gemeten;
#'   \item EV24	Referentiegewasverdamping (Makkink) (in 0.1 mm);
#' }
#' @keywords historic weather data
#' @export
get_climate_data_zip <- function(stationID = "ALL",
                                 from = paste(format(Sys.Date(), format="%Y"), "0101", sep=""),
                                 to = format(Sys.Date()-1, format="%Y%m%d"),
                                 return_only_land = TRUE) {

   thisYear <- format(Sys.Date(), format="%Y")
   fromYear <- substr(from, 1, 4)

   if(station=='ALL' & fromYear < thisYear) {
      # no other choice than to use the slower script-based download:
      data <- get_climate_data_api(station, from, to)

   } else {
      # retrieve prepared download-files from the KNMI website
      if(station == 'ALL' & fromYear == thisYear) {
        link <- "http://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/jaar.zip"
        file <- "jaar.txt"
      }
      else {
        # ToDo: check if stion code is valid, else issue error message.

        # retrieve all available data for this station:
        link <- paste0("http://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_",station,".zip")
        file <- paste0("etmgeg_",station,".txt")
      }
      # download a zip file containing broadband data, save it to the working directory
      download.file(link,
                    destfile="./temp.zip")
      # unzip the file
      unzip(zipfile="./temp.zip",
            exdir = "./temp")
      # read the data into R, with "|" seperating values
      data <- read.csv(file = paste0("./temp/", file),
                       header = FALSE,
                       sep = ",",
                       skip = 47,
                       strip.white = TRUE,
                       comment.char = "#",
                       na.strings = "-",
                       as.is = TRUE)

      colnames(data) <- c("STN","YYYYMMDD","DDVEC","FHVEC","FG","FHX","FHXH","FHN","FHNH","FXX","FXXH","TG","TN","TNH",
                          "TX","TXH","T10N","T10NH","SQ","SP","Q","DR","RH","RHX","RHXH","PG","PX","PXH","PN","PNH",
                          "VVN","VVNH","VVX","VVXH","NG","UG","UX","UXH","UN","UNH","EV24")

      # return subset based on provided parameters.
      data <- data[data$YYYYMMDD >= from & data$YYYYMMDD <= to,]
   }

   if (!(return_only_land)) {
      sea_data <- get_sea_data_zip(station, from, to)

      if (station == "ALL") {
         # if station = "ALL", then combine the data while taking into account that the sea-based stations provide
         # less variables than the land-based stations
         data <- rbind(data[,colnames(sea_data)],
                       sea_data)

      } else {
         # only return the data from the specified sea-based station
         data <- sea_data
      }
   }

   return(data)
}


get_sea_data_zip <- function(station = "ALL",
                             from = paste(format(Sys.Date(), format="%Y"), "0101", sep=""),
                             to = format(Sys.Date()-1, format="%Y%m%d")) {

  base_url <- "http://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_"
  sea_based_stations <- c(201,
                          203,
                          204,
                          205,
                          206,
                          207,
                          208,
                          211,
                          212,
                          239,
                          252,
                          320,
                          321)

  if (station != "ALL")
     sea_based_stations <- station

  i <- 0
  for (s in sea_based_stations) {
     i <- i + 1

     link <- paste0(base_url, s, ".zip")
     download.file(link,
                   destfile="./temp.zip")

     # unzip the file
     unzip(zipfile="./temp.zip",
           exdir = "./temp")

          # read the data into R, with "|" seperating values
     file <- paste0("etmgeg_",s,".txt") # etmgeg_201.txt
     data <- read.csv(file = paste0("./temp/", file),
                      header = FALSE,
                      sep = ",",
                      skip = 47,
                      strip.white = TRUE,
                      comment.char = "#",
                      na.strings = "-",
                      as.is = TRUE)
     #  gegevens van temperatuur, zon, bewolking en zicht, luchtdruk, wind en neerslag per station gecombineerd.
     colnames(data) <- c("STN", "YYYYMMDD", "DDVEC", "FHVEC", "FG", "FHX", "FHXH", "FHN", "FHNH", "FXX", "FXXH", "TG",
                         "TN", "TNH", "TX", "TXH", "PG", "PX","PXH", "PN", "PNH", "VVN", "VVNH", "VVX", "VVXH", "NG",
                         "UG", "UX", "UXH",  "UN", "UNH")

     if (i == 1) {
        all_stations <- data
     } else {
        all_stations <- rbind(all_stations, data)
     }
  }

  # return subset based on provided parameters.
  all_stations <- all_stations[all_stations$YYYYMMDD >= from & all_stations$YYYYMMDD <= to, 1:31]

  return(all_stations)

}
