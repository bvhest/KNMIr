# library(plyr)
# library(RCurl)

#' retrieve Historic Data
#'
#' This function retrieves historic weather data collected by the official KNMI weather stations.
#' The function 'retrieveDataRange()' in this package is better suited to retrieve smaller amounts of data for more specific date-ranges.
#'
#' You can specify a specific station or get data from all the stations at once (the default).
#' When the from and to date parameters are not proviced, all measurements are returned. Otherwise the data is subsetted to the given interval.
#'
#' @param station ID for the KNMI station. The available stations can be retrieved with the function 'getStations()'. Defaults to "all". . Note: a string of characters in the format 'iii'.
#' @param from startdate for the time-window. Defaults to the start of the current year. If the returned data is from a later date, no prior data is available for the selected station. Note: a string of characters in the format 'yyyymmdd'.
#' @param to enddate for the time-window. Defaults to yesterday. If the returned data is from an earlier date, no recent data is available for the selected station. Note: a string of characters in the format 'yyyymmdd'.
#' @return a data frame.
#' @format The returned data frame contains the following columns:
#' \itemize{
#'   \item STN      = ID of measurementstation;
#'   \item YYYYMMDD = Datum (YYYY=jaar MM=maand DD=dag);
#'   \item DDVEC	Vectorgemiddelde windrichting in graden (360=noord, 90=oost, 180=zuid, 270=west, 0=windstil/variabel). Zie http://www.knmi.nl/kennis-en-datacentrum/achtergrond/klimatologische-brochures-en-boeken;
#'   \item FHVEC	Vectorgemiddelde windsnelheid (in 0.1 m/s). Zie http://www.knmi.nl/kennis-en-datacentrum/achtergrond/klimatologische-brochures-en-boeken;
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
retrieveHistoricData <- function(station = "ALL",
                                 from = paste(format(today(), format="%Y"), "0101", sep=""),
                                 to = format(today()-1, format="%Y%m%d")) {

   thisYear <- format(today(), format="%Y")
   selectedYear <- substr(from, 1, 4)

   if(station == 'ALL' && selectedYear < thisYear) {
      # no other choice than to use the slower script-based download:
      retrieveDataRange(station, from, to)
   } else {
      if(station=='ALL') {
            link <- "http://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/jaar.zip"
            file <- "jaar.txt"
      }
      else {
         # ToDo: check if stion code is valid, else issue error message.
         # retrieve all available data for this station:
         link <- paste("http://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_",station,".zip", sep="")
         file <- paste("etmgeg_",station,".txt", sep="")
      }
      # download a zip file containing broadband data, save it to the working directory
      download.file(link, destfile="./temp.zip")
      # unzip the file
      unzip(zipfile="./temp.zip", exdir = "./temp")
      # read the data into R, with "|" seperating values
      data <- read.csv(file = paste("./temp/",file,sep="")
                       , header = FALSE, sep = ",", skip = 47
                       , strip.white = TRUE, comment.char = "#"
                       , na.strings = "-", as.is = TRUE)

      colnames(data) <- c("STN","YYYYMMDD","DDVEC","FHVEC","FG","FHX","FHXH","FHN","FHNH","FXX","FXXH","TG","TN","TNH","TX","TXH","T10N","T10NH","SQ","SP","Q","DR","RH","RHX","RHXH","PG","PX","PXH","PN","PNH","VVN","VVNH","VVX","VVXH","NG","UG","UX","UXH","UN","UNH","EV24")

      # return subset based on provided parameters.
      data[data$YYYYMMDD >= from & data$YYYYMMDD <= to,]
   }
}
