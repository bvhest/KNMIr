#
# load German weather stations
#
# Duitse klimaat data via Access to the climate data of the Deutscher Wetterdienst
#
# Beschrijving van de data: ftp://ftp-cdc.dwd.de/pub/CDC/Readme_intro_CDC_ftp.pdf
# Beschrijving van de stations: ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/KL_Tageswerte_Beschreibung_Stationen.txt
#
# Zie bijv. de volgende directory voor de data (bijgewerkt tot "vandaag"-1): ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/
# stations-data is in de vorm; "tageswerte_KL_00044_akt.zip"
# BHE, 11-07-2016
#

#' Title retrieveDEDataByStation
#'
#' help-function that actually downloads the data from the Deutscher Wetterdienst [web site](ftp://ftp-cdc.dwd.de/pub/CDC/Readme_intro_CDC_ftp.pdf).
#'
#' @param stationID ID for the Deutscher Wetterdienst measurement station.
#'
#' @return a data-frame containing the measurement data for the given station..
#' @export
#'
retrieveDEDataByStation <- function(stationID) {
   library(stringr)
   library(readr)

   temp <- tempfile()
   url <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/"
   filename <- paste0("tageswerte_KL_",str_pad(stationID, 5, pad = "0"),"_akt.zip")

   download.file(paste0(url,filename),temp)

   datum_van <- "20150107"
   datum_tot <- stations$actief_tot[stations$station == stationID]
   filename <- paste0("produkt_klima_Tageswerte_",datum_van,"_",datum_tot,"_",str_pad(stationID, 5, pad = "0"),".txt")
   t <- unz(temp, filename)
   d <- read.table(file = unz(temp, filename),
                   header = TRUE,
                   sep = ";",
                   fill = TRUE)
   unlink(temp)

   colnames(d) <- c("STN", "YYYYMMDD", "QUALITY", "TG", "DAMPDRUK", "NG", "PG", "UG", "FG" , "TX", "TN", "T10N", "FXX", "RH", "RH_IND", "SQ", "SNEEUWHOOGTE", "EOR")

   d <- subset(d,
            select= c("STN", "YYYYMMDD", "TG",  "TX", "TN", "T10N", "NG", "PG", "UG", "FG" , "FXX", "RH", "SQ"))
   return(d)
}

#' Title retrieveDEDataRange
#'
#' This function retrieves theweather data collected by the official Deutscher Wetterdienst weather stations for a specified range of stations and for a specified date-range.
#'
#' @param stationID ID for the Deutscher Wetterdienst measurement station. The available stations can be retrieved with the function 'getStations()'. Defaults to "all". . Note: a string of characters in the format 'iii'.
#' @param from startdate for the time-window. Defaults to the start of the current year. Note: a string of characters in the format 'yyyymmdd'.
#' @param to enddate for the time-window. Defaults to yesterday. Note: a string of characters in the format 'yyyymmdd'.
#'
#' @return a data-frame containing the measurement data for one or multiple stations.
#' @export
#'
retrieveDEDataRange <- function(stationID,
                                from = paste0(format(Sys.Date(), format="%Y"), "0101"),
                                to = format(Sys.Date()-1, format="%Y%m%d")) {

  d <- data.frame( "STN"= integer(), "YYYYMMDD"= integer(), "TG" = numeric(),  "TX" = numeric(), "TN" = numeric(), "T10N" = numeric(), "NG" = numeric(), "PG" = numeric(), "UG" = numeric(), "FG" = numeric() , "FXX" = numeric(), "RH" = numeric(), "SQ" = numeric())
  for (id in stationID) {
    t <- retrieveDEDataByStation(id)
    d <- rbind(d,t)
  }

  d <- subset(d, YYYYMMDD >= as.integer(from) & YYYYMMDD <= as.integer(to))

  return(d)
}
