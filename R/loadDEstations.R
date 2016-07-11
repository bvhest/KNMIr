#
# load German weather stations
#
# Duitse klimaat data via Access to the climate data of the Deutscher Wetterdienst
#
# Beschrijving van de data: ftp://ftp-cdc.dwd.de/pub/CDC/Readme_intro_CDC_ftp.pdf
# Beschrijving van de stations: ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/KL_Tageswerte_Beschreibung_Stationen.txt
#
# Zie bijv. de volgende directory voor de data (bijgewerkt tot "vandaag"-1): ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/
#
# BHE, 11-07-2016
#

#' Title loadDEWeatherStations
#'
#' Returns a list of active measurement stations for the Deutscher Wetterdienst.
#'
#' @return a data frame with the id, name, longitude and latitude of the stations.
#' @export
#'
#' @examples
loadDEWeatherStations <- function() {
   url <- "ftp://ftp-cdc.dwd.de/pub/CDC/help/KL_Tageswerte_Beschreibung_Stationen.txt"
   library(readr)
   d <- read_fwf(file = url,
                 skip = 2,
                 n_max = 1089,
                 locale = locale(encoding = "windows-1252"),
                 fwf_widths(c(12, 9, 9, 15, 12, 10, 40, 92)))
   colnames(d) <- c("station", "actief_van", "actief_tot", "hoogte", "lat", "lon", "plaats", "staat")

   # selecteer actieve stations:
   vandaag <- as.integer(format(Sys.Date(), format="%Y%m%d")) - 7 # veiligheidsmarge omdat data niet altijd bijgewerkt is.
   d <- subset(d,
               actief_tot >= vandaag,
               select = c("station", "plaats", "lon",  "lat"))

   return(d)
}

#' Title selectDEWeatherStations
#'
#' Subsets the weather stations based on latitude of the stations. Defaults to returning all stations.
#'
#' @param stations stations to be subsetted.
#' @param from_lat minimum latitude
#' @param to_lat maximum latitude
#' @param from_lon minimum longitude
#' @param to_lon maximum longitude
#'
#' @return data frame with selection of stations.
#' @export
#'
selectDEWeatherStations <- function(stations,
                                    from_lat = 0,
                                    to_lat   = 90,
                                    from_lon = 0,
                                    to_lon   = 180) {

  s <- subset(stations,
              lat >= from_lat & lat <= to_lat &
              lon >= from_lon & lon <= to_lon)
  return(s)
}
