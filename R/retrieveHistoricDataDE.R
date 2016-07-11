library(stringr)
library(readr)

#' Title retrieveHistoricDEDataByStation
#'
#' help-function that actually downloads the data from the Deutscher Wetterdienst [web site](ftp://ftp-cdc.dwd.de/pub/CDC/Readme_intro_CDC_ftp.pdf).
#'
#' @param stationID ID for the Deutscher Wetterdienst measurement station.
#'
#' @return a data-frame containing the measurement data for the given station..
#' @export
#'
retrieveHistoricDEDataByStation <- function(stationID,
                                            from,
                                            to) {
  padded_station_id <- str_pad(stationID, 5, pad = "0")

  url <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/"

  # eerst alle bestandsnamen ophalen omdat deze naast de stationscode, ook de
  # (onbekende) start en einddatum bevatten.
  filenames <- listFilesOnFTPserver(url)

  pattern <- paste0("tageswerte_",padded_station_id)
  zipname <- filenames[grepl(pattern, filenames)]
  print(paste("downloading zipfile",zipname))

  temp <- tempfile()
  download.file(paste0(url,zipname),temp)

  # start en einddatum uit de bestandsnaam halen
  datum_van_tot <- substr(zipname, 18, 34)

  # filename moet er uitzien als: produkt_klima_Tageswerte_19710301_20151231_00044.txt
  filename <- paste0("produkt_klima_Tageswerte_",datum_van_tot,"_",padded_station_id,".txt")
  print(paste("unzipping and reading file",filename))
  t <- unz(temp, filename)
  d <- read.table(file = unz(temp, filename),
                  header = TRUE,
                  sep = ";",
                  fill = TRUE,
                  stringsAsFactors = FALSE)
  unlink(temp)

  colnames(d) <- c("STN", "YYYYMMDD", "QUALITY", "TG", "DAMPDRUK", "NG", "PG", "UG", "FG" , "TX", "TN", "T10N", "FXX", "RH", "RH_IND", "SQ", "SNEEUWHOOGTE", "EOR")

  d <- subset(d,
              YYYYMMDD >= from & YYYYMMDD <= to,
              select= c("STN", "YYYYMMDD", "TG",  "TX", "TN", "T10N", "NG", "PG", "UG", "FG" , "FXX", "RH", "SQ"))
  d$STN <- as.integer(d$STN)
  return(d)
}

listFilesOnFTPserver <- function(url){
  library(RCurl)
  dir <- unlist(
    strsplit(
      getURL(url,
             ftp.use.epsv = FALSE,
             dirlistonly = TRUE),
      "\n")
  )
  return(dir)
}


#' Title retrieveHistoricDataDE
#'
#' This function retrieves the *historic* weather data collected by the official Deutscher Wetterdienst weather stations for a specified range of stations and for a specified date-range.
#' Note that the most recent data will allways be December 31th of the previous year.
#'
#' @param stationID ID for the Deutscher Wetterdienst measurement station. The available stations can be retrieved with the function 'getStations()'. Defaults to "all". . Note: a string of characters in the format 'iii'.
#' @param from startdate for the time-window. Defaults to the start of the current year. Note: a string of characters in the format 'yyyymmdd'.
#' @param to enddate for the time-window. Defaults to yesterday. Note: a string of characters in the format 'yyyymmdd'.
#'
#' @return a data-frame containing the measurement data for one or multiple stations.
#' @export
#'
retrieveHistoricDataDE <- function(stationID,
                                   from = paste0(format(Sys.Date(), format="%Y"), "0101"),
                                   to = format(Sys.Date()-1, format="%Y%m%d")) {

  d <- data.frame( "STN"= integer(), "YYYYMMDD"= integer(), "TG" = numeric(),  "TX" = numeric(), "TN" = numeric(), "T10N" = numeric(), "NG" = numeric(), "PG" = numeric(), "UG" = numeric(), "FG" = numeric() , "FXX" = numeric(), "RH" = numeric(), "SQ" = numeric())
  for (id in stationID) {
    print(paste("downloading data for station",id))
    t <- retrieveHistoricDEDataByStation(id, as.integer(from), as.integer(to))
    d <- rbind(d,t)
  }
  return(d)
}
