#' @title Get six-day weather forecast (KNMI).
#'
#' @description
#' \code{get_6day_weather_forecast} retrieves the six-day weather forecast from the KNMI-website.
#'
#' @details
#' This function retrieves the six-day weather forecast from the KNMI-website by applying web-scraping to
#' the original KNMI web-page. This page can be viewed at \url{http://www.knmi.nl/nederland-nu/weer/verwachtingen}.
#'
#' The page \url{https://www.knmi.nl/kennis-en-datacentrum/achtergrond/de-weersverwachting} contains background
#' information on the weather forecast.
#'
#' @return a data frame.
#' @format The returned data frame contains the following columns:
#' \itemize{
#'   \item STN      ID of measurementstation;
#'   \item YYYYMMDD Datum (YYYY=jaar MM=maand DD=dag) in karakter-formaat;
#'   \item date     date in date-format;
#'   \item TN	Minimum temperatuur (in 0.1 graden Celsius);
#'   \item TX	Maximum temperatuur (in 0.1 graden Celsius);
#'   \item TG	Etmaalgemiddelde temperatuur (in 0.1 graden Celsius);
#'   \item RH	Etmaalsom van de neerslag (in 0.1 mm) (-1 voor <0.05 mm);
#'   \item neerslagKans	Kans op neerslag (uitgedrukt in %);
#'   \item SP	Percentage van de langst mogelijke zonneschijnduur;
#'   \item windkracht (Beaufort);
#'   \item min_m_per_s	minimum wind (in m/s);
#'   \item max_m_per_s	maximum wind (in m/s);
#'   \item doy	day-of-the-year;
#'   \item year	jaar;
#'   \item month	maand;
#'   \item week	weeknummer;
#'   \item day	   weekdag;
#' }
#' @source \url{http://www.knmi.nl/nederland-nu/weer/verwachtingen}
#' @keywords weather forecast
#' @export
get_6day_weather_forecast <- function() {

   # haal 6-daagse voorspelling:
   baseURL <- "http://www.knmi.nl/nederland-nu/weer/verwachtingen"
   pagina <- xml2::read_html(baseURL)

   # extraheer waarden in lists:
   datum <- rvest::html_nodes(pagina, css = '.weather-map__table-cell:nth-child(2)')
   maxTemp <- rvest::html_nodes(pagina, css = '.weather-map__table-cell:nth-child(4)')
   minTemp <- rvest::html_nodes(pagina, css = '.weather-map__table-cell:nth-child(5)')
   neerslag <- rvest::html_nodes(pagina, css = '.weather-map__table-cell:nth-child(6)')
   neerslagKans <- rvest::html_nodes(pagina, css = '.weather-map__table-cell:nth-child(7)')
   percZon <- rvest::html_nodes(pagina, css = '.weather-map__table-cell:nth-child(8)')
   wind <- rvest::html_nodes(pagina, css = '.weather-map__table-cell:nth-child(9)')

   size <- length(datum)
   voorspelling <- data.frame(station = rep("260", size),
                              YYYYMMDD = rep("20000101", size),
                              date = as.Date("2000-01-01") + 1:size, # just dummy dates
                              maxTemp = double(size),
                              maxTempSD = double(size),
                              minTemp = double(size),
                              minTempSD = double(size),
                              gemTemp = double(size),
                              gemTempSD = double(size),
                              neerslag = double(size),
                              neerslagSD = double(size),
                              neerslagKans = double(size),
                              percZon = double(size),
                              windkracht = double(size),
                              stringsAsFactors = FALSE)

   # nu alles in een loopje:
   for (i in 1:size) {
      voorspelling$date[i] <- as.Date(rvest::html_text(datum[i]), format = "%d-%m-%Y")
      voorspelling$YYYYMMDD[i] <- as.character(rvest::html_text(datum[i]), format = "%Y%m%d")
      # maxTemp
      voorspelling$maxTemp[i] <- mean(as.numeric(extraheer_numerieke_waarden(maxTemp[i])))
      voorspelling$maxTempSD[i] <- sd(as.numeric(extraheer_numerieke_waarden(maxTemp[i])))
      # minTemp
      voorspelling$minTemp[i] <- mean(as.numeric(extraheer_numerieke_waarden(minTemp[i])))
      voorspelling$minTempSD[i] <- sd(as.numeric(extraheer_numerieke_waarden(minTemp[i])))
      # gemTemp
      voorspelling$gemTemp[i] <- mean(c(voorspelling$minTemp[i], voorspelling$maxTemp[i]))
      voorspelling$gemTempSD[i] <-
        sqrt((voorspelling$minTempSD[i]*voorspelling$minTempSD[i] + voorspelling$maxTempSD[i]*voorspelling$maxTempSD[i]) / 2)
      # neerslag
      voorspelling$neerslag[i] <- mean(as.numeric(extraheer_numerieke_waarden(neerslag[i])))
      voorspelling$neerslagSD[i] <- sd(as.numeric(extraheer_numerieke_waarden(neerslag[i])))
      # neerslagKans
      voorspelling$neerslagKans[i] <- mean(as.numeric(extraheer_numerieke_waarden(neerslagKans[i])))
      # percZon
      voorspelling$percZon[i] <- mean(as.numeric(extraheer_numerieke_waarden(percZon[i])))
      # wind
      voorspelling$windkracht[i] <- mean(as.numeric(extraheer_numerieke_waarden(wind[i])))
   }

   # kolom-namen omzetten van leesbaar, naar  codes (kan weer worden teruggezet met de functie 'rename_columns()')
   colnames(voorspelling) <- c("STN","YYYYMMDD","date","TX","SD_maxTemp","TN","SD_minTemp","TG","SD_gemTemp",
                               "RH","SD_neerslag","neerslagKans","SP","windkracht")

   # windkracht omzetten naar m/s:
   wind <- converteer_beaufort_schaal(voorspelling$windkracht)
   voorspelling <- cbind(voorspelling, wind)

   # dezelfde hulp-kolommen toevoegen als in de andere functies die KNMI-data ophalen:
   voorspelling$doy <- yday(voorspelling$date)
   voorspelling$year <- year(voorspelling$date)
   voorspelling$month <- month(voorspelling$date)
   voorspelling$week <- week(voorspelling$date)
   voorspelling$day <- wday(voorspelling$date)

   return(voorspelling)
}

#
# hulp-functie voor get_6day_weather_prediction
#
extraheer_numerieke_waarden <- function(html_node) {
   text <- stringr::str_extract_all(rvest::html_text(html_node),
                                    "\\(?[0-9,]+\\)?")[[1]]
   numeriek <- as.numeric(text)
   return(numeriek)
}

#
# hulp-functie voor omzetten windkracht in Beaufort naar m/s
#
converteer_beaufort_schaal <- function(windkracht) {
   data(beaufort_scale)
   min_m_per_s <- beaufort_scale$min_m_per_s[match(windkracht, beaufort_scale$Beaufort)]
   max_m_per_s <- beaufort_scale$max_m_per_s[match(windkracht, beaufort_scale$Beaufort)]

   return(data.frame(minWind = min_m_per_s, maxWind = max_m_per_s))
}

#' @title Get fourteen-day weather forecast (Weerplaza).
#'
#' @description
#' \code{get_14day_weather_forecast} retrieves the fourteen-day weather forecast from the Weerplaza-website.
#'
#' @details
#' This function retrieves the fourteen-day weather forecast from the Weerplaza-website by applying web-scraping to
#' the original Weerplaza web-page. This page can be viewed at \url{https://www.weerplaza.nl/nederland/}.
#'
#' Note: the source of the information on which the Weerplaza forecast is based is not known. Most probably it's a
#'       combination of the European weather model ECMWF and an (unknown) American model (see
#'       \url{https://www.weerplaza.nl/15daagse/}.
#'
#' @return a data frame.
#' @format The returned data frame contains the following columns:
#' \itemize{
#'   \item STN      ID of measurementstation;
#'   \item YYYYMMDD Datum (YYYY=jaar MM=maand DD=dag) in karakter-formaat;
#'   \item date     date in date-format;
#'   \item TN	Minimum temperatuur (in 0.1 graden Celsius);
#'   \item TX	Maximum temperatuur (in 0.1 graden Celsius);
#'   \item TG	Etmaalgemiddelde temperatuur (in 0.1 graden Celsius);
#'   \item RH	Etmaalsom van de neerslag (in 0.1 mm) (-1 voor <0.05 mm);
#'   \item neerslagKans	Kans op neerslag (uitgedrukt in %);
#'   \item SP	Percentage van de langst mogelijke zonneschijnduur;
#'   \item windkracht (Beaufort);
#'   \item min_m_per_s	minimum wind (in m/s);
#'   \item max_m_per_s	maximum wind (in m/s);
#'   \item doy	day-of-the-year;
#'   \item year	jaar;
#'   \item month	maand;
#'   \item week	weeknummer;
#'   \item day	   weekdag;
#' }
#' @source \url{https://www.weerplaza.nl/nederland/}
#' @keywords weather forecast
#' @export
get_14day_weather_forecast <- function() {

   # haal 6-daagse voorspelling:
   baseURL <- "https://www.weerplaza.nl/nederland/gilze-rijen/9950/"
   pagina <- xml2::read_html(baseURL)
   weather_table <- rvest::html_nodes(pagina, xpath = '//table[@class="hasChart" or @class=""]')
   weather_row <- rvest::html_nodes(weather_table, xpath = '//table[@class="hasChart" or @class=""]/tbody/tr')
   # 10 rijen:
   # rij 1: datum + samenvatting
   # rij 2: zonkans
   # rij 3: min/max temp
   # rij 4: neerslag (kans en hoeveelheid)
   # rij 5: wind
   # idem voor rij 6-10
   #
   # Nb elke rij bevat in de td-elementen een attribuut met de datum (@data-day)

   # extraheer waarden in lists:
   datum <- rvest::html_nodes(weather_row, xpath = 'td/@data-day')
   maxTemp <- rvest::html_nodes(weather_row, xpath = 'td/div[@class="red temp"]')
   minTemp <- rvest::html_nodes(weather_row, xpath = 'td/div[@class="blue temp"]')
   neerslag <- rvest::html_nodes(weather_row, xpath = 'td/div[@class="blue"][not(i/@class)]')
   neerslagKans <- rvest::html_nodes(weather_row, xpath = 'td/div[@class="blue"][i/@class="fa fa-tint blue"]')
   percZon <- rvest::html_nodes(weather_row, xpath = 'td/div[@class="orange"]')
   wind <- rvest::html_nodes(weather_row, xpath = 'td/div[@class="hide"]')

   datum <- unique(rvest::html_text(datum))
   size <- length(datum)
   voorspelling <- data.frame(station = rep("260", size),
                              YYYYMMDD = rep("20000101", size),
                              date = as.Date("2000-01-01") + 1:size, # just dummy dates
                              maxTemp = double(size),
                              maxTempSD = double(size),
                              minTemp = double(size),
                              minTempSD = double(size),
                              gemTemp = double(size),
                              gemTempSD = double(size),
                              neerslag = double(size),
                              neerslagSD = double(size),
                              neerslagKans = double(size),
                              percZon = double(size),
                              windkracht = double(size),
                              stringsAsFactors = FALSE)

   # nu alles in een loopje:
   for (i in 1:size) {
      voorspelling$date[i] <- as.Date(datum[i], format = "%d%m%Y")
      voorspelling$YYYYMMDD[i] <- as.character(voorspelling$date[i], format = "%Y%m%d")
      # maxTemp
      voorspelling$maxTemp[i] <- mean(as.numeric(extraheer_numerieke_waarden(maxTemp[i])))
      voorspelling$maxTempSD[i] <- sd(as.numeric(extraheer_numerieke_waarden(maxTemp[i])))
      # minTemp
      voorspelling$minTemp[i] <- mean(as.numeric(extraheer_numerieke_waarden(minTemp[i])))
      voorspelling$minTempSD[i] <- sd(as.numeric(extraheer_numerieke_waarden(minTemp[i])))
      # gemTemp
      voorspelling$gemTemp[i] <- mean(c(voorspelling$minTemp[i], voorspelling$maxTemp[i]))
      voorspelling$gemTempSD[i] <-
         sqrt((voorspelling$minTempSD[i]*voorspelling$minTempSD[i] + voorspelling$maxTempSD[i]*voorspelling$maxTempSD[i]) / 2)
      # neerslag
      voorspelling$neerslag[i] <- mean(as.numeric(extraheer_numerieke_waarden(neerslag[i])))
      voorspelling$neerslagSD[i] <- sd(as.numeric(extraheer_numerieke_waarden(neerslag[i])))
      # neerslagKans
      voorspelling$neerslagKans[i] <- mean(as.numeric(extraheer_numerieke_waarden(neerslagKans[i])))
      # percZon
      voorspelling$percZon[i] <- mean(as.numeric(extraheer_numerieke_waarden(percZon[i])))
      # wind
      voorspelling$windkracht[i] <- mean(as.numeric(extraheer_numerieke_waarden(wind[i])))
   }

   # kolom-namen omzetten van leesbaar, naar  codes (kan weer worden teruggezet met de functie 'rename_columns()')
   colnames(voorspelling) <- c("STN","YYYYMMDD","date","TX","SD_maxTemp","TN","SD_minTemp","TG","SD_gemTemp",
                               "RH","SD_neerslag","neerslagKans","SP","windkracht")

   # windkracht omzetten naar m/s:
   wind <- converteer_beaufort_schaal(voorspelling$windkracht)
   voorspelling <- cbind(voorspelling, wind)

   # dezelfde hulp-kolommen toevoegen als in de andere functies die KNMI-data ophalen:
   voorspelling$doy <- yday(voorspelling$date)
   voorspelling$year <- year(voorspelling$date)
   voorspelling$month <- month(voorspelling$date)
   voorspelling$week <- week(voorspelling$date)
   voorspelling$day <- wday(voorspelling$date)

   return(voorspelling)
}
