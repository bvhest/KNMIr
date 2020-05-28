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
#'   \item station  Name of the measurement station;
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
#'
get_6day_weather_forecast <-
  function() {

     # haal 6-daagse voorspelling:
     baseURL <- "http://www.knmi.nl/nederland-nu/weer/verwachtingen"
     pagina <- xml2::read_html(baseURL)

     # extraheer waarden in lists:
     datum <-
       rvest::html_nodes(pagina,
                         css = '.weather-map__table-cell:nth-child(2)')
     maxTemp <-
       rvest::html_nodes(pagina,
                         css = '.weather-map__table-cell:nth-child(4)')
     minTemp <-
       rvest::html_nodes(pagina,
                         css = '.weather-map__table-cell:nth-child(5)')
     neerslag <-
       rvest::html_nodes(pagina,
                         css = '.weather-map__table-cell:nth-child(6)')
     neerslagKans <-
       rvest::html_nodes(pagina,
                         css = '.weather-map__table-cell:nth-child(7)')
     percZon <-
       rvest::html_nodes(pagina,
                         css = '.weather-map__table-cell:nth-child(8)')
     wind <-
       rvest::html_nodes(pagina,
                         css = '.weather-map__table-cell:nth-child(9)')

     size <- length(datum)
     voorspelling <-
       data.frame(stationID = rep("260", size), # De Bilt
                  station = rep("De Bilt", size),
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
     colnames(voorspelling) <-
       c("STN","station","YYYYMMDD","date","TX","SD_maxTemp","TN","SD_minTemp","TG","SD_gemTemp",
         "RH","SD_neerslag","neerslagKans","SP","windkracht")

     # windkracht omzetten naar m/s:
     wind <- converteer_beaufort_schaal(voorspelling$windkracht)
     voorspelling <- cbind(voorspelling, wind)

     # dezelfde hulp-kolommen toevoegen als in de andere functies die KNMI-data ophalen:
     voorspelling$doy <- lubridate::yday(voorspelling$date)
     voorspelling$year <- lubridate::year(voorspelling$date)
     voorspelling$month <- lubridate::month(voorspelling$date)
     voorspelling$week <- lubridate::week(voorspelling$date)
     voorspelling$day <- lubridate::wday(voorspelling$date)

     return(voorspelling)
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
#' @param station either the id or the name of a KNMI measurement station.
#' @return a data frame.
#' @format The returned data frame contains the following columns:
#' \itemize{
#'   \item STN      ID of the measurement station;
#'   \item station  Name of the measurement station;
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
#'
get_14day_weather_forecast <-
  function(station = 260) {

    # perform some sanity checks on the input
    station_is_known <-
      (station %in% stations$stationID | station %in% stations$plaats)

    if(!station_is_known)
      stop("The value for 'station' does not match any known KNMI measurement station.")

    # default station is 260 (De Bilt, KNMI)
    if (is.numeric(station)) {
      stationID = station
      stationName = stations$plaats[stations$stationID == stationID]
    } else {
      stationName = station
      stationID = stations$stationID[stations$plaats == stationName]
    }

    # haal 6-daagse voorspelling:
    base_URL <- "https://www.weerplaza.nl/nederland/"
    location_URL <-
     get_URL_based_onlocation(station)
    URL <-
     paste0(base_URL, location_URL)

    pagina <- xml2::read_html(URL)
    weather_table <-
     rvest::html_nodes(pagina,
                       xpath = '//table[@class="hasChart" or @class=""]')
    weather_row <-
     rvest::html_nodes(weather_table,
                       xpath = '//table[@class="hasChart" or @class=""]/tbody/tr')
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
    datum <-
     rvest::html_nodes(weather_row,
                       xpath = 'td/@data-day')
    maxTemp <-
     rvest::html_nodes(weather_row,
                       xpath = 'td/div[@class="red temp"]')
    minTemp <-
     rvest::html_nodes(weather_row,
                       xpath = 'td/div[@class="blue temp"]')
    neerslag <-
     rvest::html_nodes(weather_row,
                       xpath = 'td/div[@class="blue"][not(i/@class)]')
    neerslagKans <-
     rvest::html_nodes(weather_row,
                       xpath = 'td/div[@class="blue"][i/@class="fa fa-tint blue"]')
    percZon <-
     rvest::html_nodes(weather_row,
                       xpath = 'td/div[@class="orange"]')
    wind <-
     rvest::html_nodes(weather_row,
                       xpath = 'td/div[@class="hide"]')

    datum <- unique(rvest::html_text(datum))
    size <- length(datum)
    voorspelling <-
     data.frame(stationID = rep(stationID, size),
                station = rep(stationName, size),
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
    colnames(voorspelling) <-
     c("STN","station","YYYYMMDD","date","TX","SD_maxTemp","TN","SD_minTemp","TG","SD_gemTemp",
       "RH","SD_neerslag","neerslagKans","SP","windkracht")

    # windkracht omzetten naar m/s:
    wind <- converteer_beaufort_schaal(voorspelling$windkracht)
    voorspelling <- cbind(voorspelling, wind)

    # dezelfde hulp-kolommen toevoegen als in de andere functies die KNMI-data ophalen:
    voorspelling$doy <- lubridate::yday(voorspelling$date)
    voorspelling$year <- lubridate::year(voorspelling$date)
    voorspelling$month <- lubridate::month(voorspelling$date)
    voorspelling$week <- lubridate::week(voorspelling$date)
    voorspelling$day <- lubridate::wday(voorspelling$date)

    return(voorspelling)
}

########################################################################################################################
#
# hulp-functies
#
########################################################################################################################

# voor get_6day_weather_prediction
extraheer_numerieke_waarden <- function(html_node) {
  text <- stringr::str_extract_all(rvest::html_text(html_node),
                                   "\\(?[0-9,]+\\)?")[[1]]
  numeriek <- as.numeric(text)
  return(numeriek)
}

# voor omzetten windkracht in Beaufort naar m/s
converteer_beaufort_schaal <- function(windkracht) {
  data(beaufort_scale)
  min_m_per_s <- beaufort_scale$min_m_per_s[match(windkracht, beaufort_scale$Beaufort)]
  max_m_per_s <- beaufort_scale$max_m_per_s[match(windkracht, beaufort_scale$Beaufort)]

  return(data.frame(minWind = min_m_per_s, maxWind = max_m_per_s))
}

# decode station id of naam voor Weerplaza URL
get_URL_based_onlocation <-
  function(station) {

    locationURL <-
      dplyr::case_when(station == 210 | station == "Valkenburg" ~ "valkenburg/19412/",
                       station == 215 | station == "Voorschoten" ~ "voorschoten/19838/",
                       station == 225 | station == "IJmuiden" ~ "ijmuiden/11054/",
                       station == 235 | station == "De Kooy" ~ "de-kooy/2753344/",
                       station == 240 | station == "Schiphol" ~ "schiphol/5577/",
                       station == 242 | station == "Vlieland" ~ "vlieland/2743351/",
                       station == 248 | station == "Wijdenes" ~ "wijdenes/20205/",
                       station == 249 | station == "Berkhout" ~ "berkhout/6569/",
                       station == 251 | station == "Hoorn Terschelling" ~ "hoorn/2750195/",
                       station == 257 | station == "Wijk aan Zee" ~ "wijk-aan-zee/20209/",
                       station == 260 | station == "De Bilt" ~ "de-bilt/8500/",
                       station == 265 | station == "Soesterberg" ~ "amersfoort/5551/",
                       station == 267 | station == "Stavoren" ~ "stavoren/18367/",
                       station == 269 | station == "Lelystad" ~ "lelystad/12571/",
                       station == 270 | station == "Leeuwarden" ~ "leeuwarden/12537/",
                       station == 273 | station == "Marknesse" ~ "marknesse/13455/",
                       station == 275 | station == "Deelen" ~ "deelen/8552/",
                       station == 277 | station == "Lauwersoog" ~ "lauwersoog/12418/",
                       station == 278 | station == "Heino" ~ "heino/10580/",
                       station == 279 | station == "Hoogeveen" ~ "hoogeveen/10870/",
                       station == 280 | station == "Eelde" ~ "eelde-paterswolde/8967/",
                       station == 283 | station == "Hupsel" ~ "hupsel/2749993/",
                       station == 286 | station == "Nieuw Beerta" ~ "nieuw-beerta/2747446/",
                       station == 290 | station == "Twenthe" | station == "Twente" ~ "twente/2743881/",
                       station == 308 | station == "Cadzand" ~ "cadzand/7340/",
                       station == 310 | station == "Vlissingen" ~ "vlissingen/19806/",
                       station == 311 | station == "Hoofdplaat" ~ "hoofdplaat/10860/",
                       station == 315 | station == "Hansweert" ~ "hansweert/10434/",
                       station == 319 | station == "Westdorpe" ~ "westdorpe/20115/",
                       station == 323 | station == "Wilhelminadorp" ~ "wilhelminadorp/20231/",
                       station == 324 | station == "Stavenisse" ~ "stavenisse/18364/",
                       station == 330 | station == "Hoek van Holland" ~ "hoek-van-holland/10780/",
                       station == 331 | station == "Tholen" ~ "tholen/18860/",
                       station == 340 | station == "Woensdrecht" ~ "woensdrecht/20308/",
                       station == 343 | station == "Rotterdam Geulhaven" ~ "rotterdam/16707/",
                       station == 344 | station == "Rotterdam" ~ "rotterdam/16707/",
                       station == 348 | station == "Cabauw Mast" ~ "cabauw/13004/",
                       station == 350 | station == "Gilze-Rijen" ~ "gilze-rijen/9950/",
                       station == 356 | station == "Herwijnen" ~ "herwijnen/10680/",
                       station == 370 | station == "Eindhoven" ~ "eindhoven/9020/",
                       station == 375 | station == "Volkel" ~ "volkel/19829/",
                       station == 377 | station == "Ell" ~ "ell/9091/",
                       station == 380 | station == "Maastricht" ~ "maastricht/13193/",
                       station == 391 | station == "Arcen" ~ "arcen/5741/",
                       TRUE   ~ "de-bilt/8500/")

    return(locationURL)
  }
