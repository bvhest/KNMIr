#' @title Get six-day weather forecast (buienradar).
#'
#' @description
#' \code{get_6day_weather_forecast} retrieves the six-day weather forecast from the Buienradar-website.
#'
#' @details
#' This function retrieves the six-day weather forecast from the Buienradar-website by applying web-scraping to
#' the original Buienradara web-page. This page can be viewed at \url{https://www.buienradar.nl/nederland/verwachtingen/5-daagse-verwachting}.
#' Note that the url mentions a five day forecast, but the page contains a six day forecast.
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
#' @source \url{https://www.buienradar.nl/nederland/verwachtingen/5-daagse-verwachting}
#' @keywords weather forecast
#' @export
#'
get_6day_weather_forecast <- function() {
  # haal 6-daagse voorspelling:
  baseURL <- "https://www.buienradar.nl/nederland/verwachtingen/5-daagse-verwachting"
  pagina <- xml2::read_html(baseURL)

  weather_table <- rvest::html_nodes(pagina, xpath = '//table[@class="styled"]')

  # Het verwerken van de eerder gebruikte 'weather_row' is vervangen door een
  # functie-aanroep waarin het pad van de rij wordt meegegeven.
  voorspelling <- parse_buienradar_weer_tabel(
    rvest::html_nodes(weather_table, xpath = '//table[@class="styled"]/tbody/tr')
  )

  # kolom-namen omzetten van leesbaar, naar  codes (kan weer worden teruggezet met de functie 'rename_columns()')
  colnames(voorspelling) <- c(
    "STN",
    "station",
    "YYYYMMDD",
    "date",
    "TX",
    "SD_maxTemp",
    "TN",
    "SD_minTemp",
    "TG",
    "SD_gemTemp",
    "RH",
    "SD_neerslag",
    "neerslagKans",
    "SP",
    "windkracht"
  )

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
# get_6day_weather_forecast <-
#   function() {
#     # haal 6-daagse voorspelling:
#     baseURL <- "http://www.knmi.nl/nederland-nu/weer/verwachtingen"
#     pagina <- xml2::read_html(baseURL)
#
#     # extraheer waarden in lists:
#     datum <-
#       rvest::html_nodes(pagina,
#         css = ".weather-map__table-cell:nth-child(2)"
#       )
#     maxTemp <-
#       rvest::html_nodes(pagina,
#         css = ".weather-map__table-cell:nth-child(4)"
#       )
#     minTemp <-
#       rvest::html_nodes(pagina,
#         css = ".weather-map__table-cell:nth-child(5)"
#       )
#     neerslag <-
#       rvest::html_nodes(pagina,
#         css = ".weather-map__table-cell:nth-child(6)"
#       )
#     neerslagKans <-
#       rvest::html_nodes(pagina,
#         css = ".weather-map__table-cell:nth-child(7)"
#       )
#     percZon <-
#       rvest::html_nodes(pagina,
#         css = ".weather-map__table-cell:nth-child(8)"
#       )
#     wind <-
#       rvest::html_nodes(pagina,
#         css = ".weather-map__table-cell:nth-child(9)"
#       )
#
#     size <- length(datum)
#     voorspelling <-
#       data.frame(
#         stationID = rep("260", size), # De Bilt
#         station = rep("De Bilt", size),
#         YYYYMMDD = rep("20000101", size),
#         date = as.Date("2000-01-01") + 1:size, # just dummy dates
#         maxTemp = double(size),
#         maxTempSD = double(size),
#         minTemp = double(size),
#         minTempSD = double(size),
#         gemTemp = double(size),
#         gemTempSD = double(size),
#         neerslag = double(size),
#         neerslagSD = double(size),
#         neerslagKans = double(size),
#         percZon = double(size),
#         windkracht = double(size),
#         stringsAsFactors = FALSE
#       )
#
#     # nu alles in een loopje:
#     for (i in 1:size) {
#       voorspelling$date[i] <- as.Date(rvest::html_text(datum[i]), format = "%d-%m-%Y")
#       voorspelling$YYYYMMDD[i] <- as.character(rvest::html_text(datum[i]), format = "%Y%m%d")
#       # maxTemp
#       voorspelling$maxTemp[i] <- mean(as.numeric(extraheer_numerieke_waarden(maxTemp[i])))
#       voorspelling$maxTempSD[i] <- sd(as.numeric(extraheer_numerieke_waarden(maxTemp[i])))
#       # minTemp
#       voorspelling$minTemp[i] <- mean(as.numeric(extraheer_numerieke_waarden(minTemp[i])))
#       voorspelling$minTempSD[i] <- sd(as.numeric(extraheer_numerieke_waarden(minTemp[i])))
#       # gemTemp
#       voorspelling$gemTemp[i] <- mean(c(voorspelling$minTemp[i], voorspelling$maxTemp[i]))
#       voorspelling$gemTempSD[i] <-
#         sqrt((voorspelling$minTempSD[i] * voorspelling$minTempSD[i] + voorspelling$maxTempSD[i] * voorspelling$maxTempSD[i]) / 2)
#       # neerslag
#       voorspelling$neerslag[i] <- mean(as.numeric(extraheer_numerieke_waarden(neerslag[i])))
#       voorspelling$neerslagSD[i] <- sd(as.numeric(extraheer_numerieke_waarden(neerslag[i])))
#       # neerslagKans
#       voorspelling$neerslagKans[i] <- mean(as.numeric(extraheer_numerieke_waarden(neerslagKans[i])))
#       # percZon
#       voorspelling$percZon[i] <- mean(as.numeric(extraheer_numerieke_waarden(percZon[i])))
#       # wind
#       voorspelling$windkracht[i] <- mean(as.numeric(extraheer_numerieke_waarden(wind[i])))
#     }
#
#     # kolom-namen omzetten van leesbaar, naar  codes (kan weer worden teruggezet met de functie 'rename_columns()')
#     colnames(voorspelling) <-
#       c(
#         "STN", "station", "YYYYMMDD", "date", "TX", "SD_maxTemp", "TN", "SD_minTemp", "TG", "SD_gemTemp",
#         "RH", "SD_neerslag", "neerslagKans", "SP", "windkracht"
#       )
#
#     # windkracht omzetten naar m/s:
#     wind <- converteer_beaufort_schaal(voorspelling$windkracht)
#     voorspelling <- cbind(voorspelling, wind)
#
#     # dezelfde hulp-kolommen toevoegen als in de andere functies die KNMI-data ophalen:
#     voorspelling$doy <- lubridate::yday(voorspelling$date)
#     voorspelling$year <- lubridate::year(voorspelling$date)
#     voorspelling$month <- lubridate::month(voorspelling$date)
#     voorspelling$week <- lubridate::week(voorspelling$date)
#     voorspelling$day <- lubridate::wday(voorspelling$date)
#
#     return(voorspelling)
#   }

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
#'   \item TX	Maximum temperatuur (in 0.1 graden Celsius);
#'   \item TN	Minimum temperatuur (in 0.1 graden Celsius);
#'   \item TG	Etmaalgemiddelde temperatuur (in 0.1 graden Celsius);
#'   \item RH	Etmaalsom van de neerslag (in 0.1 mm) (-1 voor <0.05 mm);
#'   \item neerslagKans	Kans op neerslag (uitgedrukt in %);
#'   \item SP	Percentage van de langst mogelijke zonneschijnduur;
#'   \item windkracht (Beaufort);
#'   \item minWind	minimum wind (in m/s);
#'   \item maxWind	maximum wind (in m/s);
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
get_14day_weather_forecast <- function(station = 260) { # default station is 260 (De Bilt, KNMI)

  # perform some sanity checks on the input
  station_is_known <- (station %in% stations$stationID | station %in% stations$plaats)

  if (!station_is_known) {
    stop("The value for 'station' does not match any known KNMI measurement station.")
  }

  # check if the station is passed as a code or a name
  if (is.numeric(station)) {
    station_ID <- as.numeric(station)
    station_Name <- stations$plaats[stations$stationID == station_ID]
  } else {
    station_Name <- station
    station_ID <- stations$stationID[stations$plaats == station_Name]
  }

  # haal 14-daagse voorspelling:
  base_URL <- "https://www.weerplaza.nl/nederland/"
  location_URL <- get_URL_based_on_location(station)
  URL <- paste0(base_URL, location_URL)

  pagina <- xml2::read_html(URL)
  weather_table <- rvest::html_nodes(pagina, xpath = '//table[@class="hasChart" or @class=""]')

  # Het verwerken van de eerder gebruikte 'weather_row' is vervangen door een
  # functie-aanroep waarin het pad van de rij wordt meegegeven.
  voorspelling <- parse_weerplaza_weer_tabel(
    station_ID,
    station_Name,
    rvest::html_nodes(weather_table, xpath = '//table[@class="hasChart"]/tbody/tr')
  ) |>
    dplyr::bind_rows(
      parse_weerplaza_weer_tabel(
        station_ID,
        station_Name,
        rvest::html_nodes(weather_table, xpath = '//table[@class=""]/tbody/tr')
      )
    )

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

