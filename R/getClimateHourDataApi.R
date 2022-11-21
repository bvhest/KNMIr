#' @title get KNMI climate hourly data.
#'
#' @description
#' \code{get_hourly_data} retrieves KNMI hourly data through the KNMI-API.
#'
#' @details
#' This function retrieves raw climate data collected by the official KNMI measurement stations for a specific station
#' and/or date-range.
#'
#' See the documentation of the function \code{\link{get_daily_data}}, as both functions work similarly.
#'
#' Parameter defaults have been changed to prevent retrieving to much data.
#'
#' @param stationID ID for the KNMI measurement station. The available stations can be retrieved with the function \code{list_stations}. Defaults to "260" (De Bilt). . Note: a string of characters in the format 'iii', with 'iii' the number of the station of 'ALL'.
#' @param from startdate for the time-window. Defaults to one week ago. A string of characters in the format 'yyyymmddhh', 'yyyymmdd', 'yyyymm' or 'yyyy'. Missing digits are replaced with the first hour of the day, the first day of the month and/or the first month of the year.
#' @param to enddate for the time-window. Defaults to yesterday (most recent data provided by the KNMI). A string of characters in the format 'yyyymmddhh', 'yyyymmdd', 'yyyymm' or 'yyyy'. Missing digits are replaced with the last hour of the day, the last day of the month and/or the last month of the year.
#' @return a tibble.
#' @format The returned data frame contains the following columns:
#' \itemize{
#'   \item STN      = ID of measurement station;
#'   \item YYYYMMDD = Datum (YYYY=jaar MM=maand DD=dag);
#'   \item  HH       = tijd (HH=uur, UT.12 UT=13 MET, 14 MEZT. Uurvak 05 loopt van 04.00 UT tot 5.00 UT;
#'   \item  DD       = Windrichting (in graden) gemiddeld over de laatste 10 minuten van het afgelopen uur (360=noord, 90=oost, 180=zuid, 270=west, 0=windstil 990=veranderlijk. Zie http://www.knmi.nl/kennis-en-datacentrum/achtergrond/klimatologische-brochures-en-boeken;
#'   \item  FH       = Uurgemiddelde windsnelheid (in 0.1 m/s). Zie http://www.knmi.nl/kennis-en-datacentrum/achtergrond/klimatologische-brochures-en-boeken;
#'   \item  FF       = Windsnelheid (in 0.1 m/s) gemiddeld over de laatste 10 minuten van het afgelopen uur;
#'   \item  FX       = Hoogste windstoot (in 0.1 m/s) over het afgelopen uurvak;
#'   \item  T        = Temperatuur (in 0.1 graden Celsius) op 1.50 m hoogte tijdens de waarneming;
#'   \item  T10N     = Minimumtemperatuur (in 0.1 graden Celsius) op 10 cm hoogte in de afgelopen 6 uur;
#'   \item  TD       = Dauwpuntstemperatuur (in 0.1 graden Celsius) op 1.50 m hoogte tijdens de waarneming;
#'   \item  SQ       = Duur van de zonneschijn (in 0.1 uren) per uurvak, berekend uit globale straling  (-1 for <0.05 uur);
#'   \item  Q        = Globale straling (in J/cm2) per uurvak;
#'   \item  DR       = Duur van de neerslag (in 0.1 uur) per uurvak;
#'   \item  RH       = Uursom van de neerslag (in 0.1 mm) (-1 voor <0.05 mm);
#'   \item  P        = Luchtdruk (in 0.1 hPa) herleid naar zeeniveau, tijdens de waarneming;
#'   \item  VV       = Horizontaal zicht tijdens de waarneming (0=minder dan 100m, 1=100-200m, 2=200-300m,..., 49=4900-5000m, 50=5-6km, 56=6-7km, 57=7-8km, ..., 79=29-30km, 80=30-35km, 81=35-40km,..., 89=meer dan 70km);
#'   \item  N        = Bewolking (bedekkingsgraad van de bovenlucht in achtsten), tijdens de waarneming (9=bovenlucht onzichtbaar);
#'   \item  U        = Relatieve vochtigheid (in procenten) op 1.50 m hoogte tijdens de waarneming;
#'   \item  WW       = Weercode (00-99), visueel(WW) of automatisch(WaWa) waargenomen, voor het actuele weer of het weer in het afgelopen uur. Zie http://bibliotheek.knmi.nl/scholierenpdf/weercodes_Nederland;
#'   \item  IX       = Weercode indicator voor de wijze van waarnemen op een bemand of automatisch station (1=bemand gebruikmakend van code uit visuele waarnemingen, 2,3=bemand en weggelaten (geen belangrijk weersverschijnsel, geen gegevens), 4=automatisch en opgenomen (gebruikmakend van code uit visuele waarnemingen), 5,6=automatisch en weggelaten (geen belangrijk weersverschijnsel, geen gegevens), 7=automatisch gebruikmakend van code uit automatische waarnemingen);
#'   \item  M        = Mist 0=niet voorgekomen, 1=wel voorgekomen in het voorgaande uur en/of tijdens de waarneming;
#'   \item  R        = Regen 0=niet voorgekomen, 1=wel voorgekomen in het voorgaande uur en/of tijdens de waarneming;
#'   \item  S        = Sneeuw 0=niet voorgekomen, 1=wel voorgekomen in het voorgaande uur en/of tijdens de waarneming;
#'   \item  O        = Onweer 0=niet voorgekomen, 1=wel voorgekomen in het voorgaande uur en/of tijdens de waarneming;
#'   \item  Y        = IJsvorming 0=niet voorgekomen, 1=wel voorgekomen in het voorgaande uur en/of tijdens de waarneming;
#' }
#' @keywords historic weather data by hour
#' @export
#'
get_hourly_data <-
  function(stationID = "260",
           from,
           to) {

    stop("Sorry, this function is temporarily unavailable as the KNMI changed their API.")

    # try to parse date-parameters
    if (missing(from))
      from <-
        (lubridate::today() - 7) %>% # one week ago
        as.character() %>%
        stringr::str_remove_all(pattern = "-") %>%
        stringr::str_glue(., "01")
    if (missing(to))
      to <-
        (lubridate::today() - 1) %>% # yesterday
        as.character() %>%
        stringr::str_remove_all(pattern = "-") %>%
        stringr::str_glue(., "24")

    if (!is.character(from) | !is.character(to) | stringr::str_length(from) %% 2 == 1 | stringr::str_length(to) %% 2 == 1) {
      stop("The values for 'from' and 'to' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM', 'YYYYMMDD' or 'YYYYMMDDHH'.")
    } else {
        if (stringr::str_length(from) == 8) {
          from_date <- paste0(from, "01")
        } else if (stringr::str_length(from) == 6) {
          from_date <- paste0(from, "0101")
        } else if (stringr::str_length(from) == 4) {
          from_date <- paste0(from, "010101")
        } else {
          from_date <- from
        }

        if (stringr::str_length(to) == 10)
          to_date <- to
        else {
          if (stringr::str_length(to) == 6)
            to <- paste0(to, "01")
          else if (stringr::str_length(to) == 4)
            to <- paste0(to, "1231")

          to_date <-
            lubridate::ymd(to) %>%
            lubridate::ceiling_date(unit = "month") - 1
          to_date <-
            to_date %>%
            as.character() %>%
            stringr::str_remove_all(pattern = "-") %>%
            paste0(., "24")
        }
    }
    if (as.numeric(to) < as.numeric(from))
      stop("The values for 'from' and 'to' could not be parsed into dates where 'from' <= 'to'.")

    #
    # ToDo: download data from https://www.daggegevens.knmi.nl/klimatologie/uurgegevens
    #
   baseURL <- "http://projects.knmi.nl/klimatologie/uurgegevens/getdata_uur.cgi"
   params <- "ALL"
   URL <- paste(baseURL, "?start=", from, "&end=", to, "&stns=", stationID,"&", params, sep = "")

   hourly_data <-
     readr::read_csv(URL, col_names = FALSE, comment = "#") %>%
     dplyr::as_tibble()

   column_names <-
      c("STN", "YYYYMMDD", "HH", "DD", "FH", "FF", "FX", "T", "T10N", "TD", "SQ", "Q", "DR", "RH", "P", "VV", "N", "U", "WW", "IX", "M", "R", "S", "O", "Y")

   colnames(hourly_data) <-
      column_names

   return(hourly_data)
}



#' @title get KNMI climate hourly data.
#'
#' @description
#' \code{get_climate_hour_data_api} retrieves KNMI hourly data through the KNMI-API.
#'
#' @details
#' Depricated function. Will be removed in the next release. Please use '\code{get_hourly_data}' instead.
#'
#' @param stationID ID for the KNMI measurement station. The available stations can be retrieved with the function \code{list_stations}. Defaults to "260" (De Bilt). . Note: a string of characters in the format 'iii', with 'iii' the number of the station of 'ALL'.
#' @param from startdate and hour for the time-window. Defaults to the start of the current year. Note: a string of characters in the format 'yyyymmddhh'.
#' @param to enddate and hour for the time-window. Defaults to yesterday. Note: a string of characters in the format 'yyyymmddhh'.
#' @return a tibble.
#' @keywords historic weather data by hour
#' @export
#'
get_climate_hour_data_api <-
  function(stationID = "ALL",
           from = paste(format(Sys.Date(), format = "%Y"), "0101", sep = ""),
           to = format(Sys.Date()-1, format = "%Y%m%d")) {

    print("Depricated function. Please use 'get_hourly_data' instead.")

    hourly_data <- get_hourly_data(stationID, from, to)

    return(hourly_data)
}
