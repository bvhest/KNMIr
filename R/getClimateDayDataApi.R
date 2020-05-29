#' @title get daily KNMI climate data.
#'
#' @description
#' \code{get_daily_data} retrieves KNMI data through the KNMI-API.
#'
#' @details
#' This function retrieves raw climate data collected by the official KNMI measurement stations for a specific station
#' and/or date-range. It uses the, somewhat slower, KNMI-API to collect the data.
#' The function \code{\link{get_daily_data_from_prepared_zip}} in this package is optimized to collect data for larger
#' date-ranges (e.g. for > 10 years), but is less flexible with combinations of all or specific stations and date ranges.
#'
#' You can specify a specific station or get data from all the stations at once (the default).
#' When the from and to date parameters are not proviced, all measurements for the current year are returned. Otherwise
#' the data is subsetted to the given interval.
#'
#' The original KNMI API is described on the web-page \href{https://www.knmi.nl/kennis-en-datacentrum/achtergrond/data-ophalen-vanuit-een-script}{collecting data through a script}.
#'
#' Note: this function only works for the land-based measurement stations, so not for the stations in the North Sea, as
#'       the API does not expose these.
#'
#' @param stationID ID for the KNMI measurement station. The available stations can be retrieved with the function 'getStations()'. Defaults to "all". . Note: a string of characters in the format 'iii'.
#' @param from startdate for the time-window. Defaults to start of current year. A string of characters in the format 'yyyymmdd', 'yyyymm' or 'yyyy'. Missing digits are replaced with the first day of the month and/or the first month of the year.
#' @param to enddate for the time-window. Defaults to yesterday (most recent data provided by the KNMI). A string of characters in the format 'yyyymmdd', 'yyyymm' or 'yyyy'. Missing digits are replaced with the last day of the month and/or the last month of the year.
#' @return a tibble.
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
#'   \item EV24	Referentiegewasverdamping (Makkink) (in 0.1 mm);
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
#' }
#' @keywords historic weather data
#' @export
get_daily_data <-
  function(stationID = "ALL",
           from,
           to) {

    # try to parse date-parameters
    if (missing(from))
      from <-
        lubridate::today() %>%
        lubridate::year() %>%
        stringr::str_glue(., "0101")
    if (missing(to))
      to <-
        (lubridate::today() - 1) %>% # yesterday
        as.character() %>%
        stringr::str_remove_all(pattern = "-")

    if (!is.character(from) | !is.character(to) | stringr::str_length(from) %% 2 == 1 | stringr::str_length(to) %% 2 == 1) {
      stop("The values for 'from' and 'to' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM' or 'YYYYMMDD'.")
    } else {
      if (stringr::str_length(from) == 6) {
        from_date <- paste0(from, "01")
      } else if (stringr::str_length(from) == 4) {
        from_date <- paste0(from, "0101")
      } else {
        from_date <- from
      }

      if (stringr::str_length(to) == 8) {
        to_date <- to
      } else {
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
          stringr::str_remove_all(pattern = "-")
      }
    }
    if (as.numeric(to_date) < as.numeric(from_date))
      stop("The values for 'from' and 'to' could not be parsed into dates where 'from' <= 'to'.")

    baseURL <- "http://projects.knmi.nl/klimatologie/daggegevens/getdata_dag.cgi"
    params <- "ALL"
    URL <- paste0(baseURL, "?start=", from_date, "&end=", to_date, "&stns=", stationID,"&", params)

    data_daily <-
      readr::read_csv(URL, col_names = FALSE, comment = "#") %>%
      dplyr::as_tibble()

    colnames(data_daily) <-
      c("STN","YYYYMMDD","DDVEC","FHVEC","FG","FHX","FHXH","FHN","FHNH","FXX","FXXH","TG","TN","TNH","TX","TXH","T10N",
        "T10NH","SQ","SP","Q","DR","RH","RHX","RHXH","EV24","PG","PX","PXH","PN","PNH","VVN","VVNH","VVX","VVXH","NG",
        "UG","UX","UXH","UN","UNH")

    return(data_daily)
  }

#' @title get daily KNMI climate data.
#'
#' @description
#' \code{get_climate_day_data_api} retrieves KNMI data through the KNMI-API.
#'
#' @details
#' Depricated function. Please use '\code{get_daily_data}' instead.
#'
#' @param stationID ID for the KNMI measurement station. The available stations can be retrieved with the function 'getStations()'. Defaults to "all". . Note: a string of characters in the format 'iii'.
#' @param from startdate for the time-window. Defaults to start of current year. A string of characters in the format 'yyyymmdd'.
#' @param to enddate for the time-window. Defaults to yesterday. A string of characters in the format 'yyyymmdd'.
#' @return a tibble.
#' @keywords historic weather data
#' @export
get_climate_day_data_api <-
  function(stationID = "ALL",
           from = paste(format(Sys.Date(), format = "%Y"), "0101", sep = ""),
           to = format(Sys.Date()-1, format = "%Y%m%d")) {

    print("Depricated function. Please use 'get_daily_data' instead.")

    data_daily <- get_daily_data(stationID, from, to)

    return(data_daily)
  }

#' @title get daily KNMI climate data.
#'
#' @description
#' \code{get_climate_day_data_api} retrieves KNMI data through the KNMI-API.
#'
#' @details
#' Depricated function. Will be removed in the next release. Please use '\code{get_daily_data}' instead.
#'
#' @param stationID ID for the KNMI measurement station. The available stations can be retrieved with the function 'getStations()'. Defaults to "all". . Note: a string of characters in the format 'iii'.
#' @param from startdate for the time-window. Defaults to start of current year. A string of characters in the format 'yyyymmdd'.
#' @param to enddate for the time-window. Defaults to yesterday. A string of characters in the format 'yyyymmdd'.
#' @return a tibble.
#' @keywords historic weather data
#' @export
get_climate_data_api <-
  function(stationID = "ALL",
           from = paste(format(Sys.Date(), format = "%Y"), "0101", sep = ""),
           to = format(Sys.Date()-1, format = "%Y%m%d")) {

    print("Depricated function. Please use 'get_daily_data' instead.")

    data_daily <- get_daily_data(stationID, from, to)

    return(data_daily)
  }
