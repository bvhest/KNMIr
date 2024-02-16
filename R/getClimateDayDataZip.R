#' @title get KNMI climate daily data from zip-files.
#'
#' @description
#' \code{get_daily_data_from_prepared_zip} retrieves KNMI data by downloading prepared KNMI zip-files.
#'
#' @details
#' This function retrieves raw climate data collected by the official KNMI weather stations. It is optimised for
#' retrieving large sets of data that have been prepared by the KNMI for download. If a year in the past is selected,
#' the call is forwarded to the function \code{\link{get_daily_data}}. The function \code{\link{get_daily_data}}
#' in this package is better suited to retrieve data for very specific date-ranges.
#'
#' You can specify a specific station or get data from all the stations at once (the default).
#' When the from and to date parameters are not proviced, all measurements for the current year are returned. Otherwise
#' the data is subsetted to the given interval.
#'
#' The original KNMI API is described at the web-page \href{http://www.knmi.nl/nederland-nu/klimatologie/daggegevens}{Daggegevens van het weer in Nederland}.
#'
#' Note: this function also works for the measurement stations in the North Sea. When the parameter station = "ALL"
#'       and station_type = FALSE, the data for all stations on land and sea is returned. With station_type = TRUE
#'       (the default) only the data for the land-based stations is returned.
#'
#' @param stationID ID for the KNMI station. The available stations can be retrieved with the function 'list_stations()'. Defaults to "ALL". . Note: a string of characters in the format 'iii'.
#' @param from startdate for the time-window. Defaults to start of current year. A string of characters in the format 'yyyymmdd', 'yyyymm' or 'yyyy'. Missing digits are replaced with the first day of the month and/or the first month of the year.
#' @param to enddate for the time-window. Defaults to yesterday (most recent data provided by the KNMI). A string of characters in the format 'yyyymmdd', 'yyyymm' or 'yyyy'. Missing digits are replaced with the last day of the month and/or the last month of the year.
#' @param station_type string indicating station type. Possible values 'land', 'sea', 'both', is returned. Defaults to 'land'.
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
#'
get_daily_data_from_prepared_zip <-
  function(stationID = "ALL",
           from,
           to,
           station_type = "land") {
    # validate-parameters
    # ToDo: check if station code is valid, else issue error message.
    if (!(station_type %in% c("land", "sea", "both"))) {
      stop("The station_type must be one of 'land', 'sea' or 'both'.")
    }

    # try to parse date-parameters
    if (missing(from)) {
      from <-
        lubridate::today() %>%
        lubridate::year() %>%
        stringr::str_glue(., "0101")
    }
    if (missing(to)) {
      to <-
        (lubridate::today() - 1) %>% # yesterday
        as.character() %>%
        stringr::str_remove_all(pattern = "-")
    }

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
        if (stringr::str_length(to) == 6) {
          to <- paste0(to, "01")
        } else if (stringr::str_length(to) == 4) {
          to <- paste0(to, "1231")
        }

        to_date <-
          lubridate::ymd(to) %>%
          lubridate::ceiling_date(unit = "month") - 1
        to_date <-
          to_date %>%
          as.character() %>%
          stringr::str_remove_all(pattern = "-")
      }
    }
    if (as.numeric(to_date) < as.numeric(from_date)) {
      stop("The values for 'from' and 'to' could not be parsed into dates where 'from' <= 'to'.")
    }

    if (station_type == "land") {
      daily_data <-
        get_land_data_zip(stationID, from_date, to_date)
    } else if (station_type == "sea") {
      daily_data <-
        get_sea_data_zip(stationID, from_date, to_date)
    } else {
      land_data <-
        get_land_data_zip(stationID, from_date, to_date)
      sea_data <-
        get_sea_data_zip(stationID, from_date, to_date)

      # combine the land (possible empty) and sea-data while taking into account that the sea-based stations
      # provide less variables than the land-based stations
      daily_data <-
        land_data %>%
        dplyr::select(colnames(sea_data)) %>%
        dplyr::bind_rows(sea_data)
    }
    return(daily_data)
  }



#' @title get KNMI climate daily data from zip-files.
#'
#' @description
#' \code{get_climate_data_zip} retrieves KNMI data by downloading the prepared KNMI zip-files.
#'
#' Depricated function. Please use '\code{get_daily_data_from_prepared_zip}' instead.
#'
#' @param stationID ID for the KNMI measurement station. The available stations can be retrieved with the function 'getStations()'. Defaults to "all". . Note: a string of characters in the format 'iii'.
#' @param from startdate for the time-window. Defaults to start of current year. A string of characters in the format 'yyyymmdd'.
#' @param to enddate for the time-window. Defaults to yesterday. A string of characters in the format 'yyyymmdd'.
#' @param return_only_land boolean indicating that only the data for the land-based stations is returned. Defaults to "TRUE".
#' @return a tibble.
#' @format The returned data frame contains the following columns:
#' @keywords historic weather data
#' @export
#'
get_climate_data_zip <- function(stationID = "ALL",
                                 from = paste(format(Sys.Date(), format = "%Y"), "0101", sep = ""),
                                 to = format(Sys.Date() - 1, format = "%Y%m%d"),
                                 return_only_land = TRUE) {
  print("Depricated function. Please use 'get_daily_data_from_prepared_zip' instead.")

  if (return_only_land) {
    station_type <- "land"
  } else {
    station_type <- "both"
  }

  data_daily <- get_daily_data_from_prepared_zip(stationID, from, to, station_type)

  return(data_daily)
}



# **********************************************************************************************************************
#
# hulp-functies
#
# **********************************************************************************************************************

# ophalen (prepared zip-file) data for the land-based KNMI stations
get_land_data_zip <-
  function(stationID,
           from,
           to) {
    # check if the zip-API can be used:
    thisYear <-
      format(Sys.Date(), format = "%Y") %>%
      as.numeric()
    fromYear <-
      substr(from, 1, 4) %>%
      as.numeric()

    if (stationID == "ALL" & (thisYear - fromYear) > 5) {
      stop("A download for all stations for a period > 5 years is not permitted by the KNMI API. Suggestion: download the data in batches.")
    } else if (stationID == "ALL" & (thisYear - fromYear) > 0 & (thisYear - fromYear) <= 5) {
      # download of prepared zip-file is not possible, use the slower script-based API.
      # With an additional restriction that a download is restricted to 5 years
      #   by the KNMI API.
      daily_data <-
        get_daily_data(stationID, from, to)
    } else {
      # retrieve prepared download-files from the KNMI website
      if (stationID == "ALL" & fromYear == thisYear) {
        URL <- "http://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/jaar.zip"
        file <- "jaar.txt"
        skiplines <- 58
      } else {
        # retrieve all available data for this specific station:
        URL <- paste0("http://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_", stationID, ".zip")
        file <- paste0("etmgeg_", stationID, ".txt")
        skiplines <- 51
      }

      # download a zip file containing broadband data, save it to the working directory
      tmpdir <- tempdir()
      tmpfile <- file.path(tmpdir, basename(URL))
      if (!file.exists(tmpfile)) {
        download.file(URL,
          destfile = tmpfile,
          quiet = TRUE
        )
      }
      # unzip the file
      unzip(
        zipfile = tmpfile,
        exdir = tmpdir
      )

      daily_data <-
        readr::read_csv(
          file = tmpfile,
          col_names = TRUE,
          col_types = "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii", # 41columns
          skip = skiplines,
          show_col_types = FALSE
        ) %>%
        dplyr::as_tibble() %>%
        # correct the first column name (if the name contains '#')
        # inspired by https://stackoverflow.com/questions/43578723/conditional-replacement-of-column-name-in-tibble-using-dplyr
        # dplyr::rename(STN = '# STN') %>%
        rename_all(~ sub("# STN", "STN", .x)) %>%
        # return subset based on provided start-/end-date parameters
        dplyr::filter(YYYYMMDD >= as.numeric(from) & YYYYMMDD <= as.numeric(to))
    }

    return(daily_data)
  }

# ophalen (prepared zip-file) data for the sea-based KNMI stations
get_sea_data_zip <-
  function(stationID = "ALL",
           from,
           to) {
    base_url <- "http://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_"
    tmpdir <- tempdir()

    if (stationID == "ALL") {
      sea_based_stations <- c(
        201,
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
        321
      )
    } else {
      sea_based_stations <- stationID
    }

    daily_data <- NULL
    for (i in 1:length(sea_based_stations)) {
      URL <- paste0(base_url, sea_based_stations[i], ".zip")

      # download a zip file containing broadband data, save it to the working directory
      tmpfile <- file.path(tmpdir, basename(URL))
      if (!file.exists(tmpfile)) {
        download.file(URL,
          destfile = tmpfile,
          quiet = TRUE
        )
      }
      # unzip the file
      unzip(
        zipfile = tmpfile,
        exdir = tmpdir
      )

      # read the data into R, with "|" seperating values
      file <- paste0("etmgeg_", sea_based_stations[i], ".txt") # example: etmgeg_201.txt
      tmpfile <- file.path(tmpdir, file)

      daily_data <-
        # note: 'New names' warning becasue last column name is missing...
        readr::read_csv(
          file = tmpfile,
          col_names = TRUE,
          col_types = "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii",
          skip = 35,
          show_col_types = FALSE
        ) %>%
        dplyr::as_tibble() %>%
        # correct the first column name (if the name contains '#')
        # inspired by https://stackoverflow.com/questions/43578723/conditional-replacement-of-column-name-in-tibble-using-dplyr
        # dplyr::rename(STN = '# STN') %>%
        rename_all(~ sub("# STN", "STN", .x)) %>%
        # remove strange and empty last column 'without name'
        # inspired by https://stackoverflow.com/questions/67346417/how-to-drop-columns-with-column-names-that-contain-specific-string
        # dplyr::select(-"...32") %>%
        dplyr::select(-contains("32")) %>%
        # return subset based on provided start-/end-date parameters
        dplyr::filter(YYYYMMDD >= as.numeric(from) & YYYYMMDD <= as.numeric(to)) %>%
        dplyr::bind_rows(daily_data)
    }

    return(daily_data)
  }
