#' @title subset the KNMI data.
#'
#' @description
#' \code{subset_KNMI_data} returns a filtered subset of the KNMI data-set.
#'
#' @details
#' The function \code{subset_KNMI_data} can only be applied to the raw measurement data obtained with the functions
#' \code{\link{get_daily_data}}, \code{\link{get_hourly_data}},
#' \code{\link{get_daily_data_from_prepared_zip}}, \code{\link{get_6day_weather_forecast}} and
#' \code{\link{get_14day_weather_forecast}}. This
#'
#' It's a convenience function; the results can also be obtained in other ways.
#'
#' The filtering is two-fold; select the most-used variables from the complete set of variables and
#' include the data from the provided start-year upward to the most currrent date.
#'
#' More importantly, this function modifies the column values to SI-units and it adds some helper-columns, like day-of-the-year,
#' weeknumber, daynumner, year.
#'
#' @param data data-frame with KNMI-data that has been obtained with the function 'getClimateDateSet' or 'getClimateDateInBulk'.
#' @param startyear start-year for the selection. Default is 1901 Note that the end-year is always the most current year in the data-set.
#' @param variables list with variables that should be returned from the data-frame. Default is ("FG","TG","TN","TX","SQ","SP","Q","RH","NG").
#'
#' @return data-frame met subset van de KNMI-data.
#' @format The default data frame contains the following columns:
#' \itemize{
#'   \item stationID              = ID of measurementstation;
#'   \item datum                  = Datum (YYYY=jaar MM=maand DD=dag);
#'   \item FG : gemWind           = Etmaalgemiddelde windsnelheid (in m/s);
#'   \item TG : gemTemp           = Etmaalgemiddelde temperatuur (in graden Celsius);
#'   \item TN : minTemp           = Minimum temperatuur (in graden Celsius);
#'   \item TX : maxTemp           = Maximum temperatuur (in graden Celsius);
#'   \item SQ : zon               = Zonneschijnduur (in uur) berekend uit de globale straling (-1 voor <0.05 uur);
#'   \item SP : percZon           = Percentage van de langst mogelijke zonneschijnduur;
#'   \item Q  : straling          = Globale straling (in J/cm2);
#'   \item RH : dagTotaalNeerslag = Etmaalsom van de neerslag (in mm) (-1 voor <0.05 mm);
#'   \item NG : gemBewolking      = Etmaalgemiddelde bewolking (bedekkingsgraad van de bovenlucht in achtsten, 9=bovenlucht onzichtbaar);
#' }
#' @export
#'
subset_data <- function(data, startyear = 1901, variables = c("FG", "TG", "TN", "TX", "SQ", "SP", "Q", "RH", "NG")) {
  # ToDO: check params
  start_date <- lubridate::ymd(paste0(startyear, "0101"))

  data <- data %>% # add some usefull columns and transform measurement values into SI-units.
    tidy_data(.) %>% # subset on required years
    dplyr::filter(date >= start_date) # [data$date >= start_date, ]

  #    data <- tidy_data(data)

  # subset on required variables
  variables <- c(c("STN", "YYYYMMDD", "date", "doy", "year", "month", "week", "day"), variables)
  data <- data[, variables]

  return(data)
}

#' @title subset the KNMI data.
#'
#' @description
#' \code{subset_KNMI_data} returns a filtered subset of the KNMI data-set.
#'
#' Depricated function. Please use '\code{subset_data}' instead.
#'
#' @param data data-frame with KNMI-data that has been obtained with the function 'getClimateDateSet' or 'getClimateDateInBulk'.
#' @param startyear start-year for the selection. Default is 2006. Note that the end-year is always the most current year in the data-set.
#' @param variables list with variables that should be returned from the data-frame. Default is ("FG","TG","TN","TX","SQ","SP","Q","RH","NG").
#'
#' @return data-frame met subset van de KNMI-data.
#' @format The default data frame contains the following columns:
#' \itemize{
#'   \item stationID              = ID of measurementstation;
#'   \item datum                  = Datum (YYYY=jaar MM=maand DD=dag);
#'   \item FG : gemWind           = Etmaalgemiddelde windsnelheid (in m/s);
#'   \item TG : gemTemp           = Etmaalgemiddelde temperatuur (in graden Celsius);
#'   \item TN : minTemp           = Minimum temperatuur (in graden Celsius);
#'   \item TX : maxTemp           = Maximum temperatuur (in graden Celsius);
#'   \item SQ : zon               = Zonneschijnduur (in uur) berekend uit de globale straling (-1 voor <0.05 uur);
#'   \item SP : percZon           = Percentage van de langst mogelijke zonneschijnduur;
#'   \item Q  : straling          = Globale straling (in J/cm2);
#'   \item RH : dagTotaalNeerslag = Etmaalsom van de neerslag (in mm) (-1 voor <0.05 mm);
#'   \item NG : gemBewolking      = Etmaalgemiddelde bewolking (bedekkingsgraad van de bovenlucht in achtsten, 9=bovenlucht onzichtbaar);
#' }
#' @export
#'
subset_KNMI_data <- function(data, startyear = 2006, variables = c("FG", "TG", "TN", "TX", "SQ", "SP", "Q", "RH", "NG")) {
  print("Depricated function. Please use 'subset_data' instead.")

  return(subset_data(data))
}

