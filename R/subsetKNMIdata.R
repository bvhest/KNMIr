#' @title subset the KNMI data.
#'
#' @description
#' \code{subset_KNMI_data} returns a filtered subset of the KNMI data-set.
#'
#' @details
#' The function \code{subset_KNMI_data} can only be applied to the raw measurement data obtained with the functions
#' \code{\link{get_climate_data_api}}, \code{\link{get_climate_data_zip}}, \code{\link{get_6day_weather_forecast}}.
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
subset_KNMI_data <- function(data,
                             startyear = 2006,
                             variables = c("FG","TG","TN","TX","SQ","SP","Q","RH","NG")) {

   # subset on required variables
   data <- data[, c("STN","YYYYMMDD", variables)]
   # subset on required years
   data <- data[data$YYYYMMDD >= paste(startyear,"0101", sep=""), ]

   # add some usefull columns and transform values into SI-units.
   return(tidyKNMIdata(data))
}

tidyKNMIdata <- function(data) {

   # 3) separate date-string into year, month, day
   data$date <- as.Date(as.character(data$YYYYMMDD), format="%Y%m%d")
   data$doy <- yday(data$date)
   data$year <- year(data$date)
   data$month <- month(data$date)
   data$week <- week(data$date)
   data$day <- wday(data$date)

   # ugly, but working...
   # 4a) convert temp to degrees Celcius
   if ("gemTemp" %in% names(data)) {data$gemTemp <- data$gemTemp/10}
   if ("minTemp" %in% names(data)) {data$minTemp <- data$minTemp/10}
   if ("maxTemp" %in% names(data)) {data$maxTemp <- data$maxTemp/10}
   if ("minTemp10cm" %in% names(data)) {data$minTemp10cm <- data$minTemp10cm/10}

   if ("TG" %in% names(data)) {data$TG <- data$TG/10}
   if ("TN" %in% names(data)) {data$TN <- data$TN/10}
   if ("TX" %in% names(data)) {data$TX <- data$TX/10}
   if ("T10N" %in% names(data)) {data$T10N <- data$T10N/10}
   # 4b) convert neerslag to ml/m2 en verwijder de negatieve waarden:
   if ("dagTotaalNeerslag" %in% names(data)) {data$dagTotaalNeerslag <- pmax(data$dagTotaalNeerslag/10,0)}
   if ("maxUurNeerslag"  %in% names(data)) {data$maxUurNeerslag <- pmax(data$maxUurNeerslag/10,0)}

   if ("RH" %in% names(data)) {data$RH <- pmax(data$RH/10,0)}
   if ("RHX"  %in% names(data)) {data$RHX <- pmax(data$RHX/10,0)}
   # 4c) convert zonneschijn naar uren
   if ("zon" %in% names(data)) {data$zon <- data$zon/10}

   if ("SQ" %in% names(data)) {data$SQ <- data$SQ/10}
   # 4d) convert luchtdruk naar hPa
   if ("gemLuchtdruk" %in% names(data)) {data$gemLuchtdruk <- data$gemLuchtdruk/10}
   if ("maxUurLuchtdruk" %in% names(data)) {data$maxUurLuchtdruk <- data$maxUurLuchtdruk/10}
   if ("minUurLuchtdruk" %in% names(data)) {data$minUurLuchtdruk <- data$minUurLuchtdruk/10}

   if ("PG" %in% names(data)) {data$PG <- data$PG/10}
   if ("PX" %in% names(data)) {data$PX <- data$PX/10}
   if ("PN" %in% names(data)) {data$PN <- data$PN/10}
   # 4e) convert Referentiegewasverdamping naar mm
   if ("refGewasverdamping" %in% names(data)) {data$refGewasverdamping <- data$refGewasverdamping/10}

   if ("EV24" %in% names(data)) {data$EV24 <- data$EV24/10}
   # 4f) convert windkracht naar m/s
   if ("gemWind" %in% names(data)) {data$gemWind <- data$gemWind/10}
   if ("maxWind" %in% names(data)) {data$maxWind <- data$maxWind/10}
   if ("minWind" %in% names(data)) {data$minWind <- data$minWind/10}
   if ("maxWindstoot" %in% names(data)) {data$maxWindstoot <- data$maxWindstoot/10}
   if ("Vectorgemiddeldewindsnelheid" %in% names(data)) {data$Vectorgemiddeldewindsnelheid <- data$Vectorgemiddeldewindsnelheid/10}

   if ("FG" %in% names(data)) {data$FG <- data$FG/10}
   if ("FHX" %in% names(data)) {data$FHX <- data$FHX/10}
   if ("FHN" %in% names(data)) {data$FHN <- data$FHN/10}
   if ("FXX" %in% names(data)) {data$FXX <- data$FXX/10}
   if ("FHVEC" %in% names(data)) {data$FHVEC <- data$FHVEC/10}

   return(data)
}
