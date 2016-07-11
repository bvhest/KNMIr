# library(lubridate)
# library(data.table)

#' subset KNMI data-set
#'
#' This function returns a cleaned and renamed subset of a KNMI data-set.
#' It provides meaningfull names for the different variables and converts these to SI-units.
#'
#' @param data data-frame with KNMI-data that has been obtaines with the function 'retrieveHistoricData' or 'retrieveDataRange'.
#' @param jaar start-year for the selection. Default is 2006.
#' @param variabelen list with variables that should be returned. Default is ("FG","TG","TN","TX","SQ","SP","Q","RH","NG").
#'
#' @return data-frame met subset van de KNMI-data.
#' @format The default data frame contains the following columns:
#' \itemize{
#'   \item stationID         = ID of measurementstation;
#'   \item datum             = Datum (YYYY=jaar MM=maand DD=dag);
#'   \item gemWind           = Etmaalgemiddelde windsnelheid (in m/s);
#'   \item gemTemp           = Etmaalgemiddelde temperatuur (in graden Celsius);
#'   \item minTemp           = Minimum temperatuur (in graden Celsius);
#'   \item maxTemp           = Maximum temperatuur (in graden Celsius);
#'   \item zon               = Zonneschijnduur (in uur) berekend uit de globale straling (-1 voor <0.05 uur);
#'   \item percZon           = Percentage van de langst mogelijke zonneschijnduur;
#'   \item straling          = Globale straling (in J/cm2);
#'   \item dagTotaalNeerslag = Etmaalsom van de neerslag (in mm) (-1 voor <0.05 mm);
#'   \item gemBewolking      = Etmaalgemiddelde bewolking (bedekkingsgraad van de bovenlucht in achtsten, 9=bovenlucht onzichtbaar);
#' }
#' @export
#'
subsetKNMIdata <- function(data,
                           jaar = 2006,
                           variabelen = c("FG","TG","TN","TX","SQ","SP","Q","RH","NG")) {

   # subset on required variables
   data <- data[, c("STN","YYYYMMDD", variabelen)]
   # subset on required years
   data <- data[data$YYYYMMDD >= paste(jaar,"0101", sep=""), ]
   # provide more meaninfull column names:
   KNMIkolomnamen <- c("STN","YYYYMMDD","DDVEC","FHVEC",
                       "FG","FHX","FHXH","FHN","FHNH","FXX","FXXH",
                       "TG","TN","TNH","TX","TXH","T10N","T10NH",
                       "SQ","SP","Q","DR",
                       "RH","RHX","RHXH","EV24",
                       "PG","PX","PXH","PN","PNH",
                       "VVN","VVNH","VVX","VVXH",
                       "NG",
                       "UG","UX","UXH","UN","UNH")
   nieuwekolomnamen <- c("stationID", "datum", "VectorgemiddeldeWindrichting", "Vectorgemiddeldewindsnelheid",
                         "gemWind","maxWind", "uurMaxWind", "minWind", "uurMinWind", "maxWindstoot", "uurMaxWindstoot",
                         "gemTemp", "minTemp", "uurMinTemp", "maxTemp", "uurMaxTemp", "minTemp10cm", "dagdeelMinTemp10cm",
                         "zon", "percZon", "straling", "duurNeerslag",
                         "dagTotaalNeerslag", "maxUurNeerslag","uurUurNeerslag","refGewasverdamping",
                         "gemLuchtdruk", "maxUurLuchtdruk", "uurMaxUurLuchtdruk", "minUurLuchtdruk", "uurMinUurLuchtdruk",
                         "minZicht", "uurMinZicht","maxZicht", "uurMaxZicht",
                         "gemBewolking",
                         "gemRelVocht","maxRelVocht","uurMaxRelVocht","minRelVocht","uurMinRelVocht"
                         )
   Setnames(data, old = KNMIkolomnamen, new = nieuwekolomnamen)

   # add some usefull columns and transform values into SI-units.
   tidyKNMIdata(data)
}


tidyKNMIdata <- function(data) {

   # 3) separate date-string into year, month, day
   data$datum <- as.Date(as.character(data$datum), format="%Y%m%d")
   data$dagVjaar <- yday(data$datum)
   data$jaar <- year(data$datum)
   data$mnd <- month(data$datum)
   data$week <- week(data$datum)
   data$dag <- day(data$datum)

   # 4a) convert temp to degrees Celcius
   if ("gemTemp" %in% names(data)) {data$gemTemp <- data$gemTemp/10}
   if ("minTemp" %in% names(data)) {data$minTemp <- data$minTemp/10}
   if ("maxTemp" %in% names(data)) {data$maxTemp <- data$maxTemp/10}
   if ("minTemp10cm" %in% names(data)) {data$minTemp10cm <- data$minTemp10cm/10}
   # 4b) convert neerslag to ml/m2 en verwijder de negatieve waarden:
   if ("dagTotaalNeerslag" %in% names(data)) {data$dagTotaalNeerslag <- pmax(data$dagTotaalNeerslag/10,0)}
   if ("maxUurNeerslag"  %in% names(data)) {data$maxUurNeerslag <- pmax(data$maxUurNeerslag/10,0)}
   # 4c) convert zonneschijn naar uren
   if ("zon" %in% names(data)) {data$zon <- data$zon/10}
   # 4d) convert windsnelheid naar m/s
   if ("gemWind" %in% names(data)) {data$gemWind <- data$gemWind/10}
   # 4d) convert luchtdruk naar hPa
   if ("gemLuchtdruk" %in% names(data)) {data$gemLuchtdruk <- data$gemLuchtdruk/10}
   if ("maxUurLuchtdruk" %in% names(data)) {data$maxUurLuchtdruk <- data$maxUurLuchtdruk/10}
   if ("minUurLuchtdruk" %in% names(data)) {data$minUurLuchtdruk <- data$minUurLuchtdruk/10}
   # 4d) convert Referentiegewasverdamping naar mm
   if ("refGewasverdamping" %in% names(data)) {data$refGewasverdamping <- data$refGewasverdamping/10}
   # 4e) convert windkracht naar m/s
   if ("gemWind" %in% names(data)) {data$gemWind <- data$gemWind/10}
   if ("maxWind" %in% names(data)) {data$maxWind <- data$maxWind/10}
   if ("minWind" %in% names(data)) {data$minWind <- data$minWind/10}
   if ("maxWindstoot" %in% names(data)) {data$maxWindstoot <- data$maxWindstoot/10}
   if ("Vectorgemiddeldewindsnelheid" %in% names(data)) {data$Vectorgemiddeldewindsnelheid <- data$Vectorgemiddeldewindsnelheid/10}

   # return
   data
}

# function to rename column-names when not all columns are present.
# source: http://stackoverflow.com/questions/29380447/using-data-tablesetnames-when-some-column-names-might-not-be-present
Setnames <- function(x, old, new, allow.absent.cols=TRUE) {
   if (!allow.absent.cols) {
      setnames(x, old, new)
   } else {
      ix <- match(names(x), old, 0L)
      setnames(x, old[ix], new[ix])
   }
}
