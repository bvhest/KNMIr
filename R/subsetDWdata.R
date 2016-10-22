# library(lubridate)
# library(data.table)

#' subset DW data-set
#'
#' This function returns a cleaned and renamed subset of a Deutscher Wetterdienst data-set.
#' It provides meaningfull names for the different variables and converts these to SI-units.
#'
#' @param data data-frame with KNMI-data that has been obtaines with the function 'retrieveHistoricDataDE' or 'retrieveDataRangeDE'.
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
subsetDWdata <- function(data,
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
   DWkolomnamen <- c("STN", "YYYYMMDD",
                     "TG",  "TX", "TN", "T10N",
                     "NG", "PG", "UG",
                     "FG" , "FXX", "RH", "SQ")
   nieuwekolomnamen <- c("stationID", "datum",
                         "gemTemp", "maxTemp", "minTemp", "minTemp10cm",
                         "gemBewolking"," gemLuchtdruk", "gemRelVocht",
                         "gemWind","maxWind", "dagTotaalNeerslag", "zon")
   Setnames(data, old = DWkolomnamen, new = nieuwekolomnamen)

   # add some usefull columns and transform values into SI-units.
   tidyDWdata(data)
}


tidyDWdata <- function(data) {

   # 3) separate date-string into year, month, day
   data$datum <- as.Date(as.character(data$datum), format="%Y%m%d")
   data$dagVjaar <- yday(data$datum)
   data$jaar <- year(data$datum)
   data$mnd <- month(data$datum)
   data$week <- week(data$datum)
   data$dag <- day(data$datum)

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
