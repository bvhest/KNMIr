#' rename Columns KNMI data
#'
#' This function provides meaningfull names for the different variables.
#'
#' @param data data-frame with KNMI-data that has been obtained with the function 'getClimateDateSet()',
#' 'getClimateDateInBulk()' or 'subsetKNMIdata()'.
#'
#' @return data-frame met subset van de KNMI-data.
#' @export
#'
renameColumnsKNMIdata <- function(data) {

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
   setNames(data, old = KNMIkolomnamen, new = nieuwekolomnamen)

   return(data)
}

# function to rename column-names when not all columns are present.
# source: http://stackoverflow.com/questions/29380447/using-data-tablesetnames-when-some-column-names-might-not-be-present
setNames <- function(x, old, new, allow.absent.cols = TRUE) {
   if (!allow.absent.cols) {
      r <- setnames(x, old, new)
   } else {
      ix <- match(names(x), old, 0L)
      r <- setnames(x, old[ix], new[ix])
   }
  return(r)
}
