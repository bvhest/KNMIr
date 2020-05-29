#' @title rename columns in the raw KNMI dataset.
#'
#' @description
#' \code{rename_columns_KNMI_data} provides more readable names for the different measurement variables.
#'
#' @param data data-frame with KNMI-data that has been obtained with one of the function \code{\link{get_daily_data}},
#' \code{\link{get_daily_data_from_prepared_zip}}, \code{\link{get_daily_data}}, \code{\link{get_6day_weather_forecast}},
#' \code{\link{get_14day_weather_forecast}} or \code{\link{subset_KNMI_data}}.
#'
#' @return data-frame with subset of the KNMI-data.
#' @export
#'
rename_columns_KNMI_data <-
  function(daily_data) {

    # provide more meaninfull column names:
    KNMI_kolomnamen <-
      c("STN","station","YYYYMMDD","date","DDVEC","FHVEC",
        "FG","FHX","FHXH","FHN","FHNH","FXX","FXXH",
        "TG","TN","TNH","TX","TXH","T10N","T10NH",
        "SQ","SP","Q","DR",
        "RH","RHX","RHXH","EV24",
        "PG","PX","PXH","PN","PNH",
        "VVN","VVNH","VVX","VVXH",
        "NG",
        "UG","UX","UXH","UN","UNH",
        "SD_minTemp","SD_gemTemp","SD_maxTemp","SD_neerslag",
        "neerslagKans","windkracht","doy", "year", "month","week","day")
    beschrijvende_kolomnamen <-
      c("stationID", "stationNaam", "YYYYMMDD", "datum", "VectorgemiddeldeWindrichting", "Vectorgemiddeldewindsnelheid",
        "gemWind","maxWind", "uurMaxWind", "minWind", "uurMinWind", "maxWindstoot", "uurMaxWindstoot",
        "gemTemp", "minTemp", "uurMinTemp", "maxTemp", "uurMaxTemp", "minTemp10cm", "dagdeelMinTemp10cm",
        "zon", "percZon", "straling", "duurNeerslag",
        "dagTotaalNeerslag", "maxUurNeerslag","uurUurNeerslag","refGewasverdamping",
        "gemLuchtdruk", "maxUurLuchtdruk", "uurMaxUurLuchtdruk", "minUurLuchtdruk", "uurMinUurLuchtdruk",
        "minZicht", "uurMinZicht","maxZicht", "uurMaxZicht",
        "gemBewolking",
        "gemRelVocht","maxRelVocht","uurMaxRelVocht","minRelVocht","uurMinRelVocht",
        "standaardDev_minTemp","standaardDev_gemTemp","standaardDev_maxTemp","standaardDev_neerslag",
        "neerslagKans","windkracht","dogVanJaar", "jaar", "maand","week","dag")

    daily_data <-
     setNames(daily_data,
              col.from = KNMI_kolomnamen,
              col.to = beschrijvende_kolomnamen)

    return(daily_data)
}

# **********************************************************************************************************************
#
# hulp-functies
#
# **********************************************************************************************************************

# function to rename column-names when not all columns are present.
#
# source: http://stackoverflow.com/questions/29380447/using-data-tablesetnames-when-some-column-names-might-not-be-present
#
setNames <-
  function(x,
           col.from,
           col.to,
           allow.absent.cols = TRUE) {

    # r <-
    #   x %>%
    #   dplyr::rename_at(vars(all_of(col.from)), ~col.to)

    if (!allow.absent.cols) {
      r <- data.table::setnames(x, col.from, col.to)
    } else {
      ix <- match(names(x), col.from, 0L)
      r <- data.table::setnames(x, col.from[ix], col.to[ix])
    }

    return(r)
  }
