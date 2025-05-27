#' @title list KNMI measurement stations.
#'
#' @description
#' \code{list_stations} list the id's and names of the KNMI measurement
#' stations and if they are active or not.
#' Data from https://www.knmi.nl/nederland-nu/klimatologie/daggegevens,
#' http://projects.knmi.nl/klimatologie/metadata/index.html
#'
#' @param active boolean to select only currently active stations. Default =
#'   TRUE.
#' @param temperature_sensor filter on stations that (don't) provide temperature
#' data. Default = TRUE. Note that the other set often only returns wind-data.
#' @param  identifying_columns boolean to return only the station identifying
#' columns (including if a station is active). Default = FALSE (i.e. returning
#' all columns).
#' @return a tibble.
#' @format The returned data-frame contains the following columns: \itemize{
#'   \item station  = ID of measurement station; \item plaats   = city closest
#'   to the measurement station; \item active	  = indicates if the station is
#'   still active; }
#' @keywords list weather stations
#' @export
list_stations <- function(active = TRUE, temperature_sensor = TRUE, identifying_columns = FALSE) {
  utils::data(stations)

  selected_stations <- stations %>%
    # filter on (in)active stations:
    # if the einddatum (enddate) has a value, this value is always in the past
    # and the station is inactive, if it's NA the station is active.
    dplyr::filter(is.na(einddatum) == active) %>%
    # filter on stations with(out) temperature sensor:
    dplyr::filter(temp_sensor == temperature_sensor) %>%
    dplyr::as_tibble()

  if (identifying_columns) {
    selected_stations <- selected_stations %>%
      dplyr::mutate(active = is.na(einddatum)) %>%
      dplyr::select(stationID, plaats, active)
  }

  return(selected_stations)
}

