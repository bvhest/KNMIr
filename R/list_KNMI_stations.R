#' @title list KNMI measurement stations.
#'
#' @description
#' \code{list_stations} list the id's and names of the KNMI measurement
#' stations and if they are active or not.
#' Data from https://www.knmi.nl/nederland-nu/klimatologie/daggegevens,
#' http://projects.knmi.nl/klimatologie/metadata/index.html
#'
#' @param is_active boolean to select only currently active stations. Default =
#'   TRUE.
#' @param  identifying_columns boolean to return only the staion identifying
#' columns (including if a station is active). Default = FALSE (i.e. returning
#' all columns).
#' @return a tibble.
#' @format The returned data-frame contains the following columns: \itemize{
#'   \item station  = ID of measurement station; \item plaats   = city closest
#'   to the measurement station; \item active	  = indicates if the station is
#'   still active; }
#' @keywords list weather stations
#' @export
list_stations <-
  function(is_active = TRUE, identifying_columns = FALSE) {

    utils::data(stations)

    selected_stations <-
      stations %>%
      dplyr::mutate(active = is.na(einddatum)) %>%
      dplyr::filter(if(is_active) active else TRUE) %>%
      as_tibble()

    if (identifying_columns)
      selected_stations <-
        selected_stations %>%
        dplyr::select(stationID, plaats, active)

    return(selected_stations)
  }
