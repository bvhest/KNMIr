#' @title find the nearest KNMI station
#'
#' @description
#' \code{find_nearest_KNMI_station} returns the KNMI measurement station that
#' is closed to the provided location.
#'
#' @details
#' One can select from the active stations (active = TRUE,
#' the default) or all stations (active = FALSE).
#'
#' @param lat latitude of the location (using the WGS84 coordinate system).
#' @param lon longitude of the location (using the WGS84 coordinate system).
#' @param active boolean to select only currently active stations. Default = TRUE.
#' @param temperature_sensor filter on stations that (don't) provide temperature
#'   data. Default = TRUE. Note that the other set often only returns wind-data.
#' @return data-frame with the id, name, url to station information and the
#'   lat/lon of the nearest KNMI-station.
#' @export
#'
find_nearest_KNMI_station <-
  function(lat, lon,
           active = TRUE,
           temperature_sensor = TRUE) {

    # perform some sanity checks on the input
    if(length(lat) > 1 | length(lon) > 1)
      stop("The values for 'lon' and 'lat' must contain a single location/value.")
    if(!(is.numeric(lat) && is.numeric(lon)))
      stop("The values for 'lon' and 'lat' must contain numerical values.")
    if(abs(lat) > 90 || abs(lon) > 180)
      stop("The latitude must be between -90 and 90, the longitude between -180 and 180.")

    # store lat/lon into dataframe
    location <-
      data.frame(lon = lon, lat = lat)

    # load data of KNMI-stations
    utils::data("stations")

    distance.to.stations <-
      stations %>%
      # filter on (in)active stations:
      # if the einddatum (enddate) has a value, this value is always in the past
      # and the station is inactive, if it's NA the station is active.
      dplyr::filter(is.na(einddatum) == active) %>%
      # filter on stations with(out) temperature sensor:
      dplyr::filter(temp_sensor == temperature_sensor) %>%
      dplyr::mutate(distance = earth.distance(lat, lon, location$lat, location$lon))

    result <-
      distance.to.stations %>%
      dplyr::arrange(distance) %>%
      dplyr::select(stationID, plaats, lat, lon, info) %>%
      dplyr::slice(1L)

    return(result)
  }

#' @title find the nearest KNMI station
#'
#' @description
#' \code{find_nearest_KNMI_station2} returns the KNMI measurement station that
#' is closed to the provided location.
#'
#' @details
#' One can select from the active stations (active = TRUE,
#' the default) or all stations (active = FALSE).
#'
#' @param location data-frame with a lat/lon-column (using the WGS84 coordinate
#'   system).
#' @param active boolean to select only currently active stations. Default =
#'   TRUE.
#' @param temperature_sensor filter on stations that (don't) provide temperature
#'   data. Default = TRUE. Note that the other set often only returns wind-data.
#' @return data-frame with the id, name, url to station information and the
#'   lat/lon of the nearest KNMI-station.
#' @import dplyr
#' @export
#'
find_nearest_KNMI_station2 <-
  function(location,
           active = TRUE,
           temperature_sensor = TRUE) {

    # perform some sanity checks
    if(nrow(location) > 1)
      stop("The location-parameter must contain a single location, not ", length(location), " locations.")
    if(!(is.numeric(location$lat) && is.numeric(location$lon)))
      stop("The values for 'lon' and 'lat' must contain numerical values.")
    if(abs(location$lat) > 90 || abs(location$lon) > 180)
      stop("The latitude must be between -90 and 90, the longitude between -180 and 180.")

    return(find_nearest_KNMI_station(location$lat, location$lon,
                                     active, temperature_sensor))
  }
