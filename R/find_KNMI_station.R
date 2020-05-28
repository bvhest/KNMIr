#' @title find the nearest KNMI station
#'
#' @description
#' \code{find_nearest_KNMI_station} returns the KNMI measurement station that is closed to the
#' provided location.
#'
#' @details
#' One can select from the active stations (active = TRUE,
#' the default) or all stations (active = FALSE).
#'
#' @param lat latitude of the location (using the WGS84 coordinate system).
#' @param lon longitude of the location (using the WGS84 coordinate system).
#' @param active boolean to select only currently active stations. Default =
#'   TRUE.
#' @return data-frame with the id, name, url to station information and the
#'   lat/lon of the nearest KNMI-station.
#' @export
#'
find_nearest_KNMI_station <-
  function(lat, lon, active = TRUE) {

    # perform some sanity checks on the input
    if(!(is.numeric(lat) & is.numeric(lon)))
      stop("The values for 'lon' and 'lat' must contain numerical.")
    if(length(lat) > 1 | length(lon) > 1)
      stop("The values for 'lon' and 'lat' must contain a single location/value.")
    if(abs(lat) > 90 | abs(lon) > 180)
      stop("The latitude must be between -90 and 90, the longitude between -180 and 180.")

    # store lat/lon into dataframe
    location <-
      data.frame(lon = lon, lat = lat)

    # load data of KNMI-stations
    utils::data("stations")

    # find the nearest station
    # distances <-
    #   earth.distance(stations, location) %>%
    #   as.data.frame()
    # names(distances)[1] <- "distance"
    # distance.to.stations <- cbind(stations, distances)

    distance.to.stations <-
      stations %>%
      dplyr::mutate(distance = earth.distance(lon, lat, location$lat, location$lon))

    if (active) {
      # if the einddatum (enddate) has a value, this value is always in the past
      # and the station is inactive, if it's NA the station is active.
      distance.to.stations <-
        distance.to.stations %>%
        dplyr::filter(is.na(einddatum))
    }

    result <- distance.to.stations %>%
      dplyr::arrange(distance) %>%
      dplyr::select(stationID, plaats, lat, lon, info) %>%
      dplyr::slice(1L)

    return(result)
  }

# define helper functions:
#   distance in kilometers between two long/lat positions (from "fossil" package)
earth.distance <-
  function (lat1, lon1, lat2, lon2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c

  return(d)
}
