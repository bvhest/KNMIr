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
#' @param location data-frame with a lat/lon-column (using the WGS84 coordinate
#'   system).
#' @param active boolean to select only currently active stations. Default =
#'   TRUE.
#' @return data-frame with the id, name, url to station information and the
#'   lat/lon of the nearest KNMI-station.
#' @export
#'
find_nearest_KNMI_station <- function(location, active = TRUE) {
  data("stations")
  # perform some sanity checks
  if(nrow(location) > 1)
    stop("The location-parameter must contain a single location, not ", length(location), " locations.")

  # find the nearest station
  distances <- as.data.frame(earth.distance(stations, location))
  names(distances)[1] <- "distance"
  distance.to.stations <- cbind(stations, distances)
  if (active) {
    # if the einddatum (enddate) has a value, this value is always in the past
    # and the station is inactive, if it's NA the station is active.
    result <- plyr::arrange(distance.to.stations,
                            distance.to.stations$distance)[is.na(distance.to.stations$einddatum), c("station", "plaats", "lat", "lon", "info")]
  } else {
    result <- plyr::arrange(distance.to.stations,
                            distance.to.stations$distance)[, c("station", "plaats", "lat", "lon", "info")]
  }
  return(result[1,])
}


# define helper function:
# distance in kilometers between two long/lat positions (from "fossil" package)
earth.distance <- function (pos1, pos2) {
  rad <- pi/180
  a1 <- pos1$lat * rad
  a2 <- pos1$lon * rad
  b1 <- pos2$lat * rad
  b2 <- pos2$lon * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}
