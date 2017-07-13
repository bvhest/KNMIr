#' print KNMI measurement stations
#'
#' Simple helper function to print the id's and names of the KNMI measurement
#' stations and if they are active or not to the console.
#'
#' @param active boolean to select only currently active stations. Default =
#'   TRUE.
#' @return a data-frame.
#' @format The returned data-frame contains the following columns: \itemize{
#'   \item station  = ID of measurement station; \item plaats   = city closest
#'   to the measurement station; \item active	  = indicates if the station is
#'   still active; }
#' @keywords list weather stations
#' @export
printStations <- function(active = TRUE) {

  data(stations)

  if (active) {
    selected_stations <- stations[is.na(stations$einddatum),]
  } else {
    selected_stations <- stations
  }
  selected_stations$active <- is.na(selected_stations$einddatum)

  selected_stations <- selected_stations[, c("station", "plaats", "active")]

  print(selected_stations)
}
