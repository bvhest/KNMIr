#' @title plot KNMI measurement stations.
#'
#' @description
#' \code{plot_stations} plots a map of The Netherlands and shows the locations of the
#' KNMI measurement station and their id and name.
#'
#' @details
#' One can show the active
#' stations (active = TRUE, the default) or *all* stations (active = FALSE).
#'
#' @param active boolean filter on currently active stations. Default = TRUE.
#' @param temperature_sensor show all stations or filter on those that (don't) provide temperature data. Default = NULL.
#' @return data-frame with the id, name, url to station information and the
#'   lat/lon of the nearest KNMI-station.
#' @export
#'
plot_stations <-
  function(active = TRUE,
           temperature_sensor = TRUE) {

    utils::data(stations)
    utils::data(map_Netherlands)

    selected_stations <-
      stations %>%
      dplyr::filter(is.na(einddatum) == active) %>%
      dplyr::filter(temp_sensor == temperature_sensor)

    # add station labels
    selected_stations$text <- paste(selected_stations$stationID, selected_stations$plaats)

    p <-
      ggmap::ggmap(map.nl) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = lon, y = lat, color = !is.na(einddatum)),
                          size = 3,
                          data = selected_stations,
                          alpha = 1,
                          na.rm = TRUE,
                          show.legend = FALSE) +
      ggplot2::geom_label(data = selected_stations,
                          ggplot2::aes(x = lon, y = lat, label = text),
                          size = 3,
                          vjust = +0.02,
                          hjust = 0)
    print(p)
  }
