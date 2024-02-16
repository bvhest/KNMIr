#' @title plot climate diagram.
#'
#' @description
#' \code{plot_climate_diagram} plots a climate diagram for the specified
#' variable.
#'
#' @details
#' Plots a climate diagram, based loosely on the standard in the climate diagram
#' world atlas compiled by Walter and Lieth (1957-1966).
#'
#' @param data dataframe containing daily measurements for the variable
#' @param column the name of the variable to be plotted. The name must equal the
#' column name in the dataframe.
#' Note: at the moment only the mean temperature (gemTemp) can be selected.
#' @param startYear start year of the period for which the average values are
#' calculated. Default is 1981.
#' @param endYear end year of the period for which the average values are
#' calculated. Default is 2010.
#' @param currentYear the year for which the daily values must be shown. When
#' not provided, the most current year from the dataframe is taken.
#' @param title a title for the plot. When not provided, a default title is
#' generated.
#' @import ggplot2
#' @export
#'
#
# plot Klimaat Diagram
#
#
# zie beschrijving standaarden: http://www.zoolex.org/walter.html
# zie voorbeelden:
# https://www.meteo.be/meteo/view/nl/1088480-Jaarlijkse+grafieken.html
#
# Versie die lijkt op de KMI-plot in de presentatie van Tonny van Dael:
plot_climate_diagram <- function(data,
                                 column = "gemTemp",
                                 startYear = 1981,
                                 endYear = 2010,
                                 currentYear,
                                 title) {

  data(stations)
  stationsNaam <- stations$plaats[stations$stationID == unique(data$stationID)]
  stationsLat <- stations$lat[stations$stationID == unique(data$stationID)]
  stationsLon <- stations$lon[stations$stationID == unique(data$stationID)]

  variabeleNaam <- dplyr::case_when(
    column == "gemTemp" ~ "temperature",
    column == "maxTemp" ~ "maximum temperature",
    column == "minTemp" ~ "minimum temperature",
    column == "gemWind" ~ "wind",
    column == "zon" ~ "sun",
    column == "straling" ~ "solar radiation",
    column == "dagTotaalNeerslag" ~ "daily precipitation",
    column == "gemBewolking" ~ "cloud cover"
  )

  if (missing(currentYear)) {
    currentYear <- max(data$year)
  }
  if (currentYear > max(data$year)) {
    currentYear <- max(data$year)
  }
  if (startYear < min(data$year)) {
    startYear <- min(data$year)
  }
  if (endYear > max(data$year)) {
    endYear <- max(data$year)
  }

  if (missing(title)) {
    title <- paste0(stationsNaam, ", The Netherlands : average monthly ",
                    variabeleNaam)
    subtitle <- paste0("Lat: ", stationsLat, ", Lon: ", stationsLon)
  }

  labels <- c(
    paste0("extreme values ", startYear, "-", currentYear),
    paste0("normal values ", startYear, "-2010"),
    paste0("values ", currentYear)
  )

  plotData <- data %>%
    dplyr::filter(year >= startYear) %>%
    dplyr::select(year, month, column)

  colnames(plotData) <- c("year", "month", "yvar")

  if (column == "dagTotaalNeerslag") {
    plotData <- plotData %>%
      dplyr::group_by(year, month) %>%
      dplyr::summarise(yvar = sum(yvar))
  }

  # calculate monthly data based on daily data:
  maandgegevens <-
    plyr::ddply(plotData,
      c("month"),
      plyr::summarise,
      minYvar = min(yvar, na.rm = TRUE),
      maxYvar = max(yvar, na.rm = TRUE)
    )

  maandgegevens$minJaar <- 0
  maandgegevens$maxJaar <- 0
  for (m in 1:12) {
    maandgegevens$minJaar[m] <-
      max(plotData[plotData$month == m & plotData$yvar == maandgegevens[maandgegevens$month == m, ]$minYvar, ]$year, na.rm = TRUE)
    maandgegevens$maxJaar[m] <-
      max(plotData[plotData$month == m & plotData$yvar == maandgegevens[maandgegevens$month == m, ]$maxYvar, ]$year, na.rm = TRUE)
  }

  # create plot:
  # check https://stackoverflow.com/questions/6488748/passing-parameters-to-ggplot
  #   --> werkt niet door de color-variabele...

  # label based on selected column:
  if (column == "gemTemp") {
    ylabel <- "Temperature (C)"
  } else if (column == "dagTotaalNeerslag") {
    ylabel <- "Precipitation (mm)"
  } else if (column == "gemWind") {
    ylabel <- "Wind (m/s)"
  } else if (column == "zon") {
    ylabel <- "Sun (hrs)"
  }

  p <-
    ggplot(
      data = plotData,
      aes(x = month, y = yvar, colour = "extrema")
    ) +
    geom_boxplot(aes(group = month),
      show.legend = FALSE,
      fill = "blue",
      outlier.shape = 19,
      outlier.size = 0,
      outlier.stroke = 0
    ) +
    geom_smooth(
      data = plotData[plotData$year <= 2010, ],
      aes(x = month, y = yvar, colour = "normaal"),
      method = "auto",
      se = FALSE,
      formula = y ~ median(x)
    ) +
    geom_smooth(
      data = plotData[plotData$year == currentYear, ],
      aes(x = month, y = yvar, colour = "recent"),
      method = "loess",
      se = FALSE,
      formula = y ~ x
    ) +
    geom_text(
      data = maandgegevens,
      aes(x = month, y = minYvar - 1, label = minJaar, alpha = 0.75),
      size = 4,
      show.legend = FALSE
    ) +
    geom_text(
      data = maandgegevens,
      aes(x = month, y = maxYvar + 1, label = maxJaar, alpha = 0.75),
      size = 4,
      show.legend = FALSE
    ) +
    scale_colour_manual(
      name = "Legende:",
      values = c("blue", "red", "green"),
      labels = labels
    ) +
    scale_x_continuous(breaks = seq(1, 12, by = 1)) +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    labs(x = "", y = ylabel) +
    ggtitle(
      label = title,
      subtitle = subtitle
    )

  print(p)
}
