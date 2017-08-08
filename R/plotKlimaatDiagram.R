#' @title plot climate diagram.
#'
#' @description
#' \code{plot_climate_diagram} plots a climate diagram for the specified variable.
#'
#' @details
#' Plots a climate diagram, based loosely on the standard in the climate diagram world atlas compiled by Walter and
#' Lieth (1957-1966).
#'
#' @param data dataframe containing daily measurements for the variable
#' @param column the name of the variable to be plotted. The name must equal the column name in the dataframe. Note:
#' at the moment only the mean temperature (gemTemp) can be selected.
#' @param startYear start year of the period for which the average values are calculated. Default is 1981.
#' @param endYear end year of the period for which the average values are calculated. Default is 2010.
#' @param currentYear the year for which the daily values must be shown. When not provided, the most current year from
#' the dataframe is taken.
#' @param title a title for the plot. When not provided, a default title is generated.
#' @import ggplot2
#' @export
#'
#
# plot Klimaat Diagram
#
#
# zie beschrijving standaarden: http://www.zoolex.org/walter.html
# zie voorbeelden: https://www.meteo.be/meteo/view/nl/1088480-Jaarlijkse+grafieken.html
#
# Versie die lijkt op de KMI-plot in de presentatie van Tonny van Dael:
plot_climate_diagram <- function (data,
                                  column = "gemTemp",
                                  startYear = 1981,
                                  endYear = 2010,
                                  currentYear,
                                  title) {

   data(stations)
   stationsNaam <- stations$plaats[stations$stationID == unique(data$stationID)]
   stationsLat <- stations$lat[stations$stationID == unique(data$stationID)]
   stationsLon <- stations$lon[stations$stationID == unique(data$stationID)]

   variabeleNaam <- dplyr::case_when(column == "gemTemp" ~ "temperature",
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
      title <- paste(stationsNaam, ", The Netherlands : average monthly", variabeleNaam)
      subtitle <- paste0("Lat: ", stationsLat, "Lon: ", stationsLon)
   }

   labels <- c(paste0("extreme values ", startYear, "-", currentYear),
               paste0("normal values ", startYear, "-2010"),
               paste0("values ", currentYear))

   # calculate monthly data based on daily data:
   maandgegevens <- plyr::ddply(data,
                                c("month"),
                                plyr::summarise,
                                minTemp = min(gemTemp, na.rm = TRUE),
                                maxTemp = max(gemTemp, na.rm = TRUE))
   maandgegevens$minJaar <- 0
   maandgegevens$maxJaar <- 0
   for (m in 1:12) {
     maandgegevens$minJaar[m] <- max(data[data$month == m
                                                           & data$gemTemp == maandgegevens[maandgegevens$month == m,]$minTemp
                                                           ,]$year, na.rm = TRUE)
     maandgegevens$maxJaar[m] <- max(data[data$month == m
                                                           & data$gemTemp == maandgegevens[maandgegevens$month == m,]$maxTemp
                                                           ,]$year, na.rm = TRUE)
   }

   # create plot:
   # check https://stackoverflow.com/questions/6488748/passing-parameters-to-ggplot
   #   --> werkt niet door de colour-variabele...
   #
   p <-
     ggplot(data = data[data$year >= startYear,],
            aes(x = month, y = gemTemp, colour = "extrema")) +
#      aes_string(x = "month", y = column)) +
   geom_boxplot(aes(group = month),
                  show.legend = FALSE,
                  fill = "blue",
                  outlier.shape = 19,
                  outlier.size = 0,
                  outlier.stroke = 0) +
     geom_smooth(data = data[data$year >= startYear & data$year <= 2010,],
                 aes(x = month, y = gemTemp, colour = "normaal"),
                 method = "auto",
                 se = FALSE,
                 formula = y ~ median(x)) +
     geom_smooth(data = data[data$year == currentYear,],
                 aes(x = month, y = gemTemp, colour = "recent"),
                 method = "loess",
                 se = FALSE,
                 formula = y ~ x) +
     geom_text(data = maandgegevens
               ,aes(x = month, y = minTemp-1, label = minJaar, alpha = 0.75)
               ,size = 4
               ,show.legend = FALSE) +
     geom_text(data = maandgegevens
               ,aes(x = month, y = maxTemp+1, label = maxJaar, alpha = 0.75)
               ,size = 4
               ,show.legend = FALSE) +
     scale_colour_manual(name = "Legende:",
                         values = c("blue", "red", "green"),
                         labels = labels) +
     scale_x_continuous(breaks = seq(1, 12, by = 1)) +
     theme_bw() +
     theme(legend.title=element_blank(), legend.position="bottom") +
     labs(x="", y="Temperature (°C)") +
     ggtitle(label = title,
             subtitle = subtitle)

   print(p)
}
