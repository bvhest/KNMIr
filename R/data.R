#' KNMI stations
#'
#' A dataset containing the ID's and meta-data on the official KNMI measurement stations.
#'
#' \itemize{
#'   \item stationID: ID of the station (210--391)
#'   \item plaats: City where the station is located
#'   \item lat: geographical latitude (format: Decimal Degrees DDD.DDDDD°)
#'   \item lon: geographical longitude (format: Decimal Degrees DDD.DDDDD°)
#'   \item startdatum: start date of the measurement series
#'   \item einddatum: end date of the measurement series
#'   \item info: url pointing to the web-page with the station information
#'   \item temp_sensor: is a temperature sensor present?
#' }
#'
#' @docType data
#' @keywords datasets
#' @name stations
#' @usage data(stations)
#' @format A data frame with 63 rows and 8 variables:
#' @source \url{http://projects.knmi.nl/klimatologie/metadata/index.html}
"stations"

#' KNMI longterm averages
#'
#' A dataset containing the longterm averages (from 01-01-1999 to 31-12-2014) of measurements.
#'
#' @format A data frame with 13346 rows and 11 variables:
#' \itemize{
#'   \item station: ID of the station (210--391)
#'   \item dagVjaar: day of the year (1-366)
#'   \item gemTemp:
#'   \item gemMaxTemp:
#'   \item absMaxTemp:
#'   \item gemMinTemp:
#'   \item absMinTemp:
#'   \item gemNeerslag:
#'   \item gemZon:
#'   \item gemPercZon:
#'   \item gemStraling:
#' }
#'
#' @docType data
#' @keywords datasets
#' @name knmi.langJarigGem
#' @usage data(knmi_langJarigGem)
#' @format A data frame with 13346 rows and 11 variables
"knmi.langJarigGem"

#' Wind-force conversion table between the Beaufort-scale and m/s or km/hour.
#'
#' \itemize{
#'   \item Beaufort: the Beaufort scale
#'   \item min_km_per_uur: minimum wind force in km/hour corresponding to the Beaufort scale
#'   \item max_km_per_uur: maximum wind force in km/hour corresponding to the Beaufort scale
#'   \item min_m_per_s: minimum wind force in m/s corresponding to the Beaufort scale
#'   \item max_m_per_s: maximum wind force in m/s corresponding to the Beaufort scale
#'   \item omschrijving: description of the wind force (in Dutch)
#'   \item uitwerking op open zee: description of the effect of the wind force on open sea
#' }
#'
#' @docType data
#' @keywords datasets
#' @name beaufort_scale
#' @usage data(beaufort_scale)
#' @format A data frame with 13 rows and 7 variables:
#' @source \url{http://cdn.knmi.nl/system/downloads/files/000/000/011/original/beaufortschaal.pdf}
"beaufort_scale"

#' Map of the Netherlands (used for showing the locations of the measurementstations).
#'
#' details; 1280x1280 roadmap map image from Google Maps.
#'
#' @docType data
#' @keywords datasets
#' @name map
#' @usage data(map_Netherlands)
#' @format A data frame with 13 rows and 7 variables:
#' @source Google Maps
"map.nl"
