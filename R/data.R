#' KNMI stations
#'
#' A dataset containing the ID's and meta-data on the official KNMI measurement stations.
#'
#' @format A data frame with 35 rows and 4 variables:
#' \itemize{
#'   \item station: ID of the station (210--391)
#'   \item plaats: City where the station is located
#'   \item lon: geographical longitude (format: Decimal Degrees DDD.DDDDD°)
#'   \item lat: geographical latitude (format: Decimal Degrees DDD.DDDDD°)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name stations
#' @usage data(stations)
#' @format A data frame with 35 rows and 4 variables
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
#' @usage data(knmi.langJarigGem)
#' @format A data frame with 13346 rows and 11 variables
"knmi.langJarigGem"
