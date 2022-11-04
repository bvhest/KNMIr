#' @title difference-in-days between to equivalent time series
#'
#' @description
#' \code{difference_in_days} determines the two dates in two ranges of values that have the
#' most similar value. Next, the difference between the two days is calculated.
#'
#' @details
#' this helper-function calculates the difference in days between the maximum value in the first range with daily values
#' and the day-of-the-year that this value occured in the second range with daily values. Normally the first range
#' contains the actual values and the second range contains a historic time series of data.
#' The function expects more or less continuously increasing values, for example the Huglin-index, VE-index, the yearly
#' totalized amount of rain, etc.
#'
#' @param ac dataframe containing the day-of-the-year and a column containing the value (note: The values must belong to the same year. The column-names are not relevant.)
#' @param ljg dataframe containing the day-of-the-year and a column containing the value (note: The values must belong to the same year. The column-names are not relevant)
#' @return difference in days between the two ranges with daily values.
#' @export

difference_in_days <- function(ac, ljg) {
   # check the provided data-frames:
   if(dim(ac)[2] > 2)
      stop("The first data-frame has more than 2 columns. Please provide only the day-of-the-year and a column containing the value.")
   if(dim(ljg)[2] > 2)
      stop("The second data-frame has more than 2 columns. Please provide only the day-of-the-year and a column containing the value.")
   if(dim(ac)[1] > 366)
      stop("The first data-frame seems to contain data for more than one year.")
   if(dim(ljg)[1] > 366)
      stop("The second data-frame seems to contain data for more than one year.")

   # rename the column-names
   colnames(ac) <- c("doy", "value")
   colnames(ljg) <- c("doy", "value")

   # perform the calculation
   DD.actueel <- max(ac$value)
   dvj.actueel <- ac[ac$value == DD.actueel,]$doy
   dvj.ljgem <- as.integer(mean(ljg[ljg$value > DD.actueel-5 & ljg$value < DD.actueel+5,]$doy))
   if (is.na(dvj.ljgem)) {
      (dvj.actueel - 365)
   } else {
      (dvj.actueel - dvj.ljgem)
   }
}
