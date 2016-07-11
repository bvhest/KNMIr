#' calculate the difference in days between the two ranges with daily values
#'
#' This function determines the two dates in two ranges of values that have the
#' most similar value. Next, the difference between the two days is calculated.
#' @param dataframe containting the columns doy, value
#' @param dataframe containting the columns doy, value
#' @return difference in days between the two ranges with daily values.
#' @keywords date-conversion
#' @export

delta.ljgem <- function(ac, ljg) {
   DD.actueel <- max(ac$somHuglinIndex)
   dvj.actueel <- ac[ac$somHuglinIndex == DD.actueel,]$dagVjaar
   dvj.ljgem <- as.integer(mean(ljg[ljg$somHuglinIndex > DD.actueel-5 & ljg$somHuglinIndex < DD.actueel+5,]$dagVjaar))
   if (is.na(dvj.ljgem)) {
      (dvj.actueel-365)
   } else {
      (dvj.actueel-dvj.ljgem)
   }
}
