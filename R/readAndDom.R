#' Read in accelerometer file and find the dominant period for all 3 axes
#'
#' This function reads in an accelerometer file and finds the dominant period for each of the three axes.  It also returns a weight, defined as ((dominant spectra - 2nd dominant)/dominant spectra). Returns
#' a total of six values: the dominant period of x, y, z axes followed by their respective weights.
#' @param x Path to accelerometer csv file
#' @keywords
#' @export
#' @examples
#' readAndDom()

readAndDom <- function(x){

  df <- readAccel(x)
  valsX <- findDomPeriod(df$x); peakX <- valsX[1]; wpeakX <- valsX[2]
  valsY <- findDomPeriod(df$y); peakY <- valsY[1]; wpeakY <- valsY[2]
  valsZ <- findDomPeriod(df$z); peakZ <- valsZ[1]; wpeakZ <- valsZ[2]

  peaks <- cbind(peakX,peakY,peakZ,wpeakX,wpeakY,wpeakZ)
  return(peaks)
}
