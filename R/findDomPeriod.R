#' Find the dominant period and weight of a single accelerometer axis
#'
#' This function calculates the dominant period of an accelerometer axis using an autoregressive modelling approach. It also returns a weight ((dominant - 2nd dominant)/dominant).
#' Returns two values: the dominant period, and a weight
#' @param x A single acceleration axis
#' @keywords 
#' @export
#' @examples
#' findDomPeriod()
findDomPeriod <- function(x){
  require(forecast)
  require(pracma)
  #makes ts, and removes linear trend
  x <- as.ts(x)
  x <- residuals(tslm(x ~ trend))
  
  #Calculates spectrogram
  armod <- spec.ar(x, plot=FALSE)
  #Finds peaks
  peaks <- as.data.frame(findpeaks(as.numeric(armod$spec)))
  
  if(nrow(peaks) == 0) {
    dom <- NA
    weight <- NA
  } else {
    #Orders peaks by the highest spectra
    peaks <- peaks[order(peaks$V1, decreasing=TRUE),]
    
    #Determines the frequency with the highest spectra
    dom <- 1/armod$freq[peaks$V2[1]]
    
    #Weight is calculated as the difference in spectra between the 'dominant' and second dominant 
    weight <- (peaks$V1[1]-peaks$V1[2])/peaks$V1[1]
    
    #abline(v=1/dom)
    #abline(h=peaks$V1[1])
    
  }
  domPeriod <- c(dom, weight)
  names(domPeriod) <- c("domPeriod","weight")
  return(domPeriod)
}