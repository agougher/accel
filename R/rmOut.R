#' Remove outliers from a dominant period series
#'
#' This function removes outliers from a dominant period series, based residuals of a loess curve.  Returns the same data frame, but with NA's in rows of outliers.
#' @param dat A 'long form' data frame with three columns: 'day', 'value', and 'weights'. Value is the dominant period.
#' @keywords 
#' @export
#' @examples
#' rmOut()



#Removes outliers from dominant period series
#dat is 'long form' data frame with three columns named: "day","value", and "weights". Value is the dominant period
#Returns the same dataframe, but with NA's in rows of outliers
rmOut <- function(dat){
  ###Finding outliers
  #Fits loess curve
  loessMod <- loess(value ~ day, data=dat, family="symmetric", weights=dat$weights^2)
  
  #Gets residuals
  resid <- residuals(loessMod)
  
  #Plots the data and loess curve
  plot(dat$day, dat$value, xlab="Day of year",ylab="Dominant period")
  points(dat$day, fitted(loessMod), col="purple", pch=20)
  
  #Determines the interquartile range of residual values
  resid.q <- quantile(resid,prob=c(0.25,0.75))
  iqr <- diff(resid.q)
  
  #Calculates 1.5*interquartile range, and determines limits
  limits <- resid.q + 1.5*iqr*c(-1,1)
  
  #calculates number of IQR's away as "score" 
  out <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0))
  
  #If the point is within 1.5x the interquartile range, the row is turned to NA
  dat[which(out > 0), ] <- NA
  
  return(dat)
}
