#' Extract temperature data from an accelerometer file
#'
#' This function reads in an accelerometer file and extracts temperature data. 
#' Returns a dataframe with two columns: a time stamp, and the temp record
#' @param x Location of accelerometer csv file 
#' @keywords 
#' @export
#' @examples
#' extractTemp()
extractTemp <- function(x){
  df <- read.table(x, skip=10, sep=",", fill=TRUE, row.names=NULL)
  
  df <- subset(df, V1 == "T")
  df <- data.frame(Time = df$V2, Temp=df$V3)
  return(df)
}
