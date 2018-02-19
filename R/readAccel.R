#' Read in accelerometer file
#'
#' This function reads in an accelerometer file created by Oregon Research Electronics AL100 Acceleration Loggers. 
#' Returns a dataframe with 4 columns: sample number, and 3 acceleration axes
#' @param x Location of accelerometer csv file 
#' @keywords 
#' @export
#' @examples
#' readAccel()


readAccel <- function(x){
  df <- read.table(x, skip=10, sep=",", fill=TRUE, row.names=NULL)
  df <- na.omit(df)
  colnames(df) <- c("sample","x","y","z")
  return(df)
}