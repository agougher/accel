% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readAndDom.R
\name{readAndDom}
\alias{readAndDom}
\title{Read in accelerometer file and find the dominant period for all 3 axes}
\usage{
readAndDom(x)
}
\arguments{
\item{x}{Location of accelerometer csv file}
}
\description{
This function reads in an accelerometer file and finds the dominant period for each of the three axes.  It also returns a weight, defined as ((dominant spectra - 2nd dominant)/dominant spectra). Returns
six values: the dominant period of x, y, z axes followed by their respective weights.
}
\examples{
readAndDom()
}
