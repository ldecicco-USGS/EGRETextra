% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/flexFNnew.R
\name{calcWY}
\alias{calcWY}
\title{Calculate Water Year}
\usage{
calcWY(df)
}
\arguments{
\item{df}{data frame with DecYear and Month columns}
}
\description{
Calculate Water Year
}
\examples{
library(EGRET)
eList <- Choptank_eList
Daily <- eList$Daily
Daily <- calcWY(Daily)
}

