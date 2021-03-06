% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/graphAdd.R
\name{flexPlotAddOn}
\alias{flexPlotAddOn}
\title{Flexible Flow Normalization Plot Add On}
\usage{
flexPlotAddOn(eList, showArrows = TRUE, showRect = TRUE)
}
\arguments{
\item{eList}{named list with at least the Daily, Sample, and INFO dataframes}

\item{showArrows}{logical whether or not to show arrows representing flow segments}

\item{showRect}{logical whether or not to show rectangles representing sample segments}
}
\description{
Flexible Flow Normalization Plot Add On
}
\examples{
library(EGRET)
eList <- Choptank_eList
eList <- setUpEstimation(eList)
sampleSegStart <- c(1980,1990,2000)
flowSegStart <- c(1980,1985,1992)
flowSegEnd <- c(1994,2004,2011)
dateInfo <- data.frame(sampleSegStart, flowSegStart, flowSegEnd)
eList <- flexFN(eList, dateInfo)
plotFluxHist(eList)
flexPlotAddOn(eList)

eList <- Choptank_eList
eList <- setUpEstimation(eList)
sampleSegStart <- c(1980,1985,2000)
flowSegStart <- c(1980,1990,2000)
flowSegEnd <- c(1990,2000,2010)
dateInfo <- data.frame(sampleSegStart, flowSegStart, flowSegEnd)
eList <- flexFN(eList, dateInfo)
plotFluxHist(eList)
flexPlotAddOn(eList)
}

