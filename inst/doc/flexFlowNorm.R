## ----setup, include=FALSE, message=FALSE------------------
library(rmarkdown)
options(continue=" ")
options(width=60)
library(knitr)
library(EGRET)
library(EGRETextra)



## ---------------------------------------------------------
library(EGRET)
eList <- Choptank_eList
eList <- setUpEstimation(eList)

sampleSegStart <- c(1980,1985,2000)
flowSegStart <- c(1980,1990,2000)
flowSegEnd <- c(1990,2000,2010)
dateInfo <- data.frame(sampleSegStart, flowSegStart, flowSegEnd)
eList <- flexFN(eList,dateInfo)


## ----fig.height=6, fig.width=8, echo=FALSE----------------
plotFluxHist(eList)
flexPlotAddOn(eList)

