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
library(RColorBrewer)

if('segmentInfo' %in% names(attributes(eList$INFO))){
  segmentINFO <- attr(eList$INFO, "segmentInfo")
  
  plotFluxHist(eList, yearStart = min(segmentINFO$flowSegStart),
                      yearEnd = max(segmentINFO$flowSegEnd + 1))
  
  colors <- brewer.pal(nrow(segmentINFO), "Accent")
  
  arrowYs <- seq(par()$usr[4], par()$usr[3], length=10)[c(-1,-10)]
  
  if(nrow(segmentINFO) > 8){
    arrowYs <- c(arrowYs, seq(par()$usr[3], par()$usr[4], length=10)[c(-1,-10)])
  }
  
  for(i in 1:nrow(segmentINFO)){
    
    rect(segmentINFO$sampleSegStart[i], par()$usr[3], 
         segmentINFO$sampleSegEnd[i]+1, par()$usr[4],
         col= paste0(colors[i],"50")) 
    
    arrows(segmentINFO$flowSegStart[i], arrowYs[i], 
           segmentINFO$flowSegEnd[i]+1, arrowYs[i], code=3)
  }

}



