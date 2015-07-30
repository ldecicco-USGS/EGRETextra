## ----setup, include=FALSE, message=FALSE------------------
library(rmarkdown)
options(continue=" ")
options(width=60)
library(knitr)
library(EGRET)
library(rloadest)
library(survival)


## ----eval=FALSE-------------------------------------------
#  install.packages(c("rloadest"),
#    repos=c("http:/owi.usgs.gov/R","http://cran.us.r-project.org"))

## ----echo=TRUE, eval=FALSE--------------------------------
#  
#  library(EGRET)
#  library(rloadest)
#  library(survival)
#  
#  siteid <- "01491000"
#  pcode <- "00631"
#  startDate <- "1979-10-24"
#  endDate <- "2011-09-29"
#  
#  Daily <- readNWISDaily(siteid,"00060",startDate,endDate)
#  Sample <- readNWISSample(siteid,pcode,startDate,endDate)
#  INFO <- readNWISInfo(siteid,pcode)
#  eList <- mergeReport(INFO, Daily, Sample)
#  eList <- modelEstimation(eList)
#  multiPlotDataOverview(eList)

## ----echo=FALSE, fig.height=7-----------------------------
eList <- Choptank_eList
INFO <- eList$INFO
Sample <- eList$Sample
Daily <- eList$Daily
multiPlotDataOverview(eList)


## ----echo=TRUE, eval=TRUE, warning=FALSE------------------

loadestModel <- loadReg(Surv(ConcLow, ConcHigh, type="interval2")
                        ~ model(9), 
                   data = Sample, 
                   flow = "Q", dates = "Date",
                   flow.units="cms",
                   conc.units="mg/l",
                   station=INFO$station.nm)

# To see a complete summary of the model results:
# print(loadestModel, brief=FALSE, load.only=FALSE)


## ----echo=TRUE, eval=TRUE, warning=FALSE------------------

# Make DailyLoadest
DailyLoadest <- Daily[,which(!(names(Daily) %in% 
      c("ConcDay","FluxDay","FNConc","FNFlux","SE","yHat")))]

concs <- predConc(loadestModel,DailyLoadest, by="day")
flux <- predLoad(loadestModel,DailyLoadest, by="day")

predictResp <- fitted(loadestModel$cfit, type='response')
predictResp_mean <- fitted(loadestModel$cfit, type='mean')

DailyLoadest$ConcDay <- concs$Conc
DailyLoadest$FluxDay <- DailyLoadest$ConcDay*86.4*DailyLoadest$Q
DailyLoadest$SE <- concs$Std.Err

# Make SampleLoadest

SampleLoadest <- Sample[,which(!(names(Sample) %in% 
        c("yHat","SE","ConcHat")))]

SampleLoadest$SE <- concs$Std.Err[
  which(concs$Date %in% SampleLoadest$Date)]
SampleLoadest$yHat <-predictResp
SampleLoadest$ConcHat <- predictResp_mean

eListLoadest <- as.egret(INFO, DailyLoadest, SampleLoadest)


## ----echo=TRUE, fig.height=7------------------------------
fluxBiasMulti(eListLoadest, moreTitle="rloadEst")

## ----echo=TRUE--------------------------------------------
par(mfcol=c(1,2))
plotResidPred(eList, printTitle = FALSE, tinyPlot = TRUE)
title("WRTDS")
plotResidPred(eListLoadest, printTitle = FALSE, tinyPlot = TRUE)
title("rloadest")

## ----echo=TRUE--------------------------------------------
par(mfcol=c(1,2))
plotResidQ(eList, printTitle = FALSE, tinyPlot = TRUE)
title("WRTDS")
plotResidQ(eListLoadest, printTitle = FALSE, tinyPlot = TRUE)
title("rloadest")

## ----echo=TRUE--------------------------------------------
par(mfcol=c(1,2))
plotResidTime(eList, printTitle = FALSE, tinyPlot = TRUE)
title("WRTDS")
plotResidTime(eListLoadest, printTitle = FALSE, tinyPlot = TRUE)
title("rloadest")

## ----echo=TRUE--------------------------------------------
par(mfcol=c(1,2))
boxResidMonth(eList, printTitle = FALSE, tinyPlot = TRUE)
title("WRTDS")
boxResidMonth(eListLoadest, printTitle = FALSE, tinyPlot = TRUE)
title("rloadest")

## ----echo=TRUE--------------------------------------------
par(mfcol=c(1,2))
boxConcThree(eList, printTitle = FALSE, tinyPlot = TRUE)
title("WRTDS")
boxConcThree(eListLoadest, printTitle = FALSE, tinyPlot = TRUE)
title("rloadest")

## ----echo=TRUE--------------------------------------------
par(mfcol=c(1,2))
plotConcPred(eList, printTitle = FALSE, tinyPlot = TRUE)
title("WRTDS")
plotConcPred(eListLoadest, printTitle = FALSE, tinyPlot = TRUE)
title("rloadest")

## ----echo=FALSE, eval=TRUE, dev='png', warning=FALSE, fig.height=7, fig.cap="The rating-curve regression model"----
plot(loadestModel, which=1, set.up=FALSE)


## ----echo=FALSE, eval=TRUE, dev='png', warning=FALSE, fig.height=7, fig.cap="The residuals versus fit for the regression model"----

plot(loadestModel, which=2, set.up=FALSE)

## ----echo=FALSE, eval=TRUE, dev='png', warning=FALSE, fig.height=7, fig.cap="The scale-location graph for the regression model"----

plot(loadestModel, which=3, set.up=FALSE)

## ----echo=FALSE, eval=TRUE, dev='png', warning=FALSE, fig.height=7, fig.cap="The correlogram from the regression model"----

plot(loadestModel, which=4, set.up=FALSE)

## ----echo=FALSE, eval=TRUE, dev='png', warning=FALSE, fig.height=7, fig.cap="The Q-normal plot of the residuals"----

plot(loadestModel, which=5, set.up=FALSE)

## ----echo=FALSE, eval=TRUE, dev='png', warning=FALSE, fig.height=7, fig.cap="Box plot comparing estimated and observed values"----

plot(loadestModel, which=6, set.up=FALSE)

