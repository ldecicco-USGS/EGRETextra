library(EGRET)

eList_Start <- Choptank_eList


shinyServer(function(input, output) {
  
  eList <- reactive({
    
    if(is.null(input$paStart)){
      paStart <- 10
    } else {
      paStart = as.integer(which(month.name == input$paStart))
    }
    
    if(is.null(input$paLong)){
      paLong <- 12
    } else {
      paLong = as.integer(input$paLong)
    }
    
    if(!is.null(input$data)){
      path <- input$data$datapath
      fileName <- input$data$name
      splitNames <- strsplit(fileName, "\\.")[[1]]
      extension <- splitNames[length(splitNames)]
      fileName <- paste0(splitNames[-length(splitNames)],collapse = ".")
      
      if(extension == "rds"){
        eList_Start <- readRDS(input$data$datapath)
      } else {
        #Remove old eList:
        for ( obj in ls() ) { 
          if(class(get(obj)) == "egret"){
            rm(list=as.character(obj))
          } 
        }
        #Load new:
        load(input$data$datapath)
        #Assign to eList_Start
        for ( obj in ls() ) { 
          if(class(get(obj)) == "egret"){
            assign("eList_Start", get(obj))
            break
          }
        }
      }
    }
    
    eList <- setPA(eList_Start, paStart, paLong)
    
  })
  
  output$flowPlotsOut <- renderPlot({
    flowPlotsStuff()
  })
  
  flowPlotsStuff <- reactive({ 
    
    eList <- eList()
    
    if(is.null(input$flowStat)){
      stat=5
    } else {
      stat = as.integer(input$flowStat)
    }
    
    if(is.null(input$qUnit)){
      qUnit = 1
    } else {
      qUnit = as.integer(input$qUnit)
    }
    
    if(is.null(input$logScaleFlow)){
      logScale = FALSE
    } else {
      logScale = input$logScaleFlow
    }
    
    switch(input$flowPlots,
           "plotFlowSingle" = plotFlowSingle(eList, istat=stat, qUnit = qUnit),
           "plotSDLogQ" = plotSDLogQ(eList),
           "plotQTimeDaily" = plotQTimeDaily(eList, qUnit = qUnit, logScale = logScale),
           "plotFour" = plotFour(eList, qUnit = qUnit),
           "plotFourStats" = plotFourStats(eList, qUnit = qUnit)
           
           )
    
    pdf("plot.pdf")
    switch(input$flowPlots,
           "plotFlowSingle" = plotFlowSingle(eList, istat=stat, qUnit = qUnit),
           "plotSDLogQ" = plotSDLogQ(eList),
           "plotQTimeDaily" = plotQTimeDaily(eList, qUnit = qUnit, logScale = logScale),
           "plotFour" = plotFour(eList, qUnit = qUnit),
           "plotFourStats" = plotFourStats(eList, qUnit = qUnit)
           
    )
    dev.off()
    
  })
  
  output$dataPlotsOut <- renderPlot({ 
    dataPlotsStuff()
  })
  
  dataPlotsStuff <- reactive({ 
    
    eList <- eList()
    
    if(is.null(input$qUnit)){
      qUnit = 1
    } else {
      qUnit = as.integer(input$qUnit)
    }
    
    if(is.null(input$logScaleData)){
      logScale = FALSE
    } else {
      logScale = input$logScaleData
    }

    switch(input$dataPlots,
           "boxConcMonth" = boxConcMonth(eList, logScale = logScale),
           "boxQTwice" = boxQTwice(eList, qUnit = qUnit),
           "plotConcTime" = plotConcTime(eList, logScale = logScale),
           "plotConcQ" = plotConcQ(eList, qUnit = qUnit, logScale = logScale),
           "multiPlotDataOverview" = multiPlotDataOverview(eList, qUnit = qUnit)
           
    )
    
    pdf("plot.pdf")
    switch(input$dataPlots,
           "boxConcMonth" = boxConcMonth(eList, logScale = logScale),
           "boxQTwice" = boxQTwice(eList, qUnit = qUnit),
           "plotConcTime" = plotConcTime(eList, logScale = logScale),
           "plotConcQ" = plotConcQ(eList, qUnit = qUnit, logScale = logScale),
           "multiPlotDataOverview" = multiPlotDataOverview(eList, qUnit = qUnit)
           
    )
    dev.off()
  })
  
  output$modelPlotsOut <- renderPlot({
    modelPlotsStuff()
  })
  
  modelPlotsStuff <- reactive({   
    eList <- eList()
    
    if(is.null(input$date1)){
      date1 = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.1), origin="1970-01-01")
    } else {
      date1 = input$date1
    }
    
    if(is.null(input$date2)){
      date2 = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.5), origin="1970-01-01")
    } else {
      date2 = input$date2
    }
    
    if(is.null(input$date3)){
      date3 = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.9), origin="1970-01-01")
    } else {
      date3 = input$date3
    }
    
    if(is.null(input$qLow)){
      qLow = round(quantile(eList$Daily$Q, probs = 0.1),digits = 1)
    } else {
      qLow = input$qLow
    }
    
    if(is.null(input$qHigh)){
      qHigh = round(quantile(eList$Daily$Q, probs = 0.9),digits = 1)
    } else {
      qHigh = input$qHigh
    }
    
    if(is.null(input$qMid)){
      qMid = round(quantile(eList$Daily$Q, probs = 0.5),digits = 1)
    } else {
      qMid = input$qMid
    }
    
    if(is.null(input$qUnit)){
      qUnit = 1
    } else {
      qUnit = as.integer(input$qUnit)
    }
    
    if(is.null(input$logScaleModel)){
      logScale = FALSE
    } else {
      logScale = input$logScaleModel
    }
    
    if(is.null(input$fluxUnit)){
      fluxUnit = 3
    } else {
      fluxUnit = as.integer(input$fluxUnit)
    }
    
    if(is.null(input$centerDate)){
      centerDate = "04-01"
    } else {
      centerDate = input$centerDate
    }
    
    if(is.null(input$yearStart)){
      yearStart = ceiling(min(eList$Daily$DecYear))
    } else {
      yearStart = as.integer(input$yearStart)
    }
    
    if(is.null(input$yearEnd)){
      yearEnd = floor(max(eList$Daily$DecYear))
    } else {
      yearEnd = as.integer(input$yearEnd)
    }
    
    if(is.null(input$maxDiff)){
      maxDiff = diff(range(eList$Sample$ConcAve))
    } else {
      maxDiff = round(as.numeric(input$maxDiff),digits = 3)
    }
    
    contours <- pretty(c(min(eList$surfaces[,,3]), max(eList$surfaces[,,3])), n=5)
    
    if(is.null(input$from)){
      from <- contours[1]
    } else {
      from = as.numeric(input$from)
    }
    
    if(is.null(input$to)){
      to <- contours[length(contours)]
    } else {
      to = as.numeric(input$to)
    }
    
    if(is.null(input$by)){
      by <- 5
    } else {
      by = as.integer(input$by)
    }
    
    if(is.null(input$rResid)){
      rResid <- FALSE
    } else {
      rResid <- input$rResid
    }
    switch(input$modelPlots,
           "plotConcTimeDaily" = plotConcTimeDaily(eList),
           "plotFluxTimeDaily" = plotFluxTimeDaily(eList, fluxUnit=fluxUnit),
           "plotConcPred" = plotConcPred(eList, logScale = logScale),
           "plotFluxPred" = plotFluxPred(eList, fluxUnit=fluxUnit),
           "plotResidPred" = plotResidPred(eList, rResid=rResid),
           "plotResidQ" = plotResidQ(eList, qUnit=qUnit, rResid=rResid),
           "plotResidTime" = plotResidTime(eList, rResid=rResid),
           "boxResidMonth" = boxResidMonth(eList, rResid=rResid),
           "boxConcThree" = boxConcThree(eList),
           "plotConcHist" = plotConcHist(eList),
           "plotFluxHist" = plotFluxHist(eList, fluxUnit=fluxUnit),
           "plotConcQSmooth" = plotConcQSmooth(eList, date1=date1,date2=date2, date3=date3,
                                               qLow=qLow,qHigh=qHigh, logScale = logScale),
           "plotConcTimeSmooth" = plotConcTimeSmooth(eList, q1=qLow, q2=qMid, q3=qHigh, logScale = logScale,
                                                     centerDate=centerDate,yearStart=yearStart, yearEnd=yearEnd),
           "fluxBiasMulti" = fluxBiasMulti(eList, fluxUnit=fluxUnit, qUnit=qUnit, rResid=rResid),
           "plotContours" = plotContours(eList, qUnit=qUnit,yearStart = yearStart, yearEnd = yearEnd,
                                         qBottom = qLow, qTop=qHigh,contourLevels = seq(from, to, length.out =by)),
           "plotDiffContours" = plotDiffContours(eList, year0=yearStart,year1 = yearEnd, maxDiff = maxDiff,
                              qUnit=qUnit,qBottom = qLow, qTop=qHigh)

           )
    
    pdf("plot.pdf")
    switch(input$modelPlots,
           "plotConcTimeDaily" = plotConcTimeDaily(eList),
           "plotFluxTimeDaily" = plotFluxTimeDaily(eList, fluxUnit=fluxUnit),
           "plotConcPred" = plotConcPred(eList, logScale = logScale),
           "plotFluxPred" = plotFluxPred(eList, fluxUnit=fluxUnit),
           "plotResidPred" = plotResidPred(eList, rResid=rResid),
           "plotResidQ" = plotResidQ(eList, qUnit=qUnit, rResid=rResid),
           "plotResidTime" = plotResidTime(eList, rResid=rResid),
           "boxResidMonth" = boxResidMonth(eList, rResid=rResid),
           "boxConcThree" = boxConcThree(eList),
           "plotConcHist" = plotConcHist(eList),
           "plotFluxHist" = plotFluxHist(eList, fluxUnit=fluxUnit),
           "plotConcQSmooth" = plotConcQSmooth(eList, date1=date1,date2=date2, date3=date3,
                                               qLow=qLow,qHigh=qHigh, logScale = logScale),
           "plotConcTimeSmooth" = plotConcTimeSmooth(eList, q1=qLow, q2=qMid, q3=qHigh, logScale = logScale,
                                                     centerDate=centerDate,yearStart=yearStart, yearEnd=yearEnd),
           "fluxBiasMulti" = fluxBiasMulti(eList, fluxUnit=fluxUnit, qUnit=qUnit, rResid=rResid),
           "plotContours" = plotContours(eList, qUnit=qUnit,yearStart = yearStart, yearEnd = yearEnd,
                                         qBottom = qLow, qTop=qHigh,contourLevels = seq(from, to, length.out =by)),
           "plotDiffContours" = plotDiffContours(eList, year0=yearStart,year1 = yearEnd, maxDiff = maxDiff,
                                                 qUnit=qUnit,qBottom = qLow, qTop=qHigh)
           
    )
    dev.off()
    
  })
  
  output$SampleText <- renderUI({
    
    eList <- eList()
    
    if(is.na(eList$Sample)){
      HTML(paste0("<h4>","No water quality data", "</h4>"))
    } else if(nrow(eList$Sample) == 0) {
      HTML(paste0("<h4>","No water quality data", "</h4>"))
    } else {
      HTML("")
    }
  })
  
  output$modelText <- renderUI({
    
    eList <- eList()
    
    if(is.na(eList$Sample)){
      HTML(paste0("<h4>","No water quality data", "</h4>"))
    } else if(nrow(eList$Sample) == 0) {
      HTML(paste0("<h4>","No water quality data", "</h4>"))
    } else {
      HTML("")
    }
  })
  
  output$flowLog <- renderUI({
    if(input$flowPlots == "plotQTimeDaily"){
      checkboxInput("logScaleFlow", label = h5("Log Scale"))
    }
  })
  
  output$rResid <- renderUI({
    if(input$modelPlots %in% c("fluxBiasMulti", "plotResidPred","plotResidQ",
                               "plotResidTime","boxResidMonth")){
      checkboxInput("rResid", label = h5("Randomized Censored Values:"))
    }
  })
  
  output$dataLog <- renderUI({
    if(input$dataPlots %in% c("boxConcMonth", "plotConcTime", "plotConcQ")){
      checkboxInput("logScaleData", label = h5("Log Scale"))
    }
  })
  
  output$from <- renderUI({
    if(input$modelPlots %in% c("plotContours")){
      eList <- eList()
      contours <- pretty(c(min(eList$surfaces[,,3]), max(eList$surfaces[,,3])), n=5)
      numericInput("from", label = h5("From"), value = contours[1])
    }
  })  
  
  output$to <- renderUI({
    if(input$modelPlots %in% c("plotContours")){
      eList <- eList()
      contours <- pretty(c(min(eList$surfaces[,,3]), max(eList$surfaces[,,3])), n=5)
      numericInput("to", label = h5("To"), value = contours[length(contours)])
    }
  })
  
  output$by <- renderUI({
    if(input$modelPlots %in% c("plotContours")){
      numericInput("by", label = h5("Number of divisions"), value = 5)
    }
  })
  
  output$date1 <- renderUI({
    if(input$modelPlots == "plotConcQSmooth"){
      eList <- eList()
      dateInput("date1", label = h5("date1"), 
                value = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.1), origin="1970-01-01"))
    }
  })
  
  output$yearStart <- renderUI({
    if(input$modelPlots %in% c("plotConcTimeSmooth","plotContours","plotDiffContours")){
      eList <- eList()
      numericInput("yearStart", label = h5("yearStart"), value = ceiling(min(eList$Daily$DecYear)))
    }
  })
  
  output$maxDiff <- renderUI({
    if(input$modelPlots %in% c("plotDiffContours")){
      eList <- eList()
      numericInput("maxDiff", label = h5("maxDiff"), value = diff(range(eList$Sample$ConcAve)))
    }
  })
  
  output$yearEnd <- renderUI({
    if(input$modelPlots %in% c("plotConcTimeSmooth","plotContours","plotDiffContours")){
      eList <- eList()
      numericInput("yearEnd", label = h5("yearEnd"), value = floor(max(eList$Daily$DecYear)))
    }
  })
  
  output$date2 <- renderUI({
    if(input$modelPlots == "plotConcQSmooth"){
      eList <- eList()
      dateInput("date2", label = h5("date2"), 
                value = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.5), origin="1970-01-01"))
    }
  })
  
  output$centerDate <- renderUI({
    if(input$modelPlots == "plotConcQSmooth"){
      textInput("centerDate", label = h5("centerDate"), value = "04-01")
    }
  })
  
  output$date3 <- renderUI({
    if(input$modelPlots == "plotConcQSmooth"){
      eList <- eList()
      dateInput("date3", label = h5("date3"), 
                value = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.9), origin="1970-01-01"))
    }
  })
  
  output$qLow <- renderUI({
    if(input$modelPlots %in% c("plotConcQSmooth","plotConcTimeSmooth","plotContours","plotDiffContours")){
      eList <- eList()
      qFactor <- qConst[shortCode=as.integer(input$qUnit)][[1]]
      qFactor <- qFactor@qUnitFactor
      numericInput("qLow", label = h5("qLow"), value = round(qFactor * quantile(eList$Daily$Q, probs = 0.1),digits = 1))
    }
  })
  
  output$qHigh <- renderUI({
    if(input$modelPlots %in% c("plotConcQSmooth","plotConcTimeSmooth","plotContours","plotDiffContours")){
      eList <- eList()
      qFactor <- qConst[shortCode=as.integer(input$qUnit)][[1]]
      qFactor <- qFactor@qUnitFactor
      numericInput("qHigh", label = h5("qHigh"), value = round(qFactor * quantile(eList$Daily$Q, probs = 0.9),digits = 1))
    }
  })
  
  output$qMid <- renderUI({
    if(input$modelPlots %in% c("plotConcQSmooth","plotConcTimeSmooth")){
      eList <- eList()
      qFactor <- qConst[shortCode=as.integer(input$qUnit)][[1]]
      qFactor <- qFactor@qUnitFactor
      numericInput("qMid", label = h5("qMid"), value = round(qFactor * quantile(eList$Daily$Q, probs = 0.5),digits = 1))
    }
  })
  
  output$modelLog <- renderUI({
    if(input$modelPlots %in% c("plotConcPred","plotConcQSmooth","plotConcTimeSmooth")){
      checkboxInput("logScaleModel", label = h5("Log Scale"))
    }
  })
  
  output$flowStatistic <- renderUI({
    if(input$flowPlots == "plotFlowSingle"){
      selectInput("flowStat", label = "Flow Statistic", 
                  choices = list("1-day minimum"=1, "7-day minimum"=2, "30-day minimum"=3, "median"=4,
                                 "mean"=5, "30-day maximum"=6, "7-day maximum"=7, "1-day maximum"=8),
                  selected = 5, multiple = FALSE)
    }
  })
  
  output$flowCode <- renderPrint({
    
    if(is.null(input$flowStat)){
      stat=5
    } else {
      stat = as.integer(input$flowStat)
    }
    
    if(is.null(input$qUnit)){
      qUnit = 1
    } else {
      qUnit = as.integer(input$qUnit)
    }
    
    if(is.null(input$paStart)){
      paStart <- 10
    } else {
      paStart = as.integer(which(month.name == input$paStart))
    }
    
    if(is.null(input$paLong)){
      paLong <- 12
    } else {
      paLong = as.integer(input$paLong)
    }
    
    
    if(is.null(input$logScaleFlow)){
      logScale = FALSE
    } else {
      logScale = input$logScaleFlow
    }
    
    outText <- switch(input$flowPlots,
           "plotFlowSingle" = paste0("plotFlowSingle(eList, istat=", stat,", qUnit = ", qUnit, ")"),
           "plotSDLogQ" = paste0("plotSDLogQ(eList", ")"),
           "plotQTimeDaily" = paste0("plotQTimeDaily(eList, logScale = ",logScale,", qUnit = ", qUnit, ")"),
           "plotFour" = paste0("plotFour(eList, qUnit = ", qUnit, ")"),
           "plotFourStats" = paste0("plotFourStats(eList, qUnit = ", qUnit, ")")
           
    )
    
    HTML(paste0("setPA(eList, paStart = ",paStart, ", paLong = ", paLong,")\n",
                "",outText))
    
  })
  
  output$dataCode <- renderPrint({

    if(is.null(input$qUnit)){
      qUnit = 1
    } else {
      qUnit = as.integer(input$qUnit)
    }
    
    if(is.null(input$paStart)){
      paStart <- 10
    } else {
      paStart = as.integer(which(month.name == input$paStart))
    }
    
    if(is.null(input$paLong)){
      paLong <- 12
    } else {
      paLong = as.integer(input$paLong)
    }
    
    if(is.null(input$logScaleData)){
      logScale = FALSE
    } else {
      logScale = input$logScaleData
    }

    
    outText <- switch(input$dataPlots,
           "boxConcMonth" = paste0("boxConcMonth(eList, logScale = ", logScale,")"),
           "boxQTwice" = paste0("boxQTwice(eList, qUnit = ", qUnit, ")"),
           "plotConcTime" = paste0("plotConcTime(eList, logScale = ", logScale,")"),
           "plotConcQ" = paste0("plotConcQ(eList, logScale = ", logScale,", qUnit = ", qUnit,")"),
           "multiPlotDataOverview" = paste0("multiPlotDataOverview(eList, qUnit = ", qUnit,
                                            ")")
           
    )
    
    HTML(paste0("setPA(eList, paStart = ",paStart, ", paLong = ", paLong,")\n",
                outText))
    
  })
  
  output$loadCode <- renderPrint({
    HTML('# One example workflow for loading data. 
# Many other options described in EGRET User guide
# Use this workflow to produce an .rds file to 
# load in this application
library(EGRET)

siteID <- "01491000" #Choptank River at Greensboro, MD
startDate <- "" #Gets earliest date
endDate <- "2011-09-30"
parameter_cd <-"00631" #5 digit USGS code
Sample <- readNWISSample(siteID,parameter_cd,startDate,endDate)  
startDate <- min(as.character(Sample$Date)) 
Daily <- readNWISDaily(siteID,"00060",startDate,endDate)
INFO<- readNWISInfo(siteID,parameter_cd,interactive=FALSE)
INFO$shortName <- "Choptank River at Greensboro, MD"

# Merge discharge with sample data:
eList <- mergeReport(INFO, Daily, Sample)

############################
# Run WRTDS model:
eList <- modelEstimation(eList)
############################

saveRDS(eList, "chopList.rds")

# The file chopList.rds (or one like it) can be used
# with the "Choose File" button above

# load later:
eList <- readRDS("chopList.rds")

         ')
  })
  
  output$modelCode <- renderPrint({
    
    eList <- eList()
    
    if(is.null(input$qUnit)){
      qUnit = 1
    } else {
      qUnit = as.integer(input$qUnit)
    }
    
    if(is.null(input$fluxUnit)){
      fluxUnit = 3
    } else {
      fluxUnit = as.integer(input$fluxUnit)
    }
    
    if(is.null(input$paStart)){
      paStart <- 10
    } else {
      paStart = as.integer(which(month.name == input$paStart))
    }
    
    if(is.null(input$paLong)){
      paLong <- 12
    } else {
      paLong = as.integer(input$paLong)
    }
    
    if(is.null(input$date1)){
      date1 = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.1), origin="1970-01-01")
    } else {
      date1 = input$date1
    }
    
    if(is.null(input$date2)){
      date2 = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.5), origin="1970-01-01")
    } else {
      date2 = input$date2
    }
    
    if(is.null(input$date3)){
      date3 = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.9), origin="1970-01-01")
    } else {
      date3 = input$date3
    }
    
    if(is.null(input$qLow)){
      qLow = quantile(eList$Daily$Q, probs = 0.1)
    } else {
      qLow = input$qLow
    }
    
    if(is.null(input$qHigh)){
      qHigh = quantile(eList$Daily$Q, probs = 0.9)
    } else {
      qHigh = input$qHigh
    }
    
    if(is.null(input$qMid)){
      qMid = round(quantile(eList$Daily$Q, probs = 0.5),digits = 1)
    } else {
      qMid = input$qMid
    }
    
    if(is.null(input$centerDate)){
      centerDate = "04-01"
    } else {
      centerDate = input$centerDate
    }
    
    if(is.null(input$yearStart)){
      yearStart = ceiling(min(eList$Daily$DecYear))
    } else {
      yearStart = as.integer(input$yearStart)
    }
    
    if(is.null(input$yearEnd)){
      yearEnd = floor(max(eList$Daily$DecYear))
    } else {
      yearEnd = as.integer(input$yearEnd)
    }
    
    if(is.null(input$maxDiff)){
      maxDiff = diff(range(eList$Sample$ConcAve))
    } else {
      maxDiff = as.integer(input$maxDiff)
    }
    
    contours <- pretty(c(min(eList$surfaces[,,3]), max(eList$surfaces[,,3])), n=5)
    
    if(is.null(input$from)){
      from <- contours[1]
    } else {
      from = as.numeric(input$from)
    }
    
    if(is.null(input$to)){
      to <- contours[length(contours)]
    } else {
      to = as.numeric(input$to)
    }
    
    if(is.null(input$by)){
      by <- 5
    } else {
      by = as.integer(input$by)
    }
    
    if(is.null(input$rResid)){
      rResid <- FALSE
    } else {
      rResid <- input$rResid
    }
    
    if(is.null(input$logScaleModel)){
      logScale = FALSE
    } else {
      logScale = input$logScaleModel
    }
    
    outText <- switch(input$modelPlots,
           "plotConcTimeDaily" = paste0("plotConcTimeDaily(eList)"),
           "plotFluxTimeDaily" = paste0("plotFluxTimeDaily(eList, fluxUnit = ", fluxUnit),
           "plotConcPred" = paste0("plotConcPred(eList, logScale = ",logScale,")"),
           "plotFluxPred" = paste0("plotFluxPred(eList, fluxUnit = ", fluxUnit, ")"),
           "plotResidPred" = paste0("plotResidPred(eList", rResid = ",rResid)"),
           "plotResidQ" = paste0("plotResidQ(eList, qUnit = ", qUnit, rResid = ",rResid)"),
           "plotResidTime" = paste0("plotResidTime(eList", rResid = ",rResid)"),
           "boxResidMonth" = paste0("boxResidMonth(eList", rResid = ",rResid)"),
           "boxConcThree" = paste0("boxConcThree(eList)"),
           "plotConcHist" = paste0("plotConcHist(eList)"),
           "plotFluxHist" = paste0("plotFluxHist(eList, fluxUnit = ", fluxUnit, ")"),
           "plotConcQSmooth" = paste0("plotConcQSmooth(eList, date1 = '",date1, "', date2 = '",
                                      date2,"', date3 = '",date3, "', qLow = ",qLow,", qHigh = ",qHigh,
                                      ", logScale = ", logScale,")"),
           "plotConcTimeSmooth" = paste0("plotConcTimeSmooth(eList, q1 = ",qLow,
                                         ", q2 = ",qMid, ", q3 = ",qHigh, ", yearStart = ",
                                         yearStart,", yearEnd = ",yearEnd,", centerDate = ",centerDate,
                                         ", logScale = ",logScale,")"),
           "fluxBiasMulti" = paste0("fluxBiasMulti(eList, qUnit = ", qUnit,", fluxUnit = ", fluxUnit, ", rResid = ",rResid, ")"),
           "plotContours" = paste0("plotContours(eList, qUnit=", qUnit,", yearStart = ",yearStart,
                                   ", yearEnd = ",yearEnd,", qBottom = ",qLow,", qTop = ",qHigh,
                                   ", contourLevels = seq(",from,", ",to,", length.out=",by, "))"),
           "plotDiffContours" = paste0("plotDiffContours(eList, qUnit=",qUnit,", year0=",yearStart,",year1 = ",
                                       yearEnd,", qBottom = ",qLow,", qTop = ",qHigh, ", maxDiff = ",maxDiff,")")
    )
    
    HTML(paste0("setPA(eList, paStart = ",paStart, ", paLong = ", paLong,")\n",
                outText))
    
  })
  
  output$downloadModelPlot <- downloadHandler(
    
    filename = function() {
      paste(input$modelPlots, "pdf", sep = ".")
    },
    content = function(file) {
      file.copy("plot.pdf", file)
    }
  )
  
  output$downloadDataPlot <- downloadHandler(
    filename = function() {
      paste(input$dataPlots, "pdf", sep = ".")
    },
    content = function(file) {
      
      file.copy("plot.pdf", file)
    }
  )
  
  output$downloadFlowPlot <- downloadHandler(
    filename = function() {
      paste(input$flowPlots, "pdf", sep = ".")
    },
    content = function(file) {
      file.copy("plot.pdf", file)
    }
  )
  
})
