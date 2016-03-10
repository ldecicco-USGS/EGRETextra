library(EGRET)
library(EGRETci)
library(leaflet)
library(dplyr)
library(DT)

tempFolder <- tempdir()

eList_Start <- Choptank_eList

shinyServer(function(input, output, session) {
  

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
  
  flowPlotStuff <- reactive({
    
    eList <- eList()
    stat = as.integer(input$flowStat)
    qUnit = as.integer(input$qUnit)
    logScale = input$logScaleFlow
    
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
  
  output$flowPlotsOut <- renderPlot({ 
    flowPlotStuff()
  })
  
  dataPlotStuff <- reactive({
    
    eList <- eList()
    qUnit = as.integer(input$qUnit)
    logScale = input$logScaleData
    
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
  
  output$dataPlotsOut <- renderPlot({ 
    dataPlotStuff()
  })
  
  modelPlotStuff <- reactive({
    
    eList <- eList()
    
    date1 = input$date1
    date2 = input$date2
    date3 = input$date3
    qLow = input$flowRange[1]
    qHigh = input$flowRange[2]
    qMid = input$qMid
    qUnit = as.integer(input$qUnit)
    logScale = input$logScaleModel
    fluxUnit = as.integer(input$fluxUnit)
    centerDate = input$centerDate
    yearStart = as.integer(input$yearRange[1])
    yearEnd = as.integer(input$yearRange[2])
    maxDiff = input$maxDiff
    from = as.numeric(input$concRange[1])
    to = as.numeric(input$concRange[2])
    by = as.integer(input$by)+1
    rResid <- input$rResid
    
    switch(input$modelPlots,
           "plotConcTimeDaily" = plotConcTimeDaily(eList),
           "plotFluxTimeDaily" = plotFluxTimeDaily(eList, fluxUnit=fluxUnit),
           "plotConcPred" = plotConcPred(eList, logScale = logScale),
           "plotFluxPred" = plotFluxPred(eList, fluxUnit=fluxUnit),
           "plotResidPred" = plotResidPred(eList, rResid = rResid),
           "plotResidQ" = plotResidQ(eList, qUnit=qUnit, rResid = rResid),
           "plotResidTime" = plotResidTime(eList, rResid = rResid),
           "boxResidMonth" = boxResidMonth(eList, rResid = rResid),
           "boxConcThree" = boxConcThree(eList),
           "plotConcHist" = plotConcHist(eList),
           "plotFluxHist" = plotFluxHist(eList, fluxUnit=fluxUnit),
           "plotConcQSmooth" = plotConcQSmooth(eList, date1=date1,date2=date2, date3=date3,qLow=qLow,qHigh=qHigh),
           "plotConcTimeSmooth" = plotConcTimeSmooth(eList, q1=qLow, q2=qMid, q3=qHigh, logScale = logScale,
                                                     centerDate=centerDate,yearStart=yearStart, yearEnd=yearEnd),
           "fluxBiasMulti" = fluxBiasMulti(eList, fluxUnit=fluxUnit, qUnit=qUnit, rResid = rResid),
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
           "plotResidPred" = plotResidPred(eList, rResid = rResid),
           "plotResidQ" = plotResidQ(eList, qUnit=qUnit, rResid = rResid),
           "plotResidTime" = plotResidTime(eList, rResid = rResid),
           "boxResidMonth" = boxResidMonth(eList, rResid = rResid),
           "boxConcThree" = boxConcThree(eList),
           "plotConcHist" = plotConcHist(eList),
           "plotFluxHist" = plotFluxHist(eList, fluxUnit=fluxUnit),
           "plotConcQSmooth" = plotConcQSmooth(eList, date1=date1,date2=date2, date3=date3,qLow=qLow,qHigh=qHigh),
           "plotConcTimeSmooth" = plotConcTimeSmooth(eList, q1=qLow, q2=qMid, q3=qHigh, logScale = logScale,
                                                     centerDate=centerDate,yearStart=yearStart, yearEnd=yearEnd),
           "fluxBiasMulti" = fluxBiasMulti(eList, fluxUnit=fluxUnit, qUnit=qUnit, rResid = rResid),
           "plotContours" = plotContours(eList, qUnit=qUnit,yearStart = yearStart, yearEnd = yearEnd,
                                         qBottom = qLow, qTop=qHigh,contourLevels = seq(from, to, length.out =by)),
           "plotDiffContours" = plotDiffContours(eList, year0=yearStart,year1 = yearEnd, maxDiff = maxDiff,
                                                 qUnit=qUnit,qBottom = qLow, qTop=qHigh)
    )
    dev.off()
    
  })
  
  output$modelPlotsOut <- renderPlot({
    modelPlotStuff()
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
  
  output$saveData <- downloadHandler(
    filename = function() {
      paste("eList", "rds", sep = ".")
    },
    content = function(file) {
      
      file.copy(file.path(tempFolder,"eList.rds"), file)
    }
  )
  
  output$SampleText <- renderUI({
    
    eList <- eList()
    
    if(nrow(eList$Sample) == 0) {
      HTML(paste0("<h4>","No water quality data", "</h4>"))
    } else {
      HTML("")
    }
  })
  
  output$metaData <- DT::renderDataTable({
    
    eList <- eList()
    
    INFO <- eList$INFO
    
    flippedTable <- data.frame(t(INFO[,names(INFO) %in% c("station_nm","site_no","agency_cd",
                                                          "dec_lat_va","dec_long_va","tz_cd",
                                                          "drainSqKm","shortName","param_nm",
                                                          "param_units","param_nm",
                                                          "paramNumber")]))
    
    DT::datatable(flippedTable, colnames = "",
                  options = list(pageLength = nrow(flippedTable)))
  })
  
  output$modelDataToChose <- DT::renderDataTable({
    genInfo <- choseData()
    legendTitle <- attr(genInfo, "legendTitle")
    
    genInfo <- genInfo[,c("param_nm","shortName","drainSqKm","colData")]
    
    names(genInfo) <- c("param_nm","shortName","drainSqKm",legendTitle)
    
    genInfoDT <- DT::datatable(genInfo, selection = "single")
    genInfoDT <- formatRound(genInfoDT, legendTitle, 2) 
    genInfoDT
    
  })
  
  output$modelText <- renderUI({
    
    eList <- eList()
    
    if(all(is.na(eList$Sample))){
      HTML(paste0("<h4>","No water quality data", "</h4>"))
    } else if(nrow(eList$Sample) == 0) {
      HTML(paste0("<h4>","No water quality data", "</h4>"))
    } else {
      HTML("")
    }
  })
  
  observe({
    eList <- eList()
    updateNumericInput(session, "maxDiff", value = diff(range(eList$Sample$ConcAve)))
  })
  
  observe({
    eList <- eList()
    updateSliderInput(session, "yearRange", 
                      min = ceiling(min(eList$Daily$DecYear)), max = floor(max(eList$Daily$DecYear)))
  })
  
  observe({
    eList <- eList()
    contours <- pretty(c(min(eList$surfaces[,,3]), max(eList$surfaces[,,3])), n=5)
    updateSliderInput(session, "concRange", 
                      min = 0.5*contours[1], max = 2*contours[length(contours)])
  })
  
  observe({
    eList <- eList()
    qFactor <- qConst[shortCode=as.integer(input$qUnit)][[1]]
    qFactor <- qFactor@qUnitFactor
    updateSliderInput(session, "flowRange", 
                      min = 0.5*as.numeric(round(qFactor * quantile(eList$Daily$Q, probs = 0.1),digits = 1)),
                      max = 2*as.numeric(round(qFactor * quantile(eList$Daily$Q, probs = 0.9),digits = 1)))
  })    
  
  observe({
    eList <- eList()
    updateDateInput(session, "date1", value=as.Date(quantile(eList$Daily$Date, type=1, probs = 0.1), origin="1970-01-01"))
  })
  
  observe({
    eList <- eList()    
    updateDateInput(session, "date2", value=as.Date(quantile(eList$Daily$Date, type=1, probs = 0.5), origin="1970-01-01"))
  })
  
  observe({
    eList <- eList()   
    updateDateInput(session, "date3", value=as.Date(quantile(eList$Daily$Date, type=1, probs = 0.9), origin="1970-01-01"))
  })
  
  observe({
    eList <- eList()
    qFactor <- qConst[shortCode=as.integer(input$qUnit)][[1]]
    qFactor <- qFactor@qUnitFactor
    updateNumericInput(session, "qMid", value = round(qFactor * quantile(eList$Daily$Q, probs = 0.5),digits = 1))
  })
  
  output$flowCode <- renderPrint({
    
    stat = as.integer(input$flowStat)
    qUnit = as.integer(input$qUnit)
    paStart = as.integer(which(month.name == input$paStart))
    paLong = as.integer(input$paLong)
    logScale = input$logScaleFlow
    
    outText <- switch(input$flowPlots,
                      "plotFlowSingle" = paste0("plotFlowSingle(eList, istat=", stat,", qUnit = ", qUnit, ")"),
                      "plotSDLogQ" = paste0("plotSDLogQ(eList", ")"),
                      "plotQTimeDaily" = paste0("plotQTimeDaily(eList, logScale = ",logScale,", qUnit = ", qUnit, ")"),
                      "plotFour" = paste0("plotFour(eList, qUnit = ", qUnit, ")"),
                      "plotFourStats" = paste0("plotFourStats(eList, qUnit = ", qUnit, ")")
                      
    )
    
    HTML(paste0("setPA(eList, paStart = ",paStart, ", paLong = ", paLong,")\n",
                outText))
    
  })
  
  output$dataCode <- renderPrint({
    
    qUnit = as.integer(input$qUnit)
    paStart = as.integer(which(month.name == input$paStart))
    paLong = as.integer(input$paLong)
    logScale = input$logScaleData
    
    outText <- switch(input$dataPlots,
                      "boxConcMonth" = paste0("boxConcMonth(eList, logScale = ", logScale,")"),
                      "boxQTwice" = paste0("boxQTwice(eList, qUnit = ", qUnit, ")"),
                      "plotConcTime" = paste0("plotConcTime(eList, logScale = ", logScale,")"),
                      "plotConcQ" = paste0("plotConcQ(eList, logScale = ", logScale,", qUnit = ", qUnit, ")"),
                      "multiPlotDataOverview" = paste0("multiPlotDataOverview(eList, qUnit = ", qUnit, ")")
                      
    )
    
    HTML(paste0("setPA(eList, paStart = ",paStart, ", paLong = ", paLong,")\n",
                outText))
    
  })
  
  output$modelCode <- renderPrint({
    
    eList <- eList()
    
    qUnit = as.integer(input$qUnit)
    fluxUnit = as.integer(input$fluxUnit)
    paStart = as.integer(which(month.name == input$paStart))
    paLong = as.integer(input$paLong)
    date1 = input$date1
    date2 = input$date2
    date3 = input$date3
    qLow = input$flowRange[1]
    qHigh = input$flowRange[2]
    qMid = input$qMid
    centerDate = input$centerDate
    yearStart = as.integer(input$yearRange[1])
    yearEnd = as.integer(input$yearRange[2])
    maxDiff = as.integer(input$maxDiff)
    from = input$concRange[1]
    to = input$concRange[2]
    by = as.integer(input$by) + 1
    rResid = input$rResid
    
    outText <- switch(input$modelPlots,
                      "plotConcTimeDaily" = paste0("plotConcTimeDaily(eList)"),
                      "plotFluxTimeDaily" = paste0("plotFluxTimeDaily(eList, fluxUnit = ", fluxUnit),
                      "plotConcPred" = paste0("plotConcPred(eList)"),
                      "plotFluxPred" = paste0("plotFluxPred(eList, fluxUnit = ", fluxUnit, ")"),
                      "plotResidPred" = paste0("plotResidPred(eList, rResid = ", rResid, ")"),
                      "plotResidQ" = paste0("plotResidQ(eList, qUnit = ", qUnit, ", rResid = ", rResid, ")"),
                      "plotResidTime" = paste0("plotResidTime(eList, rResid = ", rResid, ")"),
                      "boxResidMonth" = paste0("boxResidMonth(eList, rResid = ", rResid, ")"),
                      "boxConcThree" = paste0("boxConcThree(eList)"),
                      "plotConcHist" = paste0("plotConcHist(eList)"),
                      "plotFluxHist" = paste0("plotFluxHist(eList, fluxUnit = ", fluxUnit, ")"),
                      "plotConcQSmooth" = paste0("plotConcQSmooth(eList, date1 = '",date1, "', date2 = '",
                                                 date2,"', date3 = '",date3, "', qLow = ",qLow,", qHigh = ",qHigh,")"),
                      "plotConcTimeSmooth" = paste0("plotConcTimeSmooth(eList, q1 = ",qLow,
                                                    ", q2 = ",qMid, ", q3 = ",qHigh, ", yearStart = ",
                                                    yearStart,", yearEnd = ",yearEnd,", centerDate = ",centerDate,")"),
                      "fluxBiasMulti" = paste0("fluxBiasMulti(eList, qUnit = ", qUnit,", fluxUnit = ", fluxUnit,", rResid = ", rResid, ")"),
                      "plotContours" = paste0("plotContours(eList, qUnit=", qUnit,", yearStart = ",yearStart,
                                              ", yearEnd = ",yearEnd,", qBottom = ",qLow,", qTop = ",qHigh,
                                              ", contourLevels = seq(",from,", ",to,", length.out=",by, "))"),
                      "plotDiffContours" = paste0("plotDiffContours(eList, qUnit=",qUnit,", year0=",yearStart,",year1 = ",
                                                  yearEnd,", qBottom = ",qLow,", qTop = ",qHigh, ", maxDiff = ",maxDiff,")")
    )
    
    HTML(paste0("setPA(eList, paStart = ",paStart, ", paLong = ", paLong,")\n",
                outText))
    
  })
  
  output$mymap <- leaflet::renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -99.5, lat = 40, zoom=4) 
    
  })

  output$textMessage <- renderUI({
    eList <- eList()
    INFO <- eList$INFO
    HTML(paste0("Data retrieved from sciencebase: ",INFO$shortName))
    
  })
  
  
})
