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
      
      extension <- strsplit(fileName, "\\.")[[1]][2]
      fileName <- strsplit(fileName, "\\.")[[1]][1]
      
      if(extension == "rds"){
        eList_Start <- readRDS(input$data$datapath)
      } else {
        load(input$data$datapath)
        assign("eList_Start", get(fileName))
      }
      
    }
    
    eList <- setPA(eList_Start, paStart, paLong)
  
    
  })
  
  output$flowPlotsOut <- renderPlot({ 
    
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
    
    switch(input$flowPlots,
           "plotFlowSingle" = plotFlowSingle(eList, istat=stat, qUnit = qUnit),
           "plotSDLogQ" = plotSDLogQ(eList, qUnit = qUnit),
           "plotQTimeDaily" = plotQTimeDaily(eList, qUnit = qUnit),
           "plotFour" = plotFour(eList, qUnit = qUnit),
           "plotFourStats" = plotFourStats(eList, qUnit = qUnit)
           
           )
  })
  
  output$dataPlotsOut <- renderPlot({ 
    
    eList <- eList()
    
    if(is.null(input$qUnit)){
      qUnit = 1
    } else {
      qUnit = as.integer(input$qUnit)
    }
    
    switch(input$dataPlots,
           "boxConcMonth" = boxConcMonth(eList),
           "boxQTwice" = boxQTwice(eList, qUnit = qUnit),
           "plotConcTime" = plotConcTime(eList),
           "plotConcQ" = plotConcQ(eList, qUnit = qUnit),
           "multiPlotDataOverview" = multiPlotDataOverview(eList, qUnit = qUnit)
           
    )
  })
  
  output$modelPlotsOut <- renderPlot({
    
    eList <- eList()
    
    
    if(is.null(input$qUnit)){
      qUnit = 1
    } else {
      qUnit = as.integer(input$qUnit)
    }
    
    switch(input$modelPlots,
           "plotConcTimeDaily" = plotConcTimeDaily(eList),
           "plotFluxTimeDaily" = plotFluxTimeDaily(eList),
           "plotConcPred" = plotConcPred(eList),
           "plotFluxPred" = plotFluxPred(eList),
           "plotResidPred" = plotResidPred(eList),
           "plotResidQ" = plotResidQ(eList),
           "plotResidTime" = plotResidTime(eList),
           "boxResidMonth" = boxResidMonth(eList),
           "boxConcThree" = boxConcThree(eList),
           "plotConcHist" = plotConcHist(eList),
           "plotFluxHist" = plotFluxHist(eList),
           # "plotConcQSmooth" = plotConcQSmooth(eList),
           # "plotConcTimeSmooth" = plotConcTimeSmooth(eList),
           "fluxBiasMulti" = fluxBiasMulti(eList)
           # "plotContours" = plotContours(eList),
           # "plotDiffContours" = plotDiffContours(eList)
           )
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
  
})
