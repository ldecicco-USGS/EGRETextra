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
      logScale = as.logical(as.integer(input$logScaleFlow))
    }
    
    switch(input$flowPlots,
           "plotFlowSingle" = plotFlowSingle(eList, istat=stat, qUnit = qUnit),
           "plotSDLogQ" = plotSDLogQ(eList),
           "plotQTimeDaily" = plotQTimeDaily(eList, qUnit = qUnit, logScale = logScale),
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
    
    if(is.null(input$logScaleData)){
      logScale = FALSE
    } else {
      logScale = as.logical(as.integer(input$logScaleData))
    }
    
    switch(input$dataPlots,
           "boxConcMonth" = boxConcMonth(eList, logScale = logScale),
           "boxQTwice" = boxQTwice(eList, qUnit = qUnit),
           "plotConcTime" = plotConcTime(eList, logScale = logScale),
           "plotConcQ" = plotConcQ(eList, qUnit = qUnit, logScale = logScale),
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
    
    if(is.null(input$fluxUnit)){
      fluxUnit = 3
    } else {
      fluxUnit = as.integer(input$fluxUnit)
    }
    
    switch(input$modelPlots,
           "plotConcTimeDaily" = plotConcTimeDaily(eList),
           "plotFluxTimeDaily" = plotFluxTimeDaily(eList, fluxUnit=fluxUnit),
           "plotConcPred" = plotConcPred(eList),
           "plotFluxPred" = plotFluxPred(eList, fluxUnit=fluxUnit),
           "plotResidPred" = plotResidPred(eList),
           "plotResidQ" = plotResidQ(eList, qUnit=qUnit),
           "plotResidTime" = plotResidTime(eList),
           "boxResidMonth" = boxResidMonth(eList),
           "boxConcThree" = boxConcThree(eList),
           "plotConcHist" = plotConcHist(eList),
           "plotFluxHist" = plotFluxHist(eList, fluxUnit=fluxUnit),
           # "plotConcQSmooth" = plotConcQSmooth(eList),
           # "plotConcTimeSmooth" = plotConcTimeSmooth(eList),
           "fluxBiasMulti" = fluxBiasMulti(eList, fluxUnit=fluxUnit, qUnit=qUnit)
           # "plotContours" = plotContours(eList, qUnit=qUnit),
           # "plotDiffContours" = plotDiffContours(eList, qUnit=qUnit)
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
  
  output$flowLog <- renderUI({
    if(input$flowPlots == "plotQTimeDaily"){
      radioButtons("logScaleFlow", label = h3("Scale"),
                   choices = list("Linear" = 0, "Log" = 1), 
                   selected = 0)
    }
  })
  
  output$dataLog <- renderUI({
    if(input$dataPlots %in% c("boxConcMonth", "plotConcTime", "plotConcQ")){
      radioButtons("logScaleData", label = h3("Scale"),
                   choices = list("Linear" = 0, "Log" = 1), 
                   selected = 0)
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
      logScale = as.logical(as.integer(input$logScaleFlow))
    }
    
    outText <- switch(input$flowPlots,
           "plotFlowSingle" = paste0("plotFlowSingle(eList, istat=", stat,", qUnit = ", qUnit, ")"),
           "plotSDLogQ" = paste0("plotSDLogQ(eList", ")"),
           "plotQTimeDaily" = paste0("plotQTimeDaily(eList, logScale = ",logScale,", qUnit = ", qUnit, ")"),
           "plotFour" = paste0("plotFour(eList, qUnit = ", qUnit, ")"),
           "plotFourStats" = paste0("plotFourStats(eList, qUnit = ", qUnit, ")")
           
    )
    
    HTML(paste0("<h5>setPA(eList, paStart = ",paStart, ", paLong = ", paLong,")</h5>",
                "<h5>",outText,"</h5>"))
    
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
      logScale = as.logical(as.integer(input$logScaleData))
    }
    
    outText <- switch(input$dataPlots,
           "boxConcMonth" = paste0("boxConcMonth(eList, logScale = ", logScale,")"),
           "boxQTwice" = paste0("boxQTwice(eList, qUnit = ", qUnit, ")"),
           "plotConcTime" = paste0("plotConcTime(eList, logScale = ", logScale,")"),
           "plotConcQ" = paste0("plotConcQ(eList, logScale = ", logScale,", qUnit = ", qUnit, ")"),
           "multiPlotDataOverview" = paste0("multiPlotDataOverview(eList, qUnit = ", qUnit, ")")
           
    )
    
    HTML(paste0("<h5>setPA(eList, paStart = ",paStart, ", paLong = ", paLong,")</h5>",
                "<h5>",outText,"</h5>"))
    
  })
  
  output$modelCode <- renderPrint({
    
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
    
    outText <- switch(input$modelPlots,
           "plotConcTimeDaily" = paste0("plotConcTimeDaily(eList)"),
           "plotFluxTimeDaily" = plotFluxTimeDaily(eList, fluxUnit=fluxUnit),
           "plotConcPred" = paste0("plotConcPred(eList)"),
           "plotFluxPred" = paste0("plotFluxPred(eList, fluxUnit = ", fluxUnit, ")"),
           "plotResidPred" = paste0("plotResidPred(eList)"),
           "plotResidQ" = paste0("plotResidQ(eList, qUnit = ", qUnit, ")"),
           "plotResidTime" = paste0("plotResidTime(eList)"),
           "boxResidMonth" = paste0("boxResidMonth(eList)"),
           "boxConcThree" = paste0("boxConcThree(eList)"),
           "plotConcHist" = paste0("plotConcHist(eList)"),
           "plotFluxHist" = paste0("plotFluxHist(eList, fluxUnit = ", fluxUnit, ")"),
           # "plotConcQSmooth" = plotConcQSmooth(eList),
           # "plotConcTimeSmooth" = plotConcTimeSmooth(eList),
           "fluxBiasMulti" = paste0("fluxBiasMulti(eList, qUnit = ", qUnit,", fluxUnit = ", fluxUnit, ")")
           # "plotContours" = plotContours(eList, qUnit=qUnit),
           # "plotDiffContours" = plotDiffContours(eList, qUnit=qUnit)
    )
    
    HTML(paste0("<h5>setPA(eList, paStart = ",paStart, ", paLong = ", paLong,")</h5>",
                "<h5>",outText,"</h5>"))
    
  })
  
})
