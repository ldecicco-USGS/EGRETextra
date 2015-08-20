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
    
    eList <- setPA(eList_Start, paStart, paLong)
  
    
  })
  
  output$flowPlotsOut <- renderPlot({ 
    
    eList <- eList()
    
    if(is.null(input$flowStat)){
      stat=5
    } else {
      stat = as.integer(input$flowStat)
    }
    
    switch(input$flowPlots,
           "plotFlowSingle" = plotFlowSingle(eList, istat=stat),
           "plotSDLogQ" = plotSDLogQ(eList),
           "plotQTimeDaily" = plotQTimeDaily(eList),
           "plotFour" = plotFour(eList),
           "plotFourStats" = plotFourStats(eList)
           
           )
  })
  
  output$dataPlotsOut <- renderPlot({ 
    
    eList <- eList()
    
    switch(input$dataPlots,
           "boxConcMonth" = boxConcMonth(eList),
           "boxQTwice" = boxQTwice(eList),
           "plotConcTime" = plotConcTime(eList),
           "plotConcQ" = plotConcQ(eList),
           "multiPlotDataOverview" = multiPlotDataOverview(eList)
           
    )
  })
  
})
