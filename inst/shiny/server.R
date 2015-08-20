library(EGRET)

eList <- Choptank_eList


shinyServer(function(input, output) {
  
  output$flowPlotsOut <- renderPlot({ 
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
    
    switch(input$dataPlots,
           "boxConcMonth" = boxConcMonth(eList),
           "boxQTwice" = boxQTwice(eList),
           "plotConcTime" = plotConcTime(eList),
           "plotConcQ" = plotConcQ(eList),
           "multiPlotDataOverview" = multiPlotDataOverview(eList)
           
    )
  })
  
})
