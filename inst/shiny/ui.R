library(EGRET)

eList <- Choptank_eList

shinyUI(
  fluidPage(
    h2("EGRET Exploration"),
    fluidRow(
      column(11,
             tabsetPanel(
               tabPanel("Flow History",
                        fluidRow(
                          column(4,
                              selectInput("flowPlots", label = "Flow History", 
                                 choices = c("plotFlowSingle","plotSDLogQ","plotQTimeDaily","plotFour","plotFourStats"),
                                 selected = "plotFlowSingle", multiple = FALSE),
                              selectInput("flowStat", label = "Flow Statistic", 
                                          choices = list("1-day minimum"=1, "7-day minimum"=2, "30-day minimum"=3, "median"=4,
                                                         "mean"=5, "30-day maximum"=6, "7-day maximum"=7, "1-day maximum"=8),
                                          selected = 5, multiple = FALSE)
                                 
                                 ),
                          column(8,
                                 plotOutput("flowPlotsOut"))
                        )),
               tabPanel("Explore Data",
                        fluidRow(
                          column(4,
                             selectInput("dataPlots", label = "Data", 
                                 choices = c("boxConcMonth","boxQTwice","plotConcTime","plotConcQ","multiPlotDataOverview"),
                                 selected = "multiPlotDataOverview", multiple = FALSE) 
                          
                        ),
                        column(8,
                               plotOutput("dataPlotsOut"))
                      )),
               tabPanel("Explore Model",
                        fluidRow(
                          column(4,
                             selectInput("modelPlots", label = "Data", 
                                 choices = c("plotConcTimeDaily","plotFluxTimeDaily","plotConcPred","plotFluxPred","plotResidPred",
                                             "plotResidQ","plotResidTime","boxResidMonth","boxConcThree",
                                             "plotConcHist","plotFluxHist","plotConcQSmooth","plotConcTimeSmooth",
                                             "fluxBiasMulti","plotContours","plotDiffContours"),
                                 selected = "fluxBiasMulti", multiple = FALSE)  
                          
                        )
                    ))               
             )),
      column(1)
    )
    
  )
)