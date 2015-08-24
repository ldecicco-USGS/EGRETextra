shinyUI(
  fluidPage(
    h2("EGRET Exploration"),
    fluidRow(fileInput("data", "Load EGRET object")),
    fluidRow(
      column(3, h3("Period of Analysis:")),
      column(4,
             selectInput("paStart", label = "Starting Month", 
                         choices = c(month.name),
                         selected = "October", multiple = FALSE)),
      column(4,
             selectInput("paLong", label = "Number of Months", 
                         choices = 1:12,
                         selected = 12, multiple = FALSE))
      ),
    fluidRow(
      column(3, h3("Units")),
      column(4,
             selectInput("fluxUnit", label = "Flux Units", 
                         choices = list("pounds/day" = 1,
                                        "tons/day" = 2,
                                        "kg/day" = 3,
                                        "thousands of kg/day" = 4,
                                        "tons/year" = 5,
                                        "thousands of tons/year" = 6,
                                        "millions of tons/year" = 7,
                                        "thousands of kg/year" = 8,
                                        "millions of kg/year" = 9,
                                        "billions of kg/year" = 10,
                                        "thousands of tons/day" = 11,
                                        "millions of kg/day" = 12,
                                        "kg/year" = 13),
                         selected=3, multiple = FALSE)
             
             ),
      column(4,
             selectInput("qUnit", label = "Flow Units", 
                         choices = list("Cubic Feet per Second" = 1,
                                        "Cubic Meters per Second" = 2,
                                        "Thousand Cubic Feet per Second" = 3,
                                        "Thousand Cubic Meters per Second" = 4),
                         selected=1, multiple = FALSE)
             
      )
    ),
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
                        htmlOutput("SampleText"),
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
                          
                        ),
                        column(8,
                               plotOutput("modelPlotsOut"))
                    ))
             )),
      column(1)
    )
    
  )
)