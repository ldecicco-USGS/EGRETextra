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
                          column(3,
                              selectInput("flowPlots", label = "Flow History", 
                                 choices = c("plotFlowSingle","plotSDLogQ","plotQTimeDaily","plotFour","plotFourStats"),
                                 selected = "plotFlowSingle", multiple = FALSE),
                              uiOutput("flowStatistic"),
                              uiOutput("flowLog"),
                              h4("R Code:"),
                              htmlOutput("flowCode")),
                          column(9,
                                 plotOutput("flowPlotsOut"),
                                 downloadButton('downloadFlowPlot', 'Download Plot'))
                        )),
               tabPanel("Explore Data",
                        htmlOutput("SampleText"),
                        fluidRow(
                          column(3,
                             selectInput("dataPlots", label = "Data", 
                                 choices = c("boxConcMonth","boxQTwice","plotConcTime","plotConcQ","multiPlotDataOverview"),
                                 selected = "multiPlotDataOverview", multiple = FALSE) ,
                             uiOutput("dataLog"),
                             h4("R Code:"),
                             htmlOutput("dataCode")),
                        column(9,
                               plotOutput("dataPlotsOut"),
                               downloadButton('downloadDataPlot', 'Download Plot'))
                      )),
               tabPanel("Explore Model",
                        fluidRow(
                          column(3,
                             selectInput("modelPlots", label = "Data", 
                                 choices = c("plotConcTimeDaily","plotFluxTimeDaily","plotConcPred","plotFluxPred","plotResidPred",
                                             "plotResidQ","plotResidTime","boxResidMonth","boxConcThree",
                                             "plotConcHist","plotFluxHist","plotConcQSmooth","plotConcTimeSmooth",
                                             "fluxBiasMulti","plotContours","plotDiffContours"),
                                 selected = "fluxBiasMulti", multiple = FALSE),
                             uiOutput("modelLog"),
                             uiOutput("date1"),
                             uiOutput("date2"),
                             uiOutput("date3"),
                             uiOutput("qLow"),
                             uiOutput("qMid"),
                             uiOutput("qHigh"),
                             uiOutput("yearStart"),
                             uiOutput("yearEnd"),
                             uiOutput("centerDate"),
                             uiOutput("maxDiff"),
                             uiOutput("from"),
                             uiOutput("to"),
                             uiOutput("by"),
                             uiOutput("rResid"),
                             uiOutput("animate"),
                             h4("R Code:"),
                             htmlOutput("modelCode")),
                        column(9,
                               plotOutput("modelPlotsOut"),
                               downloadButton('downloadModelPlot', 'Download Plot'))
                    
                    ))
             )),
      column(1)
    )
    
  )
)