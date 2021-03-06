library(shinydashboard)
tempFolder <- tempdir()


header <- dashboardHeader(title = "Explore EGRET")

body <- dashboardBody(
  
   tabsetPanel(selected = "exploreData",id = "analyzeChoices",
               tabPanel(title = tagList("Get Data",shiny::icon("folder-o")),
                        value = "getData",
                        fileInput("data", "Load EGRET object")
               ),
               tabPanel(title = tagList("MetaData",shiny::icon("bars")),
                        value = "metaData",
                        DT::dataTableOutput('metaData')
               ),
               tabPanel(title = tagList("Flow History",shiny::icon("bar-chart")),
                        value = "flowHistory",
                        h4("R Code:"),
                        verbatimTextOutput("flowCode"),
                        plotOutput("flowPlotsOut"),
                        downloadButton('downloadFlowPlot', 'Download Plot')),
               tabPanel(title = tagList("Explore Data",shiny::icon("bar-chart")),
                        value = "exploreData",
                        htmlOutput("SampleText"),
                        h4("R Code:"),
                        verbatimTextOutput("dataCode"),
                        plotOutput("dataPlotsOut"),
                        downloadButton('downloadDataPlot', 'Download Plot')),
               tabPanel(title = tagList("Explore Model",shiny::icon("bar-chart")),
                        value = "exploreModel",
                        h4("R Code:"),
                        verbatimTextOutput("modelCode"),
                        plotOutput("modelPlotsOut"),
                        downloadButton('downloadModelPlot', 'Download Plot'))
   ),
  fluidRow(
    column(1, HTML('<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="300" height="75"><path id="USGS" d="m234.95 15.44v85.037c0 17.938-10.132 36.871-40.691 36.871-27.569 0-40.859-14.281-40.859-36.871v-85.04h25.08v83.377c0 14.783 6.311 20.593 15.447 20.593 10.959 0 15.943-7.307 15.943-20.593v-83.377h25.08m40.79 121.91c-31.058 0-36.871-18.27-35.542-39.03h25.078c0 11.462 0.5 21.092 14.282 21.092 8.472 0 12.62-5.482 12.62-13.618 0-21.592-50.486-22.922-50.486-58.631 0-18.769 8.968-33.715 39.525-33.715 24.42 0 36.543 10.963 34.883 36.043h-24.419c0-8.974-1.492-18.106-11.627-18.106-8.136 0-12.953 4.486-12.953 12.787 0 22.757 50.493 20.763 50.493 58.465 0 31.06-22.75 34.72-41.85 34.72m168.6 0c-31.06 0-36.871-18.27-35.539-39.03h25.075c0 11.462 0.502 21.092 14.285 21.092 8.475 0 12.625-5.482 12.625-13.618 0-21.592-50.494-22.922-50.494-58.631 0-18.769 8.969-33.715 39.531-33.715 24.412 0 36.536 10.963 34.875 36.043h-24.412c0-8.974-1.494-18.106-11.625-18.106-8.144 0-12.955 4.486-12.955 12.787 0 22.757 50.486 20.763 50.486 58.465 0 31.06-22.75 34.72-41.85 34.72m-79.89-46.684h14.76v26.461l-1.229 0.454c-3.816 1.332-8.301 2.327-12.453 2.327-14.287 0-17.943-6.645-17.943-44.177 0-23.256 0-44.348 15.615-44.348 12.146 0 14.711 8.198 14.933 18.107h24.981c0.198-23.271-14.789-36.043-38.42-36.043-41.021 0-42.52 30.724-42.52 60.954 0 45.507 4.938 63.167 47.12 63.167 9.784 0 25.36-2.211 32.554-4.18 0.436-0.115 1.212-0.596 1.212-1.216v-59.598h-38.612v18.09" style="fill:rgb(40%,40%,40%); fill-opacity: 0.3" transform="scale(0.5)"/>
                   <path id="waves" d="m48.736 55.595l0.419 0.403c11.752 9.844 24.431 8.886 34.092 2.464 6.088-4.049 33.633-22.367 49.202-32.718v-10.344h-116.03v27.309c7.071-1.224 18.47-0.022 32.316 12.886m43.651 45.425l-13.705-13.142c-1.926-1.753-3.571-3.04-3.927-3.313-11.204-7.867-21.646-5.476-26.149-3.802-1.362 0.544-2.665 1.287-3.586 1.869l-28.602 19.13v34.666h116.03v-24.95c-2.55 1.62-18.27 10.12-40.063-10.46m-44.677-42.322c-0.619-0.578-1.304-1.194-1.915-1.698-13.702-10.6-26.646-5.409-29.376-4.116v11.931l6.714-4.523s10.346-7.674 26.446 0.195l-1.869-1.789m16.028 15.409c-0.603-0.534-1.214-1.083-1.823-1.664-12.157-10.285-23.908-7.67-28.781-5.864-1.382 0.554-2.7 1.303-3.629 1.887l-13.086 8.754v12.288l21.888-14.748s10.228-7.589 26.166 0.054l-0.735-0.707m68.722 12.865c-4.563 3.078-9.203 6.203-11.048 7.441-4.128 2.765-13.678 9.614-29.577 2.015l1.869 1.797c0.699 0.63 1.554 1.362 2.481 2.077 11.418 8.53 23.62 7.303 32.769 1.243 1.267-0.838 2.424-1.609 3.507-2.334v-12.234m0-24.61c-10.02 6.738-23.546 15.833-26.085 17.536-4.127 2.765-13.82 9.708-29.379 2.273l1.804 1.729c0.205 0.19 0.409 0.375 0.612 0.571l-0.01 0.01 0.01-0.01c12.079 10.22 25.379 8.657 34.501 2.563 5.146-3.436 12.461-8.38 18.548-12.507l-0.01-12.165m0-24.481c-14.452 9.682-38.162 25.568-41.031 27.493-4.162 2.789-13.974 9.836-29.335 2.5l1.864 1.796c1.111 1.004 2.605 2.259 4.192 3.295 10.632 6.792 21.759 5.591 30.817-0.455 6.512-4.351 22.528-14.998 33.493-22.285v-12.344" style="fill:rgb(40%,40%,40%); fill-opacity: 0.3" transform="scale(0.5)"/></svg>')
    ),
    column(11,
           h4("Disclaimer"),
           h5("This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey (USGS), an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at http://www.usgs.gov/visual-id/credit_usgs.html#copyright
              Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.
              This software is provided 'AS IS.'"))
    
           )  
    )

sidebar <- dashboardSidebar(
  conditionalPanel(
    condition = "input.analyzeChoices == 'exploreModel'",
    selectInput("modelPlots", label = "Choose Plot:", 
                choices = c("plotConcTimeDaily","plotFluxTimeDaily","plotConcPred","plotFluxPred","plotResidPred",
                            "plotResidQ","plotResidTime","boxResidMonth","boxConcThree",
                            "plotConcHist","plotFluxHist","plotConcQSmooth","plotConcTimeSmooth",
                            "fluxBiasMulti","plotContours","plotDiffContours"),
                selected = "fluxBiasMulti", multiple = FALSE)
  ),
  conditionalPanel(
    condition = paste("input.analyzeChoices == 'exploreModel' && (",
                      'input.modelPlots == "plotConcTimeSmooth" ||',
                      'input.modelPlots == "plotContours" ||',
                      'input.modelPlots == "plotDiffContours")'),
    sliderInput("yearRange", "Year Range:",sep = "",
                min = 1970, max = 2012, value = c(1982,2012))      
  ),
  conditionalPanel(
    condition = paste("input.analyzeChoices == 'exploreModel' && (",
                      'input.modelPlots == "plotConcPred" ||',
                      'input.modelPlots == "plotConcQSmooth" ||',
                      'input.modelPlots == "plotConcTimeSmooth")'),
    checkboxInput("logScaleModel", label = "Log Scale", value = TRUE)      
  ),  
  conditionalPanel(
    condition = paste("input.analyzeChoices == 'exploreModel' && (",
                      'input.modelPlots == "plotConcQSmooth" ||',
                      'input.modelPlots == "plotConcTimeSmooth" ||',
                      'input.modelPlots == "plotDiffContours" ||',
                      'input.modelPlots == "plotContours")'),
    sliderInput("flowRange", "Discharge Range:",sep = "",
                min = 0, max = 1, value = c(0.1,0.9), round = TRUE)        
  ), 
  conditionalPanel(
    condition = paste("input.analyzeChoices == 'exploreModel' && ",
                      'input.modelPlots == "plotConcQSmooth"'),
    dateInput("date1", label = "date1", 
              value = "1992-01-01"),
    dateInput("date2", label = "date2", 
              value = "1998-01-01"),
    dateInput("date3", label = "date3", 
              value = "2005-01-01"),
    textInput("centerDate", label = h5("centerDate"), value = "04-01")     
  ),
  conditionalPanel(
    condition = paste("input.analyzeChoices == 'exploreModel' && ",
                      'input.modelPlots == "plotDiffContours"'),
    numericInput("maxDiff", label = "maxDiff", value = 1)
  ),
  conditionalPanel(
    condition = paste("input.analyzeChoices == 'exploreModel' && (",
                      'input.modelPlots == "plotConcQSmooth" ||',
                      'input.modelPlots == "plotConcTimeSmooth" )'),
    numericInput("qMid", label = "qMid", value = 1)
  ),
  conditionalPanel(
    condition = paste("input.analyzeChoices == 'exploreModel' && ",
                      'input.modelPlots == "plotContours"'),
    sliderInput("concRange", "Concentration Range:",sep = "",
                min = 0, max = 1, value = c(0.1,0.9), round = TRUE),
    numericInput("by", label = h5("Number of divisions"), value = 5) 
  ),  
  conditionalPanel(
    condition = "input.analyzeChoices == 'exploreData'",
    selectInput("dataPlots", label = "Choose Plot:", 
                choices = c("boxConcMonth","boxQTwice","plotConcTime","plotConcQ","multiPlotDataOverview"),
                selected = "multiPlotDataOverview", multiple = FALSE)
  ),
  conditionalPanel(
    condition = paste("input.analyzeChoices == 'exploreData' && (",
                      'input.dataPlots == "boxConcMonth" ||',
                      'input.dataPlots == "plotConcTime" ||',
                      'input.dataPlots == "plotConcQ")'),
    checkboxInput("logScaleData", label = "Log Scale", value = FALSE)     
  ),
  conditionalPanel(
    condition = paste("input.analyzeChoices == 'exploreModel' && (",
                      'input.modelPlots == "plotResidPred" ||',
                      'input.modelPlots == "plotResidQ" ||',
                      'input.modelPlots == "plotResidTime" ||',
                      'input.modelPlots == "boxResidMonth" ||',
                      'input.modelPlots == "fluxBiasMulti")'),
    checkboxInput("rResid", label = "Randomize Residuals", value = TRUE)     
  ),
  conditionalPanel(
    condition = "input.analyzeChoices == 'flowHistory'",
    selectInput("flowPlots", label = "Choose Plot:", 
                choices = c("plotFlowSingle","plotSDLogQ","plotQTimeDaily","plotFour","plotFourStats"),
                selected = "plotFlowSingle", multiple = FALSE)
  ),
  conditionalPanel(
    condition = paste("input.analyzeChoices == 'flowHistory' && ",
                      'input.flowPlots == "plotFlowSingle"'),
    selectInput("flowStat", label = "Flow Statistic", 
                choices = list("1-day minimum"=1, "7-day minimum"=2, "30-day minimum"=3, "median"=4,
                               "mean"=5, "30-day maximum"=6, "7-day maximum"=7, "1-day maximum"=8),
                selected = 5, multiple = FALSE)     
  ), 
  conditionalPanel(
    condition = paste("input.analyzeChoices == 'flowHistory' && ",
                      'input.flowPlots == "plotQTimeDaily"'),
    checkboxInput("logScaleFlow", label = "Log Scale", value = FALSE)     
  ),
  conditionalPanel(
    condition = "input.analyzeChoices == 'exploreTrend'",
    selectInput("trendPlots", label = "Choose Plot:", 
                choices = c("plotHistogramTrendConc","plotHistogramTrendFlux",
                            "plotFluxHistBoot","plotConcHistBoot"),
                selected = "plotHistogramTrendConc", multiple = FALSE)
  ),
  selectInput("paStart", label = "Starting Month", 
              choices = c(month.name),
              selected = "October", multiple = FALSE),
  selectInput("paLong", label = "Number of Months", 
              choices = 1:12,
              selected = 12, multiple = FALSE),
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
              selected=3, multiple = FALSE),
  selectInput("qUnit", label = "Flow Units", 
              choices = list("Cubic Feet per Second" = 1,
                             "Cubic Meters per Second" = 2,
                             "Thousand Cubic Feet per Second" = 3,
                             "Thousand Cubic Meters per Second" = 4),
              selected=1, multiple = FALSE),
  sidebarMenu(id="source",
              menuItem("Source code", icon = icon("file-code-o"), 
                       href = "https://github.com/USGS-R/EGRETextra/tree/master/inst/shiny")
  )
)

dashboardPage(header, 
              sidebar,
              body)

