library(EGRET)

eList <- Choptank_eList

shinyUI(
  fluidPage(
    
    fluidRow(
      column(1),
      column(10,
             tabsetPanel(
               tabPanel("Flow History",
                        fluidRow(
                          column(4,
                              selectInput("flowPlots", label = "Flow History", 
                                 choices = c("plotFlowSingle","plotSDLogQ","plotQTimeDaily","plotFour","plotFourStats"),
                                 selected = "plotFlowSingle", multiple = FALSE) 
                                 
                                 )
                        )),
               tabPanel("Explore Data",
                        fluidRow(
                          column(4,
                             selectInput("data", label = "Data", 
                                 choices = c("Water Sample",
                                             "Passive Samples"),
                                 selected = "Water Sample + ToxCast", multiple = FALSE) 
                          
                        )
                      )),
               tabPanel("Explore Model",
                        fluidRow(
                          column(4,
                             selectInput("data", label = "Data", 
                                 choices = c("Water Sample",
                                             "Passive Samples"),
                                 selected = "Water Sample + ToxCast", multiple = FALSE)  
                          
                        )
                    ))               
             )),
      column(1)
    )
    
  )
)