library(shiny)
options(shiny.maxRequestSize = 500 * 1024 ^ 2)
source("functions/libraries.R")
source("functions/datapreparation.R")
source("functions/optimization.R")
source("functions/utilities.R")
source("functions/limits.R")
source("functions/visualization.R")

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Recommendation Intelligence"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----

      fileInput("limitsFile", "Choose limits file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      fileInput("dataFile", "Choose offline data",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      h6(uiOutput("responseTagData")),
      uiOutput("leverBase"),
      uiOutput("leverAdjuster"),

      # Input
      h5("Inputs"),
      fluidRow(column(width = 4, numericInput("numLags", "Lags in model", 1, min = 0, max = 100)),
               column(width = 4, numericInput("trainingSize", "Training Size", 120, min = 100, max = 1e+10))
              ),

      h5("Prediction Model Config"),
      fluidRow(column(width = 4, numericInput("gamma", "gamma", 0.01, min = 0, max = 1)),
               column(width = 4, numericInput("cost", "cost", 1, min = 0, max = 100)),
               column(width = 4, numericInput("epsilon", "epsilon", 0.1, min = 0, max = 1))
               ),

      h5("Optim Config"),
      fluidRow(column(width = 4, numericInput("runs", "Runs", 10, min = 1, max = 1e+3)),
               column(width = 4, numericInput("fnscale", "fnscale", 1, min = -1, max = 1)),
               column(width = 4, checkboxInput("rounding", "Lever rounding", value = TRUE, width = NULL))
               ),

      # Horizontal line ----

      actionButton(inputId = "runRecommendation",
                   label = "Run",
                   icon(name = "angle-right", lib = "font-awesome")),

      # Horizontal line ----

      uiOutput("leverResultToShow"),

      tags$hr(),
      # Button
      fluidRow(column(width = 6, downloadButton("downloadPerformance", "Performance")),
               column(width = 6, downloadButton("downloadLeverChangeLog", "Lever change"))
      )

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      tabsetPanel(type = "tabs",
                  tabPanel("Performance",
                           plotOutput("recommendationResponsePlot"),
                           tableOutput("recommendationLogResponseData")
                           ),
                  tabPanel("Lever",
                           plotOutput("recommendationLogLeverPlot"),
                           tableOutput("recommendationLogLeverData")
                           ),
                  tabPanel("Tags Info",
                           tableOutput("specialTagsInfo"),
                           tableOutput("tagIDlistData"),
                           tableOutput("rawLimitsData")
                          )
      )
    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {

  rawLimits <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$limitsFile)

    tryCatch(
      {
        rawLimits <- read.csv(file = input$limitsFile$datapath,
                              header = T,
                              stringsAsFactors = F)

      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(rawLimits)
  })

  responseTag <- reactive({
    return(TagsOfInterest(rawLimits = rawLimits())$responseTag)
  })

  output$responseTagData <- renderText({
    req(input$limitsFile)
    paste("Response", responseTag())
  })

  rawData <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$dataFile)

    tryCatch(
      {
        rawData <- read.csv(file = input$dataFile$datapath,
                            header = T,
                            stringsAsFactors = F)

      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(rawData)
  })

  tagIDlist <- reactive({
    return(rawLimits()[, c("tagID", "type")])
  })

  output$leverBase <- renderUI({
    selectInput(inputId = "leverBaseIDs",
                label = "Select base levers",
                choices = tagIDlist()[tagIDlist()[, "type"] == "leverbase", "tagID"],
                multiple = T)
  })

  output$leverAdjuster <- renderUI({
    selectInput(inputId = "leverAdjusterID",
                label = "Select adjuster lever",
                choices = tagIDlist()[tagIDlist()[, "type"] == "leveradjuster", "tagID"],
                multiple = F,
                selected = NULL)
  })

  leverTags <- reactive({
    list(base = input$leverBaseIDs,
         adjuster = input$leverAdjuster)
  })


  datasetInput <- reactive({
    PrepareData(rawData = rawData(),
                responseTag = responseTag(),
                nlags = input$numLags)
  })


  output$inputData <- renderTable({
    datasetInput()
  })

  output$rawLimitsData <- renderTable({
    rawLimits()
  })

  output$tagIDlistData <- renderTable({
    tagIDlist()
  })

  output$specialTagsInfo <- renderTable({
    tmp <- data.frame(unlist(leverTags()))
    tmp <- cbind(rownames(tmp), tmp)
    colnames(tmp) <- c("type", "tagID")
    return(tmp)
  })

  limitParams <- reactive({
    LimitParams(rawLimits = rawLimits(),
                responseTag = responseTag(),
                leverTags = leverTags())
  })

  predModelConfig <- reactive({
    list(gamma = input$gamma, cost = input$cost, epsilon = input$epsilon)
  })

  optimConfig <- reactive({
    list(runs = input$runs, fnscale = input$fnscale)
  })

  recommendationLog <- eventReactive(input$runRecommendation, {
    OptimizerOfflineAnalysis(data = datasetInput(),
                             trainingSize = input$trainingSize,
                             responseTag = responseTag(),
                             leverTags = leverTags(),
                             limitParams = limitParams(),
                             predModelConfig = predModelConfig(),
                             optimConfig = optimConfig(),
                             rounding = input$rounding)
  })

  tagIDsInResult <- reactive({
    req(recommendationLog)
    names(recommendationLog()$lever)
  })

  output$leverResultToShow <- renderUI({
    selectInput(inputId = "leverResultToShow",
                label = "See output for lever",
                choices = tagIDsInResult(),
                multiple = F,
                selected = tagIDsInResult()[1])
  })

  output$recommendationLogResponseData <- renderTable({
    recommendationLog()$response[[responseTag()]]
  })

  output$recommendationResponsePlot <- renderPlot({
    PlotLine(df = recommendationLog()$response[[responseTag()]])
  })

  recommendationLogLever <- reactive({
    req(recommendationLog())
    return(recommendationLog()$lever[[input$leverResultToShow]])
  })

  output$recommendationLogLeverData <- renderTable({
    recommendationLogLever()
  })

  output$recommendationLogLeverPlot <- renderPlot({
    PlotLine(df = recommendationLogLever())
  })



  # Downloadable csv of selected dataset ----
  output$downloadPerformance <- downloadHandler(
    filename = "output-performance.csv",
    content = function(file) {
      write.csv(recommendationLog()$response[[responseTag()]], file, row.names = FALSE)
    }
  )

  output$downloadLeverChangeLog <- downloadHandler(
    filename = paste("output-lever-change-log-", input$leverResultToShow, ".csv", collapse = ""),
    content = function(file) {
      write.csv(recommendationLogLever(), file, row.names = FALSE)
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)
