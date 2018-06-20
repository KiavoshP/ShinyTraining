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
