library(shiny)
options(shiny.maxRequestSize = 500 * 1024 ^ 2)
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
