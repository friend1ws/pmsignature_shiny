library(shiny)



shinyUI(fluidPage(
  title = "pmsignture for shiny",
  
  titlePanel("pmsignature"), 
  
  fluidRow(
    
    column(3,
      wellPanel(
  
        fileInput('mutFile', "Update the input file (only mutation position format)" ),  
        numericInput("sigNum", label = "#signature", value = 3, min = 2, max = 6),
        radioButtons("type", label = "signature type",
                     choices = list("independent", "full"), selected = "independent"),
        numericInput("flank", label = "#flanking bases", value = 5, min = 3, max = 5, step = 2),
        checkboxInput("strand", label = "transcription strand", value = FALSE),
        conditionalPanel(
          condition = "input.type == 'independent'",
          checkboxInput("scale", label = "scaling heights of flanking bases", value = TRUE)
        ),
        actionButton("goButton", label = "Execute!"),
        tags$br(),
        tags$br(),
        downloadLink('exampleDownload', 'Example Data Download')
      )
    ),
      
    column(9,
      fluidRow(
        column(4, plotOutput('signature1')),
        column(4, plotOutput('signature2')), 
        column(4, plotOutput('signature3'))
      ),
      uiOutput("sigNumControls"),
      plotOutput('membership')
    )
    
  )
  
))