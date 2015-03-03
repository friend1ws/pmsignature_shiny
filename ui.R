library(shiny)

shinyUI(fluidPage(
  title = "pmsignture for shiny",
  
  titlePanel("pmsignature"),
  
  plotOutput('signature'),
  
  hr(),
  
  fluidRow(
    mainPanel(
      fileInput('mutFile', "Update the input file (only mutation position format)"),
      numericInput("sigNum", label = "#signature", value = 3, min = 2, max = 6),
      actionButton("goButton", "Execute!")
    )
  )
))