library(shiny)

shinyUI(fluidPage(
  title = "pmsignture for shiny",
  
  titlePanel("pmsignature"), 
  
  sidebarLayout(
  
    sidebarPanel(
      fileInput('mutFile', "Update the input file (only mutation position format)"),
      numericInput("sigNum", label = "#signature", value = 3, min = 2, max = 6),
      checkboxInput("scale", label = "scaling flanking bases", value = FALSE),
      actionButton("goButton", label = "Execute!")
    ),
  
    mainPanel(
      plotOutput('signature'),
      plotOutput('membership')
    )
    
  )
  
))