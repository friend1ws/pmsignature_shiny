library(shiny)
library(VariantAnnotation)
library(BSgenome.Hsapiens.UCSC.hg19)
library(pmsignature)

shinyServer(function(input, output) {
  output$signature <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    input$goButton
    
    inFile <- isolate(input$mutFile)
    K <- isolate(input$sigNum)
    
    if (is.null(inFile))
      return(NULL)

    withProgress(message = 'Reading and formating input file', value = 0.2, {
      Sys.sleep(0.1)
      G <- readMPFile(inFile$datapath, 5)
      incProgress(0.4, message = 'Estimating mutation signatures')
      Sys.sleep(0.1)
      Param <- getPMSignature(G, K, numInit = 1)
    })
    

    par(mfrow = c(2, 3))
    for (i in 1:K) {
      visPMSignature(Param, i)
    }
    
    
  })
})