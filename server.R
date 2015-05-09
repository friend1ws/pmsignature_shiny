library(shiny)
library(BH)
library(ggplot2)
library(dplyr)
library(Rcpp)
library(GenomicRanges)
library(S4Vectors)
library(IRanges)
library(XVector)
library(Biostrings)
# library(BSgenome)
library(BSgenome.Hsapiens.UCSC.hg19)
library(pmsignature)

shinyServer(function(input, output) {
  
  estimationResult <- reactive({
    # Take a dependency on input$goButton
    input$goButton
       
    # Use isolate() to avoid dependencies
    inFile <- isolate(input$mutFile)
    K <- isolate(input$sigNum)
    if (is.null(inFile))
      return(NULL)

    print(inFile);
    
    withProgress(message = 'Reading and formating input file', value = 0.2, {
      Sys.sleep(0.1)
      G <- readMPFile(inFile$datapath, numBases = 5)
      incProgress(0.4, message = 'Estimating mutation signatures')
      Sys.sleep(0.1)
      Param <- getPMSignature(G, K, numInit = 1)
    })
    
    return(list(G, Param))
  })
  
  
  output$signature <- renderPlot({
    
    scaleOrNot <- isolate(input$scale)
    res <- estimationResult();
    if (is.null(res)) {
      return();
    } 
    
    res <- estimationResult();
    Param <- res[[2]];
    par(mfrow = c(2, 3))
    for (i in 1:K) {
      visPMSignature(Param, i, isScale = scaleOrNot)
    }
  })
  
  output$membership <- renderPlot({
    
    res <- estimationResult();
    if (is.null(res)) {
      return();
    } 
    
    res <- estimationResult();
    G <- res[[1]];
    Param <- res[[2]];
    visMembership(G, Param);
    
  })
  
})