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
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(pmsignature)



shinyServer(function(input, output) {
  
  output$exampleDownload <- downloadHandler(
    filename = "Nik_Zainal_2012.mutationPositionFormat.txt",
    content = function(file) {
      inputFile <- system.file("extdata/Nik_Zainal_2012.mutationPositionFormat.txt", package="pmsignature");
      inputTable <- read.table(inputFile);
      write.table(inputTable, file, quote = FALSE, row.names = FALSE, col.names = FALSE, sep="\t");
    }
  )
  
  estimationResult <- reactive({
    # Take a dependency on input$goButton
    input$goButton;
       
    # Use isolate() to avoid dependencies
    inFile <- isolate(input$mutFile);
    K <- isolate(input$sigNum);
    isStrand <- isolate(input$strand);
    isBG <- isolate(input$backGround);
    indOrFull <- isolate(input$type);
    flank <- isolate(input$flank);
    
    if (is.null(inFile))
      return(NULL);
    
    withProgress(message = 'Reading and formating input file', value = 0.2, {
      Sys.sleep(0.1);
      G <- readMPFile(inFile$datapath, numBases = flank, type = indOrFull, trDir = isStrand);
      incProgress(0.4, message = 'Estimating mutation signatures');
      Sys.sleep(0.1);
      if (isBG == TRUE) {
        BG_prob <- readBGFile(G);
        Param <- getPMSignature(G, K, BG = BG_prob, numInit = 1);
      } else {
        Param <- getPMSignature(G, K, numInit = 1);
      }
    })
    
    return(list(G, Param))
  })
  
  output$sigNumControls <- renderUI({
  
    K <- isolate(input$sigNum) - as.numeric(isolate(input$backGround));
    res <- estimationResult();
#     if (K >= 4) {
#       fluidRow(
#         # column(4, plotOutput('signature4')),
#         # column(4, plotOutput('signature5')), 
#         # column(4, plotOutput('signature6'))
#         column(4, imageOutput('signature4')),
#         column(4, imageOutput('signature5')), 
#         column(4, imageOutput('signature6'))
#       )
#     }
    fluidRow(
      if (!is.null(res) && K >= 1) column(4, imageOutput('signature1')),
      if (!is.null(res) && K >= 2) column(4, imageOutput('signature2')),  
      if (!is.null(res) && K >= 3) column(4, imageOutput('signature3')),
      if (!is.null(res) && K >= 4) column(4, imageOutput('signature4')),
      if (!is.null(res) && K >= 5) column(4, imageOutput('signature5')),
      if (!is.null(res) && K >= 6) column(4, imageOutput('signature6'))
    )
  })
  
  output$signature1 <- renderImage({
    scaleOrNot <- isolate(input$scale)
    K <- isolate(input$sigNum) - as.numeric(isolate(input$backGround));
    res <- estimationResult();

    outfile <- tempfile(fileext='.png')
    res <- estimationResult();
    Param <- res[[2]];
    
    png(outfile, width=400, height=300)
    if (!is.null(res) && K >= 1) {
      visPMSignature(Param, 1, isScale = scaleOrNot)
    }
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
    }, deleteFile = TRUE)

  output$signature2 <- renderImage({
    scaleOrNot <- isolate(input$scale)
    K <- isolate(input$sigNum) - as.numeric(isolate(input$backGround));
    res <- estimationResult();

    outfile <- tempfile(fileext='.png')
    res <- estimationResult();
    Param <- res[[2]];
    
    png(outfile, width=400, height=300)
    if (!is.null(res) && K >= 2) {
      visPMSignature(Param, 2, isScale = scaleOrNot)
    }
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)

  output$signature3 <- renderImage({
    scaleOrNot <- isolate(input$scale)
    K <- isolate(input$sigNum) - as.numeric(isolate(input$backGround));
    res <- estimationResult();

    outfile <- tempfile(fileext='.png')
    res <- estimationResult();
    Param <- res[[2]];
    
    png(outfile, width=400, height=300)
    if (!is.null(res) && K >= 3) {
      visPMSignature(Param, 3, isScale = scaleOrNot)
    }
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)

  output$signature4 <- renderImage({
    scaleOrNot <- isolate(input$scale)
    K <- isolate(input$sigNum) - as.numeric(isolate(input$backGround));
    res <- estimationResult();

    outfile <- tempfile(fileext='.png')
    res <- estimationResult();
    Param <- res[[2]];
    
    png(outfile, width=400, height=300)
    if (!is.null(res) && K >= 4) {
      visPMSignature(Param, 4, isScale = scaleOrNot)
    }
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)

  output$signature5 <- renderImage({
    scaleOrNot <- isolate(input$scale)
    K <- isolate(input$sigNum) - as.numeric(isolate(input$backGround));
    res <- estimationResult();

    outfile <- tempfile(fileext='.png')
    res <- estimationResult();
    Param <- res[[2]];
    png(outfile, width=400, height=300)
    if (!is.null(res) && K >= 5) {
      visPMSignature(Param, 5, isScale = scaleOrNot)
    }
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)

  output$signature6 <- renderImage({
    scaleOrNot <- isolate(input$scale)
    K <- isolate(input$sigNum) - as.numeric(isolate(input$backGround));
    res <- estimationResult();

    outfile <- tempfile(fileext='.png')
    res <- estimationResult();
    Param <- res[[2]];
    png(outfile, width=400, height=300)
    if (!is.null(res) && K >= 6) {
      visPMSignature(Param, 6, isScale = scaleOrNot)
    }
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
#   output$signature1 <- renderPlot({
#     scaleOrNot <- isolate(input$scale)
#     K <- isolate(input$sigNum) - as.numeric(isolate(input$backGround));
#     res <- estimationResult();
#     if (is.null(res) || K < 1) {
#       return();
#     } 
#     
#     res <- estimationResult();
#     Param <- res[[2]];
#     visPMSignature(Param, 1, isScale = scaleOrNot)
#   })
#   
#   output$signature2 <- renderPlot({
#     scaleOrNot <- isolate(input$scale)
#     K <- isolate(input$sigNum) - as.numeric(isolate(input$backGround));
#     res <- estimationResult();
#     if (is.null(res) || K < 2) {
#       return();
#     } 
#     
#     res <- estimationResult();
#     Param <- res[[2]];
#     visPMSignature(Param, 2, isScale = scaleOrNot);
#   })
#   
#   output$signature3 <- renderPlot({
#     scaleOrNot <- isolate(input$scale);
#     K <- isolate(input$sigNum) - as.numeric(isolate(input$backGround));
#     res <- estimationResult();
#     if (is.null(res)|| K < 3) {
#       return();
#     } 
#     
#     res <- estimationResult();
#     Param <- res[[2]];
#     visPMSignature(Param, 3, isScale = scaleOrNot);
#   })
#   
#   output$signature4 <- renderPlot({
#     scaleOrNot <- isolate(input$scale);
#     K <- isolate(input$sigNum) - as.numeric(isolate(input$backGround));
#     res <- estimationResult();
#     if (is.null(res) || K < 4) {
#       return();
#     } 
#     
#     res <- estimationResult();
#     Param <- res[[2]];
#     visPMSignature(Param, 4, isScale = scaleOrNot);
#   })
#   
#   output$signature5 <- renderPlot({
#     scaleOrNot <- isolate(input$scale);
#     K <- isolate(input$sigNum) - as.numeric(isolate(input$backGround));
#     res <- estimationResult();
#     if (is.null(res) || K < 5) {
#       return();
#     } 
#     
#     res <- estimationResult();
#     Param <- res[[2]];
#     visPMSignature(Param, 5, isScale = scaleOrNot);
#   })
#   
#   output$signature6 <- renderPlot({
#     scaleOrNot <- isolate(input$scale);
#     K <- isolate(input$sigNum) - as.numeric(isolate(input$backGround));
#     res <- estimationResult();
#     if (is.null(res) || K < 6) {
#       return();
#     } 
#     
#     res <- estimationResult();
#     Param <- res[[2]];
#     visPMSignature(Param, 6, isScale = scaleOrNot);
#   })
  
  output$membership <- renderPlot({
    
    res <- estimationResult();
    if (is.null(res)) {
      return();
    } 
    
    res <- estimationResult();
    G <- res[[1]];
    Param <- res[[2]];
    visMembership(G, Param, colourBrewer = "Set2");
    
  })
  
  outputOptions(output, "sigNumControls", priority = 10);
  outputOptions(output, "signature1", priority = 5);
  outputOptions(output, "signature2", priority = 5);
  outputOptions(output, "signature3", priority = 5);
  outputOptions(output, "membership", priority = 5);
  
  
})