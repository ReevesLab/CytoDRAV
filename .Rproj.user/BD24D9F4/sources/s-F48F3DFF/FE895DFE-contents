library(shiny)
source("./serverFunctions.R")
future::plan(future::multiprocess)


#Default max file size is only 5MB. This ups that limit to 30MB
options(shiny.maxRequestSize=100*1024^2)

# Needed for display of bh-SNE progress
tmpfile <- tempfile()
tmpdir <- tempdir()
`%...>%` <- promises::`%...>%`
`%>%` <- magrittr::`%>%`
# Main server function
function(input, output, session) {

  userDF <- reactiveValues(orig_data=NULL, markers=NULL, plotstyle=NULL, sel_data=NULL)

  load_data <- reactive({
    userDF$orig_data <- NULL
    userDF$sel_data <- NULL
    fcsFileList <- input$file1
    fcsFileList <- rbind(fcsFileList[!stringr::str_detect(fcsFileList[,"name"], "Live"),],
                         fcsFileList[stringr::str_detect(fcsFileList[,"name"], "Live"),])
    rownames(fcsFileList) <- NULL
    exprsData <- loadFCS(fcsFileList, input$transform)
    userDF$markers <- colnames(exprsData)
    return(exprsData)
  })

  prev_data <- reactive({
    d <- readRDS(input$filep$datapath)
    userDF$markers <- colnames(d)
    return(d)
  })

  observeEvent(input$loadfcs, {
    userDF$markers <- NULL
    userDF$orig_data <- load_data()
  })

  observeEvent(input$runprev, {

    userDF$sel_data <- prev_data()
    userDF$orig_data <- prev_data()
  })


  output$markerSelect <- renderUI({
    if (is.null(userDF$orig_data) && is.null(userDF$sel_data)) return ()
    checkboxGroupInput("choices", "Markers for Analysis", colnames(userDF$orig_data)[!colnames(userDF$orig_data)%in%c("Sample", "Infection","Duped")],
                       selected=NULL)

  })
  output$numCellSelect <- renderUI({
    if (is.null(userDF$orig_data) && is.null(userDF$sel_data)) return ()
    sliderInput("numcells", "Number of events to use for Analysis", min=1, max=nrow(userDF$orig_data), value=nrow(userDF$orig_data))
  })

  output$exportmarkers <- renderUI({
    checkboxGroupInput("echoices", "Markers", userDF$markers, selected=userDF$markers)
  })


  observeEvent(input$runtsne, {

    userDF$plotstyle <- NULL
    userDF$tSNEX <- NULL
    userDF$tSNEY <- NULL
    if (is.null(userDF$sel_data) || nrow(userDF$sel_data) != input$numcells) {
      userDF$ncells <- isolate(input$numcells)
      userDF$sel_data <- userDF$orig_data[sample(nrow(userDF$orig_data), userDF$ncells),]
      userDF$sel_data <- userDF$sel_data[!duplicated(userDF$sel_data[c(1:ncol(userDF$sel_data)-1)]),]
    }
    chosen_markers <- input$choices
    tmp <- userDF$sel_data[, chosen_markers]
    perp <- input$perp
    eta=input$eta
    theta=input$theta
    dims=input$ndims
    pca=input$initpca
    sink(tmpfile, type=c("output", "message"), append=FALSE)

    future::future({
      sink(tmpfile, type=c("output", "message"), append=FALSE)
      Rtsne::Rtsne(tmp, verbose=TRUE, perplexity=perp,
            theta=theta, eta=eta,
            dims=dims, pca=pca, check_duplicates = FALSE)
    }) %...>%
      (function(tsne) {
        userDF$tSNEX <- tsne$Y[,1]
        userDF$tSNEY <- tsne$Y[,2]
      })
    sink()

  })

  # Color picker to manually select colors for your samples
  cols <- reactive({
    lapply(unique(userDF$orig_data$Sample), function(i) {
      div(style="display: inline-block;vertical-align:top; width: 200px;",
          colourpicker::colourInput(i, i, "black", palette = "limited"))
    })
  })

  output$colsel <- renderUI({cols()})
  selcolors <- reactive({
    lapply(unique(userDF$orig_data$Sample), function(i) {
      input[[paste(i)]]
    })
  })

  observeEvent(input$plottsne, {
    if (is.null(userDF$sel_data$tSNEX) || is.null(userDF$sel_data$tSNEY)) {
      userDF$sel_data$tSNEX <- as.vector(userDF$tSNEX)
      userDF$sel_data$tSNEY <- as.vector(userDF$tSNEY)
    }
    userDF$plotstyle <- "tSNE"
    comment(userDF$sel_data$tSNEX) <- paste0("tSNE Settings", "\nPerplexity: ", input$perp,
                                             "\nOutput Dims: ", input$ndims, "\nTheta: ", input$theta,
                                             "\nLearning Rate: ", input$eta)
    comment(userDF$sel_data$tSNEY) <- paste0("Selected Markers\n", paste(input$choices, collapse = "\n"))
    comment(userDF$sel_data$Sample) <- paste0("Data Transformed\n", input$transform)
  })

  log <- reactiveFileReader(50, session=session, tmpfile, read.delim, sep="\n")
  output$consoletext <- renderPrint({
    validate(
      need(try(
        suppressWarnings(print(log())), silent=TRUE),
        "")
    )
    session$sendCustomMessage(type = "scrollCallback", 1)
  })

  output$export = downloadHandler(
    filename = function() {paste(input$moverlay, ".pdf", input$format, sep="")},
    content = function(file) {
      ggplot2::ggsave(file, plot=userDF$plot, device = "pdf", dpi=300, width=11, height=8.5, units="in")

    }
  )

  output$saveRDF = downloadHandler(
    filename = function() {paste("DataFrame", ".Rda", sep="")},
    content = function(file) {

      saveRDS(userDF$sel_data, file)
    }
  )

  output$overlay <- renderUI({
    if (is.null(userDF$markers)) return ()
    selectInput(inputId="moverlay", "Color", choices=userDF$markers, selected="Sample")
  })


  output$plot <- renderPlot({
    if (is.null(userDF$plotstyle)) return ()
    if(userDF$plotstyle == "tSNE") {
      sampleColor <- setNames(unlist(selcolors()), unique(userDF$orig_data$Sample))
      userDF$plot <- plotTSNE(userDF$sel_data, input$moverlay, input$size, input$alpha, sampleColor, input$showDensity)
      userDF$plot
    }
  })

  observeEvent(input$genPlots, {

    if (is.null(userDF$sel_data$tSNEX) || is.null(userDF$sel_data$tSNEY)) {
        userDF$sel_data$tSNEX <- as.vector(userDF$tSNEX)
        userDF$sel_data$tSNEY <- as.vector(userDF$tSNEY)
      }
      withProgress(message="Generating tSNE plots...", value=0, min=0, max=length(input$echoices), {
        for (mark in input$echoices) {
          sampleColor <- setNames(unlist(selcolors()), unique(userDF$orig_data$Sample))
          tmp_plot <- plotTSNE(userDF$sel_data, mark, input$size, input$alpha, sampleColor, input$showDensity)
          name <- paste0(tmpdir, "/", mark, "-bhSNE.", input$exportFormat)
          ggplot2::ggsave(filename=name, plot=tmp_plot, device=input$exportFormat, height=8.5, width=11, units="in")
          incProgress(1)
        }
      })
    })

  output$dlall = downloadHandler(
    filename = function() {paste(input$plotexport, "Plots.zip", sep=" ")},
    content = function(file) {

      zip(file, files=list.files(path=tmpdir, pattern=paste0("*.", input$exportFormat), recursive = TRUE, full.names = TRUE), flags="-j")
      ff <- list.files(path=tmpdir, pattern=paste0("*.", input$exportFormat), recursive = TRUE, full.names = TRUE)
      unlink(ff, recursive = TRUE, force=FALSE)
    }
  )
  session$onSessionEnded(function() {
    stopApp()
  })
}
