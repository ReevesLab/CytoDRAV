# serverFunctions.R holds the data loading and plotting functions.
source("./serverFunctions.R")
future::plan(future::multiprocess)
options(scipen = 999999)

#Default max file size is only 5MB. This ups that limit to 30MB
options(shiny.maxRequestSize=100*1024^2)

# Needed for display of bh-SNE progress
tmpfile <- tempfile()
tmpdir <- tempdir()

# Importing just this operator from promises
`%...>%` <- promises::`%...>%`

`%>%` <- dplyr::`%>%`

# Main server function for CytoDRAV
function(input, output, session) {

  # ReactiveValues object to hold the input FCS data and other aspects for analysis and plotting
  input_fcs_data <- shiny::reactiveValues(orig_data=NULL, markers=NULL, plot_style=NULL, sel_data=NULL)

  # Reactive function to load data. When called it resets the ReactiveValues object above
  load_data <- reactive({
    input_fcs_data$orig_data <- NULL
    input_fcs_data$sel_data <- NULL
    fcsFileList <- input$file1
    rownames(fcsFileList) <- NULL
    exprsData <- loadFCS(fcsFileList, doTransform = FALSE)# input$transform)
    input_fcs_data$markers <- colnames(exprsData)
    return(exprsData)
  })

  # Reactive function to load a previosly saved Rdata file
  prev_data <- reactive({
    withProgress(message="Loading previous data", {
    d <- readRDS(input$filep$datapath)
    input_fcs_data$markers <- colnames(d)
    return(d)
    })
  })

  # Load FCS button. Resets ReactiveValues fields and switches user to Parameters tab automatically
  observeEvent(input$load_fcs_files, {
    input_fcs_data$plot_style <- NULL
    input_fcs_data$markers <- NULL
    input_fcs_data$orig_data <- load_data()
    input_fcs_data$sel_data <- load_data()
    updateTabsetPanel(session, "apptabs",
                      selected = "paramtab")
  })

  # Load previous session button. Loads the previous Rdata file for plotting
  observeEvent(input$load_previous_file, {
    input_fcs_data$plot_style <- NULL
    input_fcs_data$orig_data <- prev_data()
    input_fcs_data$sel_data <- prev_data()
  })

  # Populates the checkbox field for markers that will be included in the bh-SNE analysis
  # Stays empty until data is loaded
  output$maker_select_tsne <- renderUI({
    if (is.null(input_fcs_data$orig_data) && is.null(input_fcs_data$sel_data)) return ()
    marks <- isolate(input_fcs_data$markers)
    checkboxGroupInput("choices", "Markers for Analysis",
                       choices = marks[!marks%in%c("Sample", "tSNEX", "tSNEY", "Cluster", "SampleID")],
                       selected = marks[!marks%in%c("Sample", "tSNEX", "tSNEY", "Cluster", "SampleID")])
    })



  # Slider to select number of events to use in analysis
  output$ncell_slider <- renderUI({
    if (is.null(input_fcs_data$orig_data) && is.null(input_fcs_data$sel_data)) return ()
    sliderInput("downsample_to_ncells", "Number of events to use for Analysis",
                min=1,
                max=nrow(input_fcs_data$orig_data),
                value=nrow(input_fcs_data$orig_data))
  })

  # Generate check box group under the Export tab for markers that will be generated and downloaded in bulk
  output$exportmarkers <- renderUI({
    checkboxGroupInput("plot_export_markers", "Markers",
                       choices=c(input_fcs_data$markers[!input_fcs_data$markers%in%c("tSNEX", "tSNEY")], "Density", "Cluster"),
                       selected=c(input_fcs_data$markers[!input_fcs_data$markers%in%c("tSNEX", "tSNEY")], "Density", "Cluster"))
  })

  # Run bh-SNE button. Isolates user settings when button is pressed and performs bh-SNE.
  # Downsamples data based on downsample_to_ncells slider
  observeEvent(input$run_tsne, {

    input_fcs_data$plot_style <- NULL
    input_fcs_data$tSNEX <- NULL
    input_fcs_data$tSNEY <- NULL

    if ( (is.null(input_fcs_data$orig_data) ) || ( nrow(input_fcs_data$sel_data) != input$downsample_to_ncells) ) {
      input_fcs_data$ncells <- isolate(input$downsample_to_ncells)
      input_fcs_data$sel_data <- input_fcs_data$orig_data[sample(nrow(input_fcs_data$orig_data), input_fcs_data$ncells),]
      input_fcs_data$sel_data <- input_fcs_data$sel_data[!duplicated(input_fcs_data$sel_data[c(1:ncol(input_fcs_data$sel_data)-1)]),]
    }

    # Isolating user selected parameters into new variables
    chosen_markers <- isolate(input$choices)
    tmp <- data.frame(asinh(input_fcs_data$sel_data[, chosen_markers]/isolate(input$asinh_cofactor)))
    if (input$transform) {
      tmp <- data.frame(apply(tmp, 2, scale_vec))
    }
    perp <- isolate(input$perp)
    eta <- isolate(input$eta)
    theta <- isolate(input$theta)
    dims <- isolate(input$ndims)
    pca <- isolate(input$initpca)
    iter <- isolate(input$iter)
    n_threads <- as.double(isolate(input$n_threads))
    # I don't know why I need to sink.reset() twice but this makes the logging work.
    #withProgress({
      sink(tmpfile, type=c("output", "message"), append=FALSE)
      future::future({
        sink(tmpfile, type=c("output", "message"), append=FALSE)
        Rtsne::Rtsne(tmp, verbose=TRUE,
                     perplexity=perp,
                     theta=theta,
                     eta=eta,
                     dims=dims,
                     pca=pca,
                     max_iter=iter,
                     check_duplicates = FALSE,
                     num_threads=n_threads)
      #  tsne <- uwot::umap(tmp,
      #             metric = "manhattan",
      #             verbose = TRUE,
      #             n_threads = n_threads)
      }) %...>%
        (function(tsne) {
          # Add tSNE results to dataframe
          input_fcs_data$sel_data$tSNEX <- tsne$Y[,1]
          input_fcs_data$sel_data$tSNEY <- tsne$Y[,2]
          #input_fcs_data$orig_data$tSNEX <- tsne[,1]
          #input_fcs_data$orig_data$tSNEY <- tsne[,2]

          # Adds comments to dataframe with the parameters used for analysis.
          # This information is printed to the About tab
          comment(input_fcs_data$sel_data$tSNEX) <- paste0("tSNE Settings", "<ul>",
                                                           "<li>Perplexity: ", perp,
                                                           "<li>Output Dims: ", dims,
                                                           "<li>Theta: ", theta,
                                                           "<li>Learning Rate: ", eta,
                                                           "<li>PCA performed: ", pca,
                                                           "<li>Iterations: ", iter, "</ul>")
          comment(input_fcs_data$sel_data$tSNEY) <- paste0("<br/>Selected Markers:", "<ul><li>",
                                                           paste(chosen_markers, collapse = "<li>"), "</ul>")
          comment(input_fcs_data$sel_data$Sample) <- paste0("Data Transformed: ",
                                                            input$transform)
        })
      sink.reset()

    #}, message = "Running analysis ...")

  })

  observeEvent(input$run_rpheno, {

    if ( (is.null(input_fcs_data$orig_data) ) || ( nrow(input_fcs_data$sel_data) != input$downsample_to_ncells) ) {
      input_fcs_data$ncells <- isolate(input$downsample_to_ncells)
      input_fcs_data$sel_data <- input_fcs_data$orig_data[sample(nrow(input_fcs_data$orig_data), input_fcs_data$ncells),]
      input_fcs_data$sel_data <- input_fcs_data$sel_data[!duplicated(input_fcs_data$sel_data[c(1:ncol(input_fcs_data$sel_data)-1)]),]
    }

    # Isolating user selected parameters into new variables
    chosen_markers <- isolate(input$choices)
    tmp <- data.frame(asinh(input_fcs_data$sel_data[, chosen_markers]/isolate(input$asinh_cofactor)))
    if (input$transform) {
      tmp <- data.frame(apply(tmp, 2, scale_vec))
    }
    perp <- isolate(input$perp)
    eta <- isolate(input$eta)
    theta <- isolate(input$theta)
    dims <- isolate(input$ndims)
    pca <- isolate(input$initpca)
    iter <- isolate(input$iter)
    n_neighbors <- isolate(input$n_neighbors)
    n_threads <- as.double(isolate(input$n_threads))
    # I don't know why I need to sink.reset() twice but this makes the logging work.
    sink(tmpfile, type=c("output", "message"), append=FALSE)
    future::future({
      sink(tmpfile, type=c("output", "message"), append=FALSE)
      Rphenograph::Rphenograph(tmp,
                               k = n_neighbors)
    }) %...>%
      (function(rp) {
        # Add tSNE results to dataframe
        input_fcs_data$sel_data$Cluster <- factor(igraph::membership(rp[[2]]))


      })
    sink.reset()
    gc()
  })
  # Printing analysis information that is saved to the data. Useful for keeping track of analysis parameters
  # after saving
  output$analysis_params <- renderUI({
    if (is.null(comment(input_fcs_data$orig_data$Sample))) return ()
    HTML(
      paste("<h4>Analysis information:</h4><br/>",
        comment(input_fcs_data$sel_data$Sample),
        comment(input_fcs_data$sel_data$tSNEY),
        comment(input_fcs_data$sel_data$tSNEX),
        collapse = "<br/>"
      )
    )
  })

  # Color picker to manually select colors for your samples
  sample_colors <- reactive({
    lapply(unique(input_fcs_data$orig_data$Sample), function(i) {
      #div(style="display: inline-block;vertical-align:top; width: 200px;",
          colourpicker::colourInput(i, i, "black", palette = "limited")#)
    })
  })

  # Populate the color selectors for samples
  output$color_selector <- renderUI({sample_colors()})
  outputOptions(output, "color_selector", suspendWhenHidden = FALSE)

  # Reactive list that maps sample to its respective chosen color
  select_sample_colors <- reactive({
    lapply(unique(input_fcs_data$orig_data$Sample), function(i) {
      input[[paste(i)]]
    })
  })

  # Plot bh-SNE button. I think I can get rid of the first if() call
  observeEvent(input$plot_tsne, {

    updateTabsetPanel(session, "apptabs",
                      selected = "maintab")
    input_fcs_data$plot_style <- "tSNE"

  })

  output$cluster_info <- renderPrint({
    if (input_fcs_data$sel_data$Cluster != "NULL") {
      q <- xtabs(~input_fcs_data$sel_data$Cluste+input_fcs_data$sel_data$Sample, data = input_fcs_data$sel_data)
      names(dimnames(q)) <- c("Cluster", "Sample")
      q <- round((q / rowSums(q)), digits = 2)
      paste("Cluster proportions by input data file")
      #qcl()
      q
    } else {
      return ()
    }

  })
  # Populate the histogram selectors
  output$hist_selector <- renderUI({
    fluidRow(
      #column(2, selectInput("base_hist", "Base", choices = c("All", sort(unique(input_fcs_data$sel_data$Cluster))))),
      column(2, shinyWidgets::pickerInput(inputId = "hist_groups", label = "Clusters to Plot",
                                          choices = c(sort(unique(input_fcs_data$sel_data$Cluster))),
                                          multiple = TRUE,
                                          selected = "All")),
      #column(2, selectInput("comp_hist", "Compare", choices = sort(unique(input_fcs_data$sel_data$Cluster)))),
      #column(2, selectInput("hist_marker", "Marker", choices = input_fcs_data$markers)),
      column(2, fluidRow(downloadButton("save_hist", "Save Histograms"), actionButton("plot_hist", "Plot Histogram")))
    )
  })

  # Plot Histogram button.
  observeEvent(input$plot_hist, {
    gen_hist()
    input_fcs_data$hist_style <- "HIST"
  })

  gen_hist <- reactive({
    data <- input_fcs_data$sel_data
    comp <- data[data$Cluster%in%input$hist_groups,]
    data <- data[!data$Cluster%in%input$hist_groups,]
    comp$Cluster <- paste0("Cluster: ", comp$Cluster)

    plot_markers <- input_fcs_data$markers[!input_fcs_data$markers%in%c("tSNEX", "tSNEY", "Sample", "Cluster",
                                                                        "Density", "Live_Dead", "SampleID",
                                                                        "L_D", "AQUA AMINE", "OC")]
    labs <- names(asinh_trans_prettybreaks(input$asinh_cofactor)$breaks())
    labs <- gsub("\\<1000\\>", bquote('10^3'), labs)
    labs <- gsub("\\<10000\\>", bquote('10^4'), labs)
    labs <- gsub("\\<100000\\>", bquote('10^5'), labs)
    labs <- gsub("\\<1000000\\>", bquote('10^6'), labs)
    #mycolors = colorRampPalette(RColorBrewer::brewer.pal(name="Set3", n = 9))(length(levels(data$Cluster)))
    #names(mycolors) <- sort(unique(data$Cluster))
    plot_data_column <- function(column) {
      ggplot2::ggplot() +
        ggplot2::geom_density(data = data, ggplot2::aes(x = data[, column], fill = "All Clusters"), alpha = 0.3) +
        ggplot2::geom_density(data = comp, ggplot2::aes(x = comp[, column], color = comp[, "Cluster"])) +
        ggplot2::scale_fill_manual(name = "", values = "grey") +
        #ggplot2::scale_color_manual(name = "Cluster", values = mycolors) +
        ggplot2::labs(color="Cluster", x = column) +
        ggplot2::scale_x_continuous(trans = asinh_trans_prettybreaks(input$asinh_cofactor), limits = c(-10**4, 262144),
                                    labels = c(parse(text = labs), rep("", abs(length(parse(text = labs))-length(labs))))) +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::guides(fill = FALSE)

    }
    myplots <- lapply(plot_markers, plot_data_column)
    input_fcs_data$plots <- myplots
  })

  # Checks the tmpfile every 50 ms for changes
  log <- reactiveFileReader(50, session=session, tmpfile, read.delim, sep="\n")

  # Prints tSNE status to text box under the file selection section
  output$consoletext <- renderPrint({
    try(print(log()), silent=TRUE)

    # Scrolls the window to last line as it prints
    session$sendCustomMessage(type = "scrollCallback", 1)
  })

  # Save plot button
  # Saves the currently displayed plot to PDF file
  output$export = downloadHandler(
    filename = function() {paste0(input$marker_overlay, ".pdf", input$format)},
    content = function(file) {
      ggplot2::ggsave(file, plot=input_fcs_data$plot, device = "pdf", dpi=300, width=11, height=8.5, units="in")

    }
  )

  # Save histogram button
  # Saves the currently displayed histogram to PDF file
  output$save_hist = downloadHandler(
    filename = function() {paste0("Cluster ", input$base_hist, " and Cluster ", input$comp_hist, ".pdf")},
    content = function(file) {
      ggplot2::ggsave(file, plot=input_fcs_data$hist_plotted, device = "pdf", dpi=300, height=22, width=17, unit="in")

    }
  )

  # Save data button.
  # Saves the user dataframe to local file. Can be uploaded later to visualize or rerun tSNE analysis
  output$saveRDF = downloadHandler(
    filename = function() {paste("DataFrame", ".Rda", sep="")},
    content = function(file) {
      saveRDS(input_fcs_data$sel_data, file)
    }
  )

  # Dropdown box for each parameter that can be used to color points on plot
  output$overlay <- renderUI({
    if (is.null(input_fcs_data$markers)) return ()
    selectInput(inputId="marker_overlay", "Color",
                choices=c(input_fcs_data$markers[!input_fcs_data$markers%in%c("tSNEX", "tSNEY")], "Density", "Cluster"),
                selected="Sample")
  })
  outputOptions(output, "overlay", suspendWhenHidden = FALSE)

  # Creates the plots the tSNE results
  output$plot <- renderPlot({
    if (is.null(input_fcs_data$plot_style)) return ()
    if(input_fcs_data$plot_style == "tSNE") {
      sampleColor <- setNames(unlist(select_sample_colors()), unique(input_fcs_data$orig_data$Sample))
      input_fcs_data$plot <- create_plot(input_fcs_data$sel_data, input$marker_overlay, input$size,
                                         input$alpha, sampleColor, show_legend=input$show_legend, input$asinh_cofactor,
                                         show_axis_labels=input$show_axis_labels, show_title=input$show_title, show_cluster = input$show_cluster)
      input_fcs_data$plot
    }
  })

  output$hist_plot <- renderPlot({
    if (is.null(input_fcs_data$hist_style)) return ()

    if(input_fcs_data$hist_style == "HIST") {
      input_fcs_data$hist_plotted <- gridExtra::grid.arrange(grobs = input_fcs_data$plots , ncol = 3)
      input_fcs_data$hist_plotted
    }


  })

  # Generates plots for each marker selected under the Export tab in the chosen file format
  observeEvent(input$generate_plots, {
    file.remove(list.files(tmpdir, full.names = T))

    if (is.null(input_fcs_data$sel_data$tSNEX) || is.null(input_fcs_data$sel_data$tSNEY)) {
        input_fcs_data$sel_data$tSNEX <- as.vector(input_fcs_data$tSNEX)
        input_fcs_data$sel_data$tSNEY <- as.vector(input_fcs_data$tSNEY)
    }

      withProgress(message="Generating bh-SNE plots...", value=0, min=0, max=length(input$plot_export_markers), {

        for (mark in input$plot_export_markers) {

          sampleColor <- setNames(unlist(select_sample_colors()), unique(input_fcs_data$orig_data$Sample))
          tmp_plot <- create_plot(dataToPlot=input_fcs_data$sel_data,
                                  input$asinh_cofactor,
                                  marker=mark,
                                  dotsize=input$size,
                                  dotalpha=input$alpha,
                                  sampleColor=sampleColor,
                                  show_legend=input$show_legend,
                                  show_axis_labels=input$show_axis_labels,
                                  show_title=input$show_title,
                                  show_cluster = input$show_cluster)
          name <- paste0(tmpdir, "/", mark, "-bhSNE.", input$exportFormat)
          ggplot2::ggsave(filename=name, plot=tmp_plot, device=input$exportFormat, height=8.5, width=11, units="in")
          incProgress(1)

        }
      })
    })

  # Zips all the generated plots together for download
  output$download_plots = downloadHandler(
    filename = function() {paste(input$plotexport, "Plots.zip", sep=" ")},
    content = function(file) {

      zip(file, files=list.files(path=tmpdir, pattern=paste0("*.", input$exportFormat),
                                 recursive = TRUE, full.names = TRUE), flags="-j")
    }
  )

  qcl <- reactive({
    q <- xtabs(~input_fcs_data$sel_data$Cluste+input_fcs_data$sel_data$Sample, data = input_fcs_data$sel_data)
    names(dimnames(q)) <- c("Cluster", "Sample")
    q <- round((q / rowSums(q)), digits = 2)
    p <- as.data.frame.matrix(q)
    p$Cluster <- row.names(p)
    cols <- colnames(p)
    cols <- cols[1: length(cols)-1]
    p <- p[, c("Cluster", cols)]
    return(p)
  })
  output$download_cluster = downloadHandler(
    filename = function() {paste("Clusters.txt")},
    content = function(file) {


      write.table(qcl(), file, sep = "\t", row.names = FALSE)
    }
  )

  # Stops R session when window closes
  session$onSessionEnded(function() {
    stopApp()
  })
}

