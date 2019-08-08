# About ----
# Application: CytoDRAV
# Version: 2.0
# Changelog: Complete overhaul of CytoDRAV. 
# Read more at https://github.com/ReevesLab/CytoDRAV/Reboot

# Global declarations ----
options(shiny.maxRequestSize=10^4*1024^2) 
future::plan(future::multiprocess)
source("serverFunctions.R")
tmpdir <- tempdir()
tmpfile <- tempfile()
fjtmp <- paste0(tmpdir, "/fjwsp")
file.remove(list.files(fjtmp, full.names = T))
`%...>%` <- promises::`%...>%`

# Server Block ----
function(input, output, session) {
    # Reactive dataframe ----
    user_data <- reactiveValues(raw_data=NULL,
                               data=NULL,
                               filelist=c("NULL"),
                               analysis_complete="NO",
                               plot=NULL,
                               markers=NULL
                               )
    # Reactive container for tracking nodes per fcs file in workspace
    gated_tracker <- reactive({
        gated_tracker <- c()
        for (i in 1:length(user_data$filelist)){
            gated_tracker[[i]] <- paste0(
                "input[[ user_data$filelist[[", i, "]] ]]"
            )
        }
        return(gated_tracker)
    })

    # ACS loading ----
    load_data <- reactive({
        withProgress(message = "Extracting ACS and Loading WSP", {
            unzip(input$wsp_upload[,"datapath"], exdir = fjtmp)
            incProgress(1/3)
            wsp_name <- list.files(path = fjtmp, pattern = ".wsp")
            wsfile <- flowWorkspace::openWorkspace(paste0(fjtmp, "/", wsp_name[1]))
            incProgress(1/3)
            workspace <- flowWorkspace::parseWorkspace(wsfile, name = 1)
            incProgress(1/3)
            })
        user_data$raw_data <- workspace
        for (i in 1:length(user_data$raw_data)){
            user_data$filelist[[i]] <- user_data$raw_data[[i]]@name
        }
    })
    observeEvent(input$load_acs, {
        user_data$raw_data <- NULL
        user_data$data <- NULL
        user_data$filelist <- c("NULL")
        user_data$analysis_complete <- "NO"
        user_data$plot <- NULL
        user_data$markers <- NULL
        load_data()
        updateTabsetPanel(session, "main", "Settings")
        updateTabsetPanel(session, "settings_tabs", "gating")
    })
    
    # Gating Plots ----
    output$gh_plot <- renderPlot({
        if (is.null(user_data$raw_data)) return ()
        par(mfrow=c(2,1))
        for (i in 1:length(user_data$raw_data)) {
            flowWorkspace::plot(user_data$raw_data[[i]])
            title(user_data$raw_data[[i]]@name)     
        }
    })
    # Load Selected Nodes ----
    output$gated_pop_selection <- renderUI({
        if (is.null(user_data$raw_data)) return ()
        for (i in 1:length(user_data$raw_data)){
            insertUI(
                selector = "#label",
                where = "beforeEnd",
                ui = checkboxGroupInput(inputId = user_data$raw_data[[i]]@name, 
                                        label = user_data$raw_data[[i]]@name, 
                                        choices = unlist(
                                            flowWorkspace::getNodes(
                                                user_data$raw_data[[i]], 
                                                path=1)
                                        )
                )
            )
        }
    })

    observeEvent({
        lapply(gated_tracker(), function(text) {eval(parse(text=text))})
    }, {
        user_data$data <- NULL
        df <- data.frame()
        for (i in 1:length(user_data$filelist)) {
            for (choice in input[[user_data$filelist[[i]]]]){
                t1 <- flowWorkspace::getData(
                    user_data$raw_data[[user_data$filelist[[i]]]], 
                    choice
                )
                m = 4.5
                t = 262144
                w = 1.5
                lgcl <- flowCore::logicleTransform( w = w, t= t, m = m)
                t1_transformed <- 
                    flowCore::transform(t1,
                                        flowCore::transformList(paste(
                                            t1@parameters@data$name[!t1@parameters@data$name%in%
                                                                        c("FSC-A", "FSC-H", "FSC-W", "SSC-A", 
                                                                          "SSC-H", "SSC-W", "Time", "Event #", "Sample")]), 
                                            lgcl))
                t <- data.frame(t1_transformed@exprs)
                column_names <- c()
                for (j in 1:ncol(t)) {
                    if (is.na(t1@parameters@data$desc[[j]])) {
                        column_names[[j]] <- t1@parameters@data$name[[j]]
                    } else {
                        column_names[[j]] <- t1@parameters@data$desc[[j]]
                    }
                }
                colnames(t) <- column_names
                #t <- data.frame(lapply(t[, colnames(t)[!colnames(t)%in%c("Sample", "Time", "Event #")]], z_transform))
                x <- gsub(" ", "_", user_data$filelist[[i]])
                n <- strsplit(x, "\\.")[[1]][1]
                n <- gsub(" ", "_", n)
                t$Population <- paste0(gsub(" ", "_", choice))
                t$Filename <- paste0(x)
                t$UniqueID <- paste0(n, "_", gsub(" ", "_", choice))
                df <- rbind(df, t)
            }
        }
        df2 <- data.frame(lapply(df[, colnames(df)[!colnames(df)%in%c("Sample", "Time", "Event #", "Population", "Filename", "UniqueID")]], z_transform))
        df2$Filename <- df$filename
        df2$UniqueID <- df$UniqueID
        df2$Population <- df$Population
        user_data$data <- df2[sample(1:nrow(df2)),]
        user_data$markers<- colnames(df2)[!colnames(df2)%in%c("Sample", "L_D", "Time", "File", "Event #", "Population", 
                                                              "Filename", "UniqueID", "UMAP1", "UMAP2", "Cluster")]
    })
    
    output$num_events <- renderPrint({
        if (is.null(user_data$data)) return ()
        cat(paste("Number of events in selected populations:", nrow(user_data$data)))
    })
    # Parameter Selection ----
    output$select_algorithm <- renderUI({
        if (is.null(user_data$markers)) return ()
        checkboxGroupInput(inputId = "tsne_params_select", 
                           label = "Parameters for t-SNE", 
                           choices = user_data$markers,
                           selected = NULL)
    })
    outputOptions(output, "select_algorithm", suspendWhenHidden = FALSE)
    
    output$algorithm_parameters <- renderUI({
        
        if (input$chosen_algorithm == "UMAP") {
            fluidRow(
                column(2,
                       h4("UMAP Parameters"),
                       numericInput("n_neighbors", 
                                    "Number of Neighbors", 
                                    min=1, 
                                    value = 15),
                       numericInput("min_dist", 
                                    "Minimum Distance",
                                    min = 0.01,
                                    value = 0.2),
                       selectInput("dist_alg", 
                                   "Distance Metric",
                                   choices = c("euclidean",
                                               "manhattan",
                                               "chebyshev",
                                               "canberra",
                                               "braycurtis",
                                               "seuclidean",
                                               "cosine",
                                               "correlation"),
                                   selected = "manhattan")
                )
            )
        } else if (input$chosen_algorithm == "bh-SNE") {
            fluidRow(
                column(2,
                       h4("bh-SNE Parameters"),
                       checkboxInput("initpca", "Initial PCA Step", TRUE),
                       numericInput("perp", "Perplexity", min=0, value=20),
                       numericInput("ndims", "Output dimensions", min=2, value=2),
                       numericInput("theta", "Theta", min=0.5, value=0.5),
                       numericInput("eta", "Learning rate", min=50, value=200),
                       numericInput("iter", "Num of Iterations", min = 500, value = 500),
                       selectInput("n_threads", "Number of Threads",
                                   choices = seq(1:(unname(future::availableCores())-1)),
                                   selected = 1)
                )
            )
        }
    })
    # Run analysis ----
    observeEvent(input$run_analysis, {
        if (input$chosen_algorithm == "bh-SNE") {
            user_data$data$tSNEX <- NULL
            user_data$data$tSNEY <- NULL
            
            # Isolating user selected parameters into new variables
            chosen_markers <- isolate(input$tsne_params_select)
            tmp <- data.frame(user_data$data[, chosen_markers])
            perp <- isolate(input$perp)
            eta <- isolate(input$eta)
            theta <- isolate(input$theta)
            dims <- isolate(input$ndims)
            pca <- isolate(input$initpca)
            iter <- isolate(input$iter)
            n_threads <- as.double(isolate(input$n_threads))
            # I don't know why I need to sink() twice but this makes the logging work.
            sink(tmpfile, type=c("output", "message"), append=FALSE)
            future::future({
                sink(tmpfile, type=c("output", "message"), append=FALSE)
                Rtsne::Rtsne(as.matrix(tmp), verbose=TRUE,
                             perplexity=perp,
                             theta=theta,
                             eta=eta,
                             dims=dims,
                             pca=pca,
                             max_iter=iter,
                             check_duplicates = FALSE,
                             num_threads=n_threads)
            }) %...>%
                (function(tsne) {
                    # Add tSNE results to dataframe
                    user_data$data$tSNEX <- tsne$Y[,1]
                    user_data$data$tSNEY <- tsne$Y[,2]
                    #user_data$analysis_complete <- TRUE
                    
                    
                })
            sink()
            
        } 
        if (input$chosen_algorithm == "UMAP") {
            user_data$data$UMAP1 <- NULL
            user_data$data$UMAP2 <- NULL
            
            # Isolating user selected parameters into new variables
            chosen_markers <- isolate(input$tsne_params_select)
            tmp <- data.frame(user_data$data[, chosen_markers])
            n_neighbors <- isolate(input$n_neighbors)
            min_dist <- isolate(input$min_dist)
            dist_alg <- isolate(input$dist_alg)
            # I don't know why I need to sink() twice but this makes the logging work.
            withProgress(message = "Running UMAP", 
                         detail = "This may take awhile...",
                         value = 50, max = 100, {
                             t <- umapr::umap(d = tmp, 
                                              n_neighbors = n_neighbors,
                                              min_dist = min_dist,
                                              metric = dist_alg,
                                              verbose=TRUE
                             )
                         })
            user_data$data$UMAP1 <- t$UMAP1
            user_data$data$UMAP2 <- t$UMAP2
            #user_data$analysis_complete <- TRUE
            
        } 
    })
    
    # Color Picker ----
    sample_colors <- reactive({
        lapply(unique(user_data$data$Population), function(i) {
            #div(style="display: inline-block;vertical-align:top; width: 200px;",
            colourpicker::colourInput(i, i, "black", palette = "limited")#)
        })
    })
    
    # Populate the color selectors for samples
    output$overlay <- renderUI({sample_colors()})
    outputOptions(output, "overlay", suspendWhenHidden = FALSE)
    
    # Reactive list that maps sample to its respective chosen color
    select_sample_colors <- reactive({
        lapply(unique(user_data$data$Population), function(i) {
            input[[paste(i)]]
        })
    })
    
    output$relative_exp <- renderUI({
        excluded <- c("tSNEX", "tSNEY", "UMAP1", "UMAP2")
        poss_overlays <- colnames(user_data$data)[!colnames(user_data$data)%in%excluded]
        selectInput(inputId = "rel_overlay",
                    label = "Overlay",
                    choices = poss_overlays,
                    selected = "Population")
    })
    outputOptions(output, "relative_exp", suspendWhenHidden = FALSE)
    # Plotting calls ----
    output$plot <- renderPlot({
        #if (is.null(user_data$plot)) return ()
        if (user_data$analysis_complete == "YES") {
            sample_color <- setNames(unlist(select_sample_colors()), unique(user_data$data$Population))
            user_data$plot <- create_plot(data=user_data$data, 
                                          marker=input$rel_overlay, 
                                          method=input$plot_style,
                                          dotsize=input$size,
                                          dotalpha=input$alpha, 
                                          sample_color=sample_color, 
                                          show_legend=input$show_legend,
                                          show_axis_labels=input$show_axis_labels, 
                                          show_title=input$show_title)
            user_data$plot
        } else {
            return ()
        }
        
    })
    # Plot Saving ----
    output$export = downloadHandler(
        filename = function() {paste(input$rel_overlay, ".pdf", input$format, sep="")},
        content = function(file) {
            ggplot2::ggsave(file, plot=user_data$plot, device = "pdf", dpi=300, width=11, height=8.5, units="in")
            
        }
    )
    # Bulk Plot Exporting ----
    output$exportmarkers <- renderUI({
        checkboxGroupInput("echoices", "Markers", user_data$markers, selected=user_data$markers)
    })
    observeEvent(input$genPlots, {
        
        withProgress(message="Generating Plots ...", value=0, min=0, max=length(input$echoices), {
            for (mark in input$echoices) {
                sample_color <- setNames(unlist(select_sample_colors()), unique(user_data$data$Population))
                tmp_plot <- create_plot(data=user_data$data, 
                                        marker=mark, 
                                        method=input$plot_style,
                                        dotsize=input$size,
                                        dotalpha=input$alpha, 
                                        sample_color=sample_color, 
                                        show_legend=input$show_legend,
                                        show_axis_labels=input$show_axis_labels, 
                                        show_title=input$show_title)
                name <- paste0(tmpdir, "/", mark, ".", input$exportFormat)
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

    # Dataframe Downloading ----
    output$saveRDF = downloadHandler(
        filename = function() {paste("DataFrame", ".rds", sep="")},
        content = function(file) {
            saveRDS(user_data$data, file)
        }
    )
    # Event Logger ----
    log <- reactiveFileReader(50, session=session, tmpfile, read.delim, sep="\n")
    
    # Prints tSNE status to text box under the file selection section
    output$consoletext <- renderPrint({
        try(print(log()), silent=TRUE)
        
        # Scrolls the window to last line as it prints
        session$sendCustomMessage(type = "scrollCallback", 1)
    })
    # Result to Plot ----
    output$res_to_plot <- renderUI({
        options_for_plots <- c()
        if ("UMAP1"%in%colnames(user_data$data)) {
            options_for_plots <- c(options_for_plots, "UMAP")
        } 
        if ("tSNEX"%in%colnames(user_data$data)) {
            options_for_plots <- c(options_for_plots, "bh-SNE")
        }
        selectInput(inputId = "plot_style", "Result to Plot", choices = options_for_plots)
    })
    outputOptions(output, "res_to_plot", suspendWhenHidden = FALSE)
    observeEvent(input$plot_analysis, {
        user_data$analysis_complete <- "YES"
    })
    # About section ----
    output$about_section <- renderUI({
        HTML(paste0("CytoDRAV v2.0 is a re-imagination of the original CytoDRAV.<br />", 
                   "Changelog:<br />",
                   "<ul>
                   <li>ACS Upload
                    <ul>
                        <li>Researchers can now upload ACS files directly to CytoDRAV. CytoDRAV will then load the ACS file
                            and show you the gating tree and a selector to pick populations of interest.
                        </li>
                    </ul>
                   <li>UMAP Support
                    <ul>
                        <li>You can now choose between running bh-SNE or UMAP. UMAP support is provided with the 
                            <a href='https://cran.r-project.org/web/packages/uwot/index.html'>uwot package</a>
                        </li>
                   </ul>
                   </ul>"))
    })
    
    session$onSessionEnded(function() {
        stopApp()
    })
}