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
                               filelist=c("NULL")
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
    # Populate Dynamic Fields ----
    
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
                t <- data.frame(lapply(t[, colnames(t)[!colnames(t)%in%c("Sample", "Time", "Event #")]], z_transform))
                x <- gsub(" ", "_", user_data$filelist[[i]])
                n <- strsplit(x, "\\.")[[1]][1]
                n <- gsub(" ", "_", n)
                t$Population <- paste0(choice)
                t$Filename <- paste0(x)
                t$UniqueID <- paste0(n, "_", gsub(" ", "_", choice))
                df <- rbind(df, t)
            }
        }
        user_data$data <- df
        user_data$col_markers<- colnames(user_data$data)
    })
    
    output$num_events <- renderPrint({
        if (is.null(user_data$data)) return ()
        cat(paste("Number of events in selected populations:", nrow(user_data$data)))
    })
    # tSNE analysis ----
    # Plotting calls ----
    # Plot Savings ----
    # Bulk Plot Exporting ----
    # Dataframe Downloading ----
    # Button Observers ----
    observeEvent(input$load_acs, {
        load_data()
        updateTabsetPanel(session, "main", "Settings")
        updateTabsetPanel(session, "settings_tabs", "gating")
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
}