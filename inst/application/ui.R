# UI generation for CytoDRAV

fluidPage(

  shinyjs::useShinyjs(),

  titlePanel("CytoDRAV - Dimensionality Reduction and Visualization for Flow Cytometry"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose FCS File", multiple=TRUE, accept=c("text/fcs", ".fcs")),
      checkboxInput("transform", "Scale Data", value=FALSE),
      div(style="margin-top:-1em", actionButton("load_fcs_files", "Load FCS")),
      hr(),
      fileInput("filep", "Choose Previous Session", multiple=FALSE, accept=c("text/rda", ".rda")),
      div(style="margin-top:-1em", actionButton("load_previous_file", "Load previous session")),
      hr(),
      fluidRow(
        actionButton("run_tsne", "Run bh-SNE"),
        actionButton("plot_tsne", "Plot bh-SNE"),
        actionButton("run_rpheno", "Run Phenograph")
      ),
      div(id="outDiv", verbatimTextOutput("consoletext")),
      tags$head(tags$style("#outDiv{overflow-y:scroll; max-height: 100px;}")),
      tags$script(
        '
        Shiny.addCustomMessageHandler("scrollCallback",
        function(color) {
        var objDiv = document.getElementById("outDiv");
        objDiv.scrollTop = objDiv.scrollHeight;
        }
        );'
    )
    ),

    # All the div calls are so the buttons would be on the same row.
    # I should look into fluidRow to cut down the clutter here.
    mainPanel(
      tabsetPanel(id="apptabs",

                  tabPanel(title="Main", value = "maintab",
                           shinyWidgets::dropdownButton(
                             fluidRow(
                             column(width=6,
                             sliderInput(inputId="size", "Dot Size", min=0.1, max=5, value=1),
                             sliderInput(inputId="alpha", "Dot alpha", min=0.1, max=1, value=1),
                             checkboxInput("show_legend", "Legend",
                                           value=TRUE),
                             checkboxInput("show_axis_labels", "Axis Labels",
                                           value=TRUE),
                             checkboxInput("show_title", "Title",
                                           value=TRUE),
                             checkboxInput("show_cluster", "Cluster Label",
                                           value = TRUE),
                             downloadButton("export", "Save plot"),
                             downloadButton("saveRDF", "Save data")),
                             column(width=6,
                                    uiOutput("overlay"),
                                    uiOutput("color_selector"))),
                             circle = FALSE, status = "danger",
                             icon = icon("sliders-h"), width = "300px",

                             tooltip = shinyWidgets::tooltipOptions(title = "Plot Settings and Download")),
                          plotOutput("plot", width="100%", height=750)
                          ),
                  tabPanel(title="Histograms", value = "histotab",
                           uiOutput("hist_selector"),
                           plotOutput("hist_plot", width="100%", height=1000)),
                  tabPanel(title="Cluster Info", value="clustinfo",
                           downloadButton("download_cluster", "Download Cluster Info"),
                           verbatimTextOutput("cluster_info")),
                  tabPanel(title="Parameters", value = "paramtab",
                           fluidRow(
                           column(2,
                                  uiOutput("maker_select_tsne"),
                                  uiOutput("ncell_slider")
                                  ),
                           column(2,
                                  h4("bh-SNE Parameters"),
                                  checkboxInput("initpca", "Initial PCA Step", TRUE),
                                  numericInput("perp", "Perplexity", min=0, value=20),
                                  numericInput("ndims", "Output dimensions", min=2, value=2),
                                  numericInput("theta", "Theta", min=0.5, value=0.5),
                                  numericInput("eta", "Learning rate", min=50, value=200),
                                  numericInput("iter", "Iterations", min = 500, max = 5000, value = 1000),
                                  numericInput("n_neighbors", "PhenoGraph # Neighbors", min = 15, value = 200),
                                  numericInput("asinh_cofactor", "Asinh Transformation Cofactor", value = 150),
                                  selectInput("n_threads", "Number of Threads",
                                              choices = seq(1:(unname(future::availableCores())-1)),
                                              selected = 1)
                                  ),
                           column(8,
                                  p("Settings shown are default. Settings explained:"),
                                  p("Initial PCA Step - Should PCA be computed first to initially reduce dimensions to variables that explain the most variance."),
                                  p("Output dimensions - How many dimensions should the output have. Currently only display of 2 dimensions is supported."),
                                  p("Theta - Speed/accuracy trade-off (increase for less accuracy), set to 0.0 for exact tSNE "),
                                  p("Learning rate - How much variation is calculated during each iteration"),
                                  p("Iterations - How many iterations to run before ending the calculations. Max 5000")
                                  )
                           )),

                  tabPanel("Export",
                           fluidRow(
                             h4("Plot Export Settings"),
                                    column(2, uiOutput("exportmarkers")),
                                    column(2, selectInput("exportFormat", "Filetype:", choices = c("pdf", "png", "jpg"),
                                                          selected = "pdf")),
                                    column(4,
                                           div(style="display: inline-block;vertical-align:top; width: 100px;",
                                               actionButton("generate_plots", "Generate")),
                                           div(style="display: inline-block;vertical-align:top; width: 100px;",
                                               downloadButton("download_plots", "Download"))
                                    )
                           )
                          ),

                  tabPanel("About",
                           h4("About CytoDRAV"),
                           p("CytoDRAV was written by Kyle Kroll at the Reeves Lab, Center for Virology and Vaccine Research at
                             Beth Israel Deaconess Medical Center/Harvard Medical School",
                             tags$br(),
                             tags$br(),
                             "CytoDRAV was written because our lab had a need for an easy-to-use tool for the computation and
                             visualization of tSNE."), br(),
                           htmlOutput("analysis_params")
                          )
      )
    )
  )
)
