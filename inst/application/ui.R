fluidPage(

  shinyjs::useShinyjs(),
  titlePanel("CytoDRAV - Dimensionality Reduction and Visualization for Flow Cytometry"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose FCS File", multiple=TRUE, accept=c("text/fcs", ".fcs")),
      checkboxInput("transform", "Transform Data", value=FALSE),
      div(style="margin-top:-1em", actionButton("loadfcs", "Load FCS")),
      hr(),
      fileInput("filep", "Choose Previous Session", multiple=FALSE, accept=c("text/rda", ".rda")),
      div(style="margin-top:-1em", actionButton("runprev", "Load previous session")),
      hr(),
      fluidRow(
        actionButton("runtsne", "Run bh-SNE"),
        actionButton("plottsne", "Plot bh-SNE")
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

    # All the div calls are so the buttons would be on the same row. I should look into fluidRow to cut down the clutter here.
    mainPanel(
      tabsetPanel(
        tabPanel("Main",

      div(style="display: inline-block;vertical-align:top; width: 150px;",
          sliderInput(inputId="size", "Dot Size", min=0.1, max=5, value=1)),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          sliderInput(inputId="alpha", "Dot alpha", min=0.1, max=1, value=1)),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("overlay")),
      downloadButton("export", "Save plot"),

      downloadButton("saveRDF", "Save data"),
      uiOutput("colsel"),
      plotOutput("plot", width="100%", height=750)
      ),
      tabPanel("Parameters",
               fluidRow(
               column(2,
                      uiOutput("markerSelect"),
                      uiOutput("numCellSelect")
                      ),
               column(2,
                      h4("bh-SNE Parameters"),
                      checkboxInput("initpca", "Initial PCA Step", TRUE),
                      numericInput("perp", "Perplexity", min=0, value=20),
                      numericInput("ndims", "Output dimensions", min=2, value=2),
                      numericInput("theta", "Theta", min=0.5, value=0.5),
                      numericInput("eta", "Learning rate", min=50, value=200),
                      numericInput("iter", "Iterations", min = 500, max = 5000, value = 1000)
                      ),
               column(8,
                      p("Settings shown are default. Settings explained:"),
                      p("Initial PCA Step - Should PCA be computed first to initially reduce dimensions to variables that explain the most variance."),
                      p("Output dimensions - How many dimensions should the output have. Currently only display of 2 dimensions is supported."),
                      p("Theta - Speed/accuracy trade-off (increase for less accuracy), set to 0.0 for exact tSNE "),
                      p("Learning rate - How much variation is calculated during each iteration"),
                      p("Iterations - How many iterations to run before ending the calculations. Max 5000"))
               )),

      tabPanel("Export",
               fluidRow(
                 h4("Plot Export Settings"),
                        column(2, uiOutput("exportmarkers")),
                        column(2, selectInput("exportFormat", "Filetype:", choices = c("pdf", "png", "jpg"),
                                              selected = "pdf")),
                        column(4,
                               div(style="display: inline-block;vertical-align:top; width: 100px;",
                                   actionButton("genPlots", "Generate")),
                               div(style="display: inline-block;vertical-align:top; width: 100px;",
                                   downloadButton("dlall", "Download"))
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
                 visualization of tSNE.")
        )
      )
    )
  )
)
