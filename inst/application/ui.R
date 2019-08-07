# About ----
# Application: CytoDRAV
# Version: 2.0
# Changelog: Complete overhaul of CytoDRAV.
# Read more at https://github.com/ReevesLab/CytoDRAV/Reboot


{fluidPage(
    titlePanel("CytoDRAV 2.0"),
    # Sidebar ----
        sidebarLayout(
            sidebarPanel(
                fileInput("wsp_upload", "Select ACS File", multiple=FALSE),
                    div(style="margin-top:-1em",
                    actionButton("load_acs", "Load ACS")
                    ),
                hr(),
                uiOutput(outputId = "plot_style"),
                hr(),
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
            mainPanel(
                tabsetPanel(id = "main",
                            tabPanel("Plot", value = "plotting",
                                     shinyWidgets::dropdownButton(
                                         fluidRow(
                                             column(width=6,
                                                    sliderInput(inputId="size", "Dot Size", min=0.1, max=5, value=1),
                                                    sliderInput(inputId="alpha", "Dot alpha", min=0.1, max=1, value=1),
                                                    checkboxInput("show_legend", "Legend", value=TRUE),
                                                    checkboxInput("show_axis_labels", "Axis Labels", value=TRUE),
                                                    checkboxInput("show_title", "Title", value=TRUE),
                                                    downloadButton("export", "Save plot"),
                                                    downloadButton("saveRDF", "Save data")),
                                             column(width=6, uiOutput("overlay"))),
                                         circle = FALSE, status = "danger",
                                         icon = icon("sliders-h"), width = "300px",

                                         tooltip = shinyWidgets::tooltipOptions(title = "Plot Settings and Download")),
                                     plotOutput("plot", width="100%", height=750)
                            ),
                            tabPanel("Settings",
                                     id = "settings",
                                     tabsetPanel(id = "settings_tabs",
                                                 tabPanel("Gating", value = "gating",
                                                          plotOutput(outputId = "gh_plot")),
                                                 tabPanel("Population", value = "population",
                                                          textOutput(outputId = "label"),
                                                          uiOutput(outputId = "gated_pop_selection"),
                                                          textOutput(output = "num_events")),
                                                 tabPanel("Parameters", value = "parameters",
                                                          fluidRow(
                                                              column(2, uiOutput(outputId = "sel_params")),
                                                              column(10, uiOutput(outputId = "params_alg"))
                                                          )
                                                 )
                                     )
                            )
                )
            )
        )

)}
