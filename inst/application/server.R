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
    acs_data <- reactiveValues(raw_data=NULL,
                               data=NULL,
                               filelist=c()
                               )
    # ACS loading ----
    # Populate Dynamic Fields ----
    # Load Selected Nodes ----
    # tSNE analysis ----
    # Plotting calls ----
    # Plot Savings ----
    # Bulk Plot Exporting ----
    # Dataframe Downloading ----
}
