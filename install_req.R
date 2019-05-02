install.packages(c("shiny", "promises", "future", 
                   "dplyr", "stringr", "colourpicker", 
                   "Rtsne", "ggplot2"),
                 repos='http://cran.us.r-project.org')
source("https://bioconductor.org/biocLite.R")
BiocInstaller::biocLite("flowCore")