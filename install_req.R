install.packages(c("shiny", "promises", "future", 
                   "dplyr", "stringr", "colourpicker", 
                   "Rtsne", "ggplot2"),
                 repos='http://cran.us.r-project.org')
install.packages("https://cran.r-project.org/bin/macosx/el-capitan/contrib/3.6/robustbase_0.93-4.tgz", repos=NULL)
install.packages("https://cran.r-project.org/src/contrib/Archive/mvtnorm/mvtnorm_1.0-8.tar.gz", repos=NULL)
source("https://bioconductor.org/biocLite.R")
BiocInstaller::biocLite("flowCore")