# CytoDRAV
Dimensionality Reduction and Visualization for Flow Cytometry

## Installation

1. Check to see if XCode Command Line tools are installed by running the following command 
    ```
    xcode-select --install
    ```
    You will either see a statement:
    ```
    xcode-select: error: command line tools are already installed, 
    use "Software Update" to install updates
    ```
    or a window will pop-up prompting you to install the command line tools. Complete this installation before proceeding.

2. After installing R from [CRAN](https://cloud.r-project.org/) launch a terminal session (&#x2318; + spacebar -> "terminal")    and type the command `R` and hit return. This will start an interactive R session . Run the following commands to install      devtools and CytoDRAV
   ```
   if(!require(devtools)) install.packages("devtools") # If not already installed
   devtools::install_github("ReevesLab/CytoDRAV", ref="PkgDev", force=T, dependencies=T)
   ```
   This will install CytoDRAV and all required packages.
3. Exit the R session by typing `q()` and then entering `n` and return. 
4. Launch CytoDRAV by executing the `RunCytoDRAV` script. This may prompt you to allow the file in your security settings. You    can either allow in settings or, in the terminal, enter the command `sh RunCytoDRAV`
5. CytoDRAV should now launch in your default browser.
