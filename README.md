# CytoDRAV
Dimensionality Reduction and Visualization for Flow Cytometry

## Installation
If you are on a linux system please skip to step 2.
I have not tested this with Windows but as long as you can make and gcc you can install CytoDRAV. 
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
   This command will progress through the required packages. It may prompt you for input, please select accordingly and make sure that if it asks for you to install Bioconductor, select `yes`
   
   This will install CytoDRAV and all required packages.
3. Exit the R session by typing `q()` and then entering `n` and return. 
4. In the terminal enter the following lines, hitting return after each line:
    ```
    cd ~/Desktop
    printf '#!/bin/bash\nR -e "CytoDRAV::launch_application(launch.browser=T)"' > RunCytoDRAV
    chmod +x RunCytoDRAV
    ```
5. You can now run CytoDRAV but double clicking the `RunCytoDRAV` script. This will launch a terminal window for progress and it will launch your default browser to the CytoDRAV page. 
