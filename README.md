# CytoDRAV
Dimensionality Reduction and Visualization for Flow Cytometry

For questions/inquries you can email me at kkroll1 (AT) bidmc (DOT) harvard (DOT) edu

# Note to interested
I am currently in the process of doing some major overhauls to CytoDRAV. Some features to come are the ability to choose dimensionality reduction algorithm, pick automated clustering algorithm, and how data is uploaded into CytoDRAV. Please stay tuned in the next few weeks for these changes!

## Web Demo
 * A demo version of this branch can be found [on my website](https://kroll.bio/CytoDRAV). 

 * Please see the [Development branch](https://github.com/ReevesLab/CytoDRAV/tree/Development) for beta version of CytoDRAV. This version has a new feature, plotting a density overlay on the bh-SNE results. This branch also contains a new section on the "About" tab that summarizes the parameters used for the analysis.


## Installation
If you are on a linux system please skip to step 2. For windows you must have gcc installed. I recommend [MinGW](https://osdn.net/projects/mingw/releases/p15522).

Tested on R-3.4.4 on Mac OS, R-3.6.0 on Linux Mint 18 and Windows 10.

1. Launch a terminal session (&#x2318; + spacebar -> "terminal") and check to see if XCode Command Line tools are installed by running the following command 
    ```
    xcode-select --install
    ```
    You will either see a statement:
    ```
    xcode-select: error: command line tools are already installed, 
    use "Software Update" to install updates
    ```
    or a window will pop-up prompting you to install the command line tools. Complete this installation before proceeding.

2. Install R by downloading the R package [here](https://cran.r-project.org/) (CytoDRAV has been tested on R-3.4.4 and R-3.6.0). Then in the terminal you already have open, run the command `R`. This will start an interactive R session. Run the following commands to install devtools and CytoDRAV
   ```
   if(!require(devtools)) install.packages("devtools") # If not already installed
   devtools::install_github("ReevesLab/CytoDRAV", force=T, dependencies=T)
   ```
   This command will progress through the required packages. It may prompt you for input, please select accordingly and make sure that if it asks for you to install Bioconductor, select `yes`
   
   This will install CytoDRAV and all required packages.
3. Exit the R session by typing `q()` and then entering `n` and return. 
4. In the terminal enter the following lines, hitting return after each line:
    ```
    cd ~/Desktop
    printf '#!/bin/bash\nR -e "CytoDRAV::launch_application()"' > RunCytoDRAV
    chmod +x RunCytoDRAV
    ```
5. You can now run CytoDRAV but double clicking the `RunCytoDRAV` script. This will launch a terminal window for progress and it will launch your default browser to the CytoDRAV page. 
    * Alternatively you can open a new terminal and enter the following command to launch CytoDRAV
        ```
        R --slave --no-restore -e "CytoDRAV::launch_application()"
        ```


CytoDRAV works best with 100,000 events and under. You can use more but it exponentially increases time to compute and the time to plot increases drastically. To downsample events you can either pre-downsample in FlowJo, or under the Settings tab there is a slider at the bottom to choose number of events. 10,000 events is fairly quick to compute just to demonstrate the program.
