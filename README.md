# CytoDRAV
Dimensionality Reduction and Visualization for Flow Cytometry

## Web Demo
 * A demo version of this branch can be found [on my website Kroll.Bio](https://kroll.bio/CytoDRAVBeta). This verison is limited to demo files only. This is just for demonstration on how CytoDRAV works so I have restricted it to files that are not large and can be quick to compute.
## New Features
 * Color plot by density of points. 
    - Sometimes it's useful to visually see where the high density regions of a tSNE plot are. This plotting option shows you this in a familiar blue to red scale for increasing density.
 * Moved plot settings and download buttons into a dropdown menu. Helps to give the actual plot more screen real estate.
## Other updates
 * I've been working on cleaning up the code in the following ways:
    - `CytoDRAV::launch_application()` now defaults to launching your default browser. You can pass `launch.browser=F` if you want to start it but not open a browser. A local link in the form `Listening on http://127.0.0.1:XXXX` will display. This is the link to the CytoDRAV page.
    - Renaming variables to more accurately convey what data they hold
    - Converting all variables and functions to snake_case. I am beginning to refresh my python knowledge and that is the accepted style there so I will continue it here.
    - Commenting all code blocks. No more chunks of code without a small descriptor on what that chunk does.
## Planned updates
 * Enable OpenMP support. Rtsne takes an optional parameters `num_threads` Initial testing gives errors, which I believe are due to the fact that I am launching the Rtsne call on a separate thread than the main CytoDRAV application. I will need to test this a lot to see if I can narrow down the error.
 * More options for plot aesthetics. Many people (our lab included) prefer to add figure legends and text using software such as GraphPad Prism. I will be adding options for the following:
    - ~~Hide: legend, title, axis labels~~ Added 2019-05-05
    - Modify: axis tick and tick label font size; plot background color
 * There is a package that adds in shiny widgets which I think might be useful for the figure update. This would allow you to change the settings from a small widget icon instead of switching between tabs
## Installation
If you are on a linux system please skip to step 2.
Tested on R-3.4.4 on Mac OS, R-3.6.0 on Linux Mint 18 and Windows 10. On Windows 10 it appears all features work EXCEPT the bulk export of plots. I will be working on that.
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
2. Install R by downloading the R-3.4.4 package [here](https://cloud.r-project.org/bin/macosx/el-capitan/base/R-3.4.3.pkg). (I have tried CytoDRAV on R-3.4.4 and R-3.5.1 but have not tested on the anything newer than 3.5.1) Then in the terminal you already have open, run the command `R`. This will start an interactive R session. Run the following commands to install devtools and CytoDRAV
   ```
   if(!require(devtools)) install.packages("devtools") # If not already installed
   devtools::install_github("ReevesLab/CytoDRAV", ref="Development", force=T, dependencies=T)
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
