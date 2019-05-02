# CytoDRAV
Dimensionality Reduction and Visualization for Flow Cytometry

## Installation
Note: This package was built on Mac OS X Movaje (10.14.4). I have tested it on Linux Mint 18 as well with success. Windows has not been tested.
1. Install R from https://cloud.r-project.org/
  - CytoDRAV was built with R 3.4.4 but has been shown to work with R 3.6.0 on Linux Mint 18.
2. Download CytoDRAV from this page to your Desktop. Extract the .zip file by double clicking.
3. Open a terminal (&#x2318; + spacebar -> "terminal") and typing `cd ~/Desktop/CytoDRAV-master` and hitting return.
4. Check to see if XCode Command Line tools are installed by running the following command 
    ```
    xcode-select --install
    ```
    You will either see a statement:
    ```
    xcode-select: error: command line tools are already installed, 
    use "Software Update" to install updates
    ```
    or a window will pop-up prompting you to install the command line tools. Complete this installation before proceeding.
4. Install required R packages by running the following command: `Rscript install_req.R`
5. If there are no errors CytoDRAV can be launched by running the following command: `Rscript runCytoDRAV.R`
6. Alternatively, from the terminal you can type `chmod +x launch` and then you can double click the `launch` file to run CytoDRAV. It should open automatically in your default browser.

A live demo-version of CytoDRAV can be found at https://kroll.bio/CytoDRAV

Note that this demo-version has a file-size limit of 50MB. The desktop version has a limit of 100MB

It is recommended to downsample events before running t-SNE with CytoDRAV. 100K total events takes >30 minutes to compute.
