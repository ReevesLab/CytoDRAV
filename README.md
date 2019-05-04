# CytoDRAV
Dimensionality Reduction and Visualization for Flow Cytometry

## Installation
Note: This package was built on Mac OS X Movaje (10.14.4). I have tested it on Linux Mint 18 as well with success. Windows has not been tested.
1. Install R from https://cloud.r-project.org/
  - CytoDRAV was built with R 3.4.4 but has been shown to work with R 3.6.0 on Linux Mint 18.
2. Download CytoDRAV from [this link](https://github.com/ReevesLab/CytoDRAV/archive/master.zip) to your Desktop. Extract the .zip file by double clicking.
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
    Note: This can take several minutes to complete.
5. If there are no errors CytoDRAV can be launched by running the following command: `Rscript runCytoDRAV.R`
    If there are errors could you please copy and email them to me at kkroll1 (at) bidmc (dot) harvard (dot) edut
6. Alternatively, from the terminal you can type `chmod +x launch` and then you can double click the `launch` file to run CytoDRAV. It should open automatically in your default browser.

A live demo-version of CytoDRAV can be found at https://kroll.bio/CytoDRAV

Note that this demo-version has a file-size limit of 50MB. The desktop version has a limit of 100MB

It is recommended to downsample events before running t-SNE with CytoDRAV. 100K total events takes >30 minutes to compute.

Please see the [PkgDev branch](https://github.com/ReevesLab/CytoDRAV/tree/PkgDev) for current stable package release and installation instructions. 

Please see the [Development branch](https://github.com/ReevesLab/CytoDRAV/tree/Development) for beta version of CytoDRAV. This version has a new feature, plotting a density overlay on the bh-SNE results. This branch also contains a new section on the "About" tab that summarizes the parameters used for the analysis.
