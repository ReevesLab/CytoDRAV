# About ----
# Project: CytoDRAV
# Author: Kyle Kroll
# Version: 2.0
# This file holds all generic functions that are used in the program
# Offloaded these functions to create a distinction between shiny code and behind the scenes R code

# Outlier correction for plots ----
norm_data <- function(x){
  quantiles <- quantile( x, c(.01, .99 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
# Plotting function ----
create_plot <- function(dataToPlot, marker, dotsize, dotalpha, show_legend,show_axis_labels, show_title) {
    data <- as.data.frame(dataToPlot)
    plot <- ggplot2::ggplot(data) + 
        ggplot2::aes(x=data[, "tSNEX"], y=data[, "tSNEY"]) +
        ggplot2::geom_point(ggplot2::aes(color=data[, marker]),size=dotsize, alpha=dotalpha) +
        ggplot2::labs(x=x, y=y, color="") +
        ggplot2::ggtitle("Population") +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=3), nrow = 1)) +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       legend.text = ggplot2::element_text(size=12, face="bold"),
                       legend.title = ggplot2::element_text(size=16, face="bold"),
                       panel.background = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black"),
                       axis.title = ggplot2::element_text(size=18, face="bold"),
                       plot.title = ggplot2::element_text(size=18, face = "bold"),
                       legend.position = "bottom",
                       legend.direction = "horizontal")
    if (!show_legend) plot <- plot + ggplot2::theme(legend.position="none")
    if (!show_title) plot <- plot + ggplot2::theme(plot.title = ggplot2::element_blank())
    if (!show_axis_labels) plot <- plot + ggplot2::theme(axis.title = ggplot2::element_blank())
    return(plot)
}

# Z-score transform ----
z_transform <- function(x) {
    zVar <- (x - mean(x)) / sd(x)
    zVar
}

# Loadng data from flowWorkspace ----
load_nodes <- function(gating_set, populations) {}
# Loading ACS files ----
extract_acs <- function(file, dir) {}